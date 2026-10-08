## Template-driven survey building ----------------------------------------
## qx_run_template() is the main entry point: reads a JSON template (or
## an already-parsed list), creates/reuses a block, and creates every
## question the template describes -- either literal (one entry -> one
## question) or generated (one entry -> many questions, e.g. a whole
## book excerpt chunked into Highlight questions).

#' Registry mapping template "type" strings to generator functions
#'
#' A generator takes the entry's \code{args} list and returns a list of
#' "specs": \code{list(payload = <question payload>, force_break = TRUE/FALSE)}
#' for each question it wants created. \code{force_break} means "always
#' put a page break immediately before this question" (see
#' \code{\link{qx_insert_page_breaks}}). Exported so you can extend it,
#' e.g. \code{qx_generator_registry[["my_generator"]] <- my_fn}.
#'
#' @export
qx_generator_registry <- list(
  book_excerpt = function(args) {
    if (is.null(args$file)) stop("book_excerpt generator requires args$file", call. = FALSE)

    max_words        <- if (!is.null(args$max_words)) args$max_words else 200
    max_header_words <- if (!is.null(args$max_header_words)) args$max_header_words else 10
    categories <- if (!is.null(args$categories)) unlist(args$categories) else {
      c("Liked", "Insightful", "Inspiring", "Confusing", "Polarizing", "Disliked")
    }
    category_colors      <- if (!is.null(args$category_colors)) unlist(args$category_colors) else NULL
    category_text_colors <- if (!is.null(args$category_text_colors)) unlist(args$category_text_colors) else NULL

    chunks <- qx_chunk_file(args$file, max_words = max_words, max_header_words = max_header_words)

    lapply(chunks, function(ch) {
      payload <- qx_question_highlight(
        text                  = ch$plain_text,
        categories            = categories,
        category_colors       = category_colors,
        category_text_colors  = category_text_colors
      )
      list(payload = payload, force_break = ch$starts_with_header)
    })
  }
)

#' @keywords internal
qx_expand_template_entry <- function(entry) {
  type <- entry$type
  args <- if (is.null(entry$args)) list() else entry$args

  if (isTRUE(entry$generator) || type %in% names(qx_generator_registry)) {
    generator <- qx_generator_registry[[type]]
    if (is.null(generator)) {
      stop("Unknown generator type in template: '", type, "'", call. = FALSE)
    }
    return(generator(args))
  }

  builder <- qx_builder_registry[[type]]
  if (is.null(builder)) {
    stop(
      "Unknown question type in template: '", type, "'. ",
      "Known types: ", paste(names(qx_builder_registry), collapse = ", "),
      call. = FALSE
    )
  }
  payload <- builder(args)
  list(list(payload = payload, force_break = isTRUE(entry$force_page_break_before)))
}

#' Build and populate a Qualtrics survey block from a template
#'
#' Reads a template describing a block and a list of questions to
#' create in it, and does exactly that: optionally creates a new block
#' (or reuses an existing one), creates every question the template
#' describes (literal questions via \code{\link{qx_builder_registry}},
#' or generated questions via \code{\link{qx_generator_registry}} --
#' e.g. a whole book excerpt file chunked into Highlight questions),
#' and finally inserts page breaks per \code{page_breaks$every} plus
#' any generator-forced breaks (e.g. before a detected section header).
#'
#' Call \code{\link{qx_set_credentials}} before this.
#'
#' Template shape (JSON or the equivalent R list):
#' \preformatted{
#' {
#'   "survey_id": "SV_xxx",
#'   "block": { "mode": "new", "description": "My Block" },
#'   "page_breaks": { "every": 4 },
#'   "questions": [
#'     { "type": "blank_text" },
#'     { "type": "highlight", "args": { "text": "...", "categories": ["Like","Dislike"] } },
#'     { "type": "book_excerpt", "generator": true,
#'       "args": { "file": "book.txt", "max_words": 200 } }
#'   ]
#' }
#' }
#'
#' \code{block.mode} is \code{"new"} (with a \code{description}) or
#' \code{"existing"} (with a \code{block_id}). Each \code{questions[]}
#' entry is either a literal question (\code{type} names a builder in
#' \code{\link{qx_builder_registry}}, \code{args} are its arguments) or
#' a generator (\code{"generator": true}, \code{type} names a generator
#' in \code{\link{qx_generator_registry}}) that expands into many
#' questions at once. A literal entry can also set
#' \code{"force_page_break_before": true} to always start a fresh page.
#'
#' @param template Either a path to a JSON template file, or an
#'   already-parsed R list with the same shape.
#' @param survey_id Character, or \code{NULL} to use
#'   \code{template$survey_id}.
#' @param stop_on_question_error Logical. If \code{FALSE} (the
#'   default), a failed question creation is logged and skipped rather
#'   than halting the whole run -- consistent with how the original
#'   book-excerpt script behaved, so one bad chunk doesn't lose all the
#'   others. Page-break insertion at the end is still attempted for
#'   whatever succeeded.
#'
#' @return Invisibly, a list with \code{block_id}, \code{created}
#'   (a data.frame of DataExportTag/QuestionID/status for every
#'   question attempted), and \code{page_break_result} (the result of
#'   \code{\link{qx_update_block_elements}}, or \code{NULL} if there
#'   was nothing to paginate).
#' @export
qx_run_template <- function(template, survey_id = NULL, stop_on_question_error = FALSE) {
  if (is.character(template) && length(template) == 1 && file.exists(template)) {
    template <- jsonlite::fromJSON(template, simplifyVector = FALSE)
  }
  stopifnot(is.list(template))

  if (is.null(survey_id)) survey_id <- template$survey_id
  if (is.null(survey_id)) stop("No survey_id given (pass it explicitly or set template$survey_id).", call. = FALSE)

  block_cfg <- template$block
  if (is.null(block_cfg)) stop("Template must include a 'block' section.", call. = FALSE)

  if (identical(block_cfg$mode, "existing")) {
    block_id <- block_cfg$block_id
    if (is.null(block_id)) stop("block.mode is 'existing' but no block.block_id given.", call. = FALSE)
  } else {
    description <- if (!is.null(block_cfg$description)) block_cfg$description else {
      paste0("qxBuilder Block - ", format(Sys.time(), "%Y-%m-%d %H:%M"))
    }
    message("Creating a new block: \"", description, "\"...")
    block_id <- qx_create_block(survey_id, description)
    message("Created new block: ", block_id)
  }

  entries <- template$questions
  if (is.null(entries)) entries <- list()

  specs <- list()
  for (entry in entries) {
    specs <- c(specs, qx_expand_template_entry(entry))
  }
  message("Template expands to ", length(specs), " question(s).")

  created_rows <- list()
  question_ids <- character(0)
  force_break_flags <- logical(0)

  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    tag <- spec$payload$DataExportTag
    message("Creating question ", i, " of ", length(specs), " (", tag, ")...")

    outcome <- qx_create_question(survey_id, block_id, spec$payload, stop_on_error = stop_on_question_error)

    if (isTRUE(outcome$ok)) {
      qid <- outcome$result$result$QuestionID
      created_rows[[length(created_rows) + 1]] <- data.frame(
        data_export_tag = tag, question_id = qid, status = "created", stringsAsFactors = FALSE
      )
      question_ids <- c(question_ids, qid)
      force_break_flags <- c(force_break_flags, isTRUE(spec$force_break))
    } else {
      message(
        "  Failed (HTTP ", outcome$status, "):\n",
        jsonlite::toJSON(outcome$result, auto_unbox = TRUE, pretty = TRUE)
      )
      created_rows[[length(created_rows) + 1]] <- data.frame(
        data_export_tag = tag, question_id = NA_character_, status = "failed", stringsAsFactors = FALSE
      )
    }
  }

  created_df <- if (length(created_rows) > 0) do.call(rbind, created_rows) else
    data.frame(data_export_tag = character(0), question_id = character(0), status = character(0))

  page_break_result <- NULL
  page_breaks_cfg <- template$page_breaks
  every <- if (!is.null(page_breaks_cfg$every)) page_breaks_cfg$every else 4

  if (length(question_ids) > 0) {
    message("Inserting page breaks (every ", every, " question(s), plus any forced breaks)...")
    block_definition <- qx_get_block(survey_id, block_id)
    new_elements <- qx_insert_page_breaks(
      block_definition$BlockElements,
      question_ids = question_ids,
      force_break_before = force_break_flags,
      every = every
    )
    page_break_result <- qx_update_block_elements(
      survey_id, block_id, new_elements, block_definition = block_definition
    )
    message("Page breaks inserted.")
  }

  invisible(list(
    block_id = block_id,
    created = created_df,
    page_break_result = page_break_result
  ))
}
