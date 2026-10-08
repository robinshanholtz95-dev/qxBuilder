## Question payload builders ----------------------------------------------
## Each qx_question_*() function returns a plain list -- a Qualtrics
## question payload -- ready to hand to qx_create_question(). None of
## these make any network calls themselves.
##
## Confidence tiers (see each function's docs for detail):
##   VERIFIED     -- exercised against a live Qualtrics account during
##                    development of this package.
##   BEST-EFFORT  -- built from Qualtrics's own documented/commonly
##                    shown schema for the type, but not yet exercised
##                    against a live API call from this package. If one
##                    of these errors, the error response from
##                    Qualtrics is normally specific about which field
##                    is wrong -- iterate from there, the same way the
##                    VERIFIED types were worked out.

#' @keywords internal
qx_auto_tag <- function(prefix = "Q") {
  sprintf("%s_%s_%04x", prefix, format(Sys.time(), "%Y%m%d%H%M%S"), sample.int(65535L, 1L))
}

#' Pick a readable black or white text color for a background color
#'
#' Uses relative (sRGB) luminance, the standard WCAG-style approach, so
#' saturated/dark backgrounds get white text and pale/light backgrounds
#' get black text automatically.
#'
#' @param bg_hex Character. A hex color, e.g. \code{"#2e7d32"}.
#' @return Either \code{"#000000"} or \code{"#ffffff"}.
#' @export
qx_auto_text_color <- function(bg_hex) {
  rgb <- grDevices::col2rgb(bg_hex) / 255
  lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  luminance <- 0.2126 * lin[1] + 0.7152 * lin[2] + 0.0722 * lin[3]
  if (luminance > 0.5) "#000000" else "#ffffff"
}

#' Build a blank Text Entry question
#'
#' Confidence: VERIFIED. A single-line Text Entry question with no
#' question wording -- just a blank text box.
#'
#' @param data_export_tag Character, or \code{NULL} to auto-generate a
#'   timestamped tag.
#'
#' @return A question payload list.
#' @export
qx_question_blank_text <- function(data_export_tag = NULL) {
  qx_question_text_entry(text = "", data_export_tag = data_export_tag)
}

#' Build a Text Entry question
#'
#' Confidence: VERIFIED (for the blank/single-line case exercised
#' during development; the multi-line selector follows the same
#' documented pattern but has not itself been separately tested).
#'
#' @param text Character. The question wording (may be \code{""} for
#'   a blank prompt).
#' @param multi_line Logical. \code{FALSE} (default) for a single-line
#'   response box (\code{Selector = "SL"}), \code{TRUE} for a
#'   multi-line box (\code{Selector = "ML"}).
#' @param data_export_tag Character, or \code{NULL} to auto-generate.
#'
#' @return A question payload list.
#' @export
qx_question_text_entry <- function(text = "", multi_line = FALSE, data_export_tag = NULL) {
  if (is.null(data_export_tag)) data_export_tag <- qx_auto_tag("Q_TE")

  list(
    QuestionText  = text,
    DataExportTag = data_export_tag,
    QuestionType  = "TE",
    Selector      = if (multi_line) "ML" else "SL",
    Configuration = list(QuestionDescriptionOption = "UseText"),
    Validation    = list(Settings = list(ForceResponse = "OFF", Type = "None")),
    Language      = list()
  )
}

#' Build a Highlight (interactive text-highlighting) question
#'
#' Confidence: VERIFIED. Reverse-engineered against a live Qualtrics
#' account (Qualtrics does not publish this question type's schema).
#' \code{HighlightText} is the field that actually drives the
#' interactive passage; if it's left out Qualtrics silently defaults it
#' to the literal string \code{"undefined"}. \code{Choices} and
#' \code{ChoiceOrder} do not need to be supplied -- Qualtrics derives
#' them itself from \code{HighlightText}. \code{WordChoiceIds} must be
#' an array of integers (not strings). \code{AnswerOrder} is NOT a
#' valid field for this question type -- do not add it.
#'
#' @param text Character. The PLAIN passage respondents will highlight
#'   (real newlines, not \code{"<br>"}). This function runs it through
#'   \code{\link{qx_htmlify}} itself to build \code{HighlightText}, and
#'   separately uses the plain text to count words for
#'   \code{WordChoiceIds} -- so always pass plain text here, never an
#'   already HTML-ified string (that would miscount words and treat
#'   markup like \code{"<br>"} as its own bogus "word").
#' @param categories Character vector of highlight category labels.
#'   Every word in \code{text} is interactive and can be tagged with
#'   any one of these. Defaults to a six-point green-to-red sentiment
#'   scale: Liked, Insightful, Inspiring, Confusing, Polarizing,
#'   Disliked.
#' @param category_colors Character vector of hex background colors,
#'   one per category, or a single color to repeat for all of them.
#'   Defaults to a green-to-red scale matching the default
#'   \code{categories}. Must otherwise be the same length as
#'   \code{categories}.
#' @param category_text_colors Optional character vector of hex text
#'   colors, one per category (or a single color to repeat). If
#'   \code{NULL} (the default), a readable black or white is chosen
#'   automatically per category based on the brightness of its
#'   background color.
#' @param data_export_tag Character, or \code{NULL} to auto-generate.
#'
#' @return A question payload list.
#' @export
qx_question_highlight <- function(text,
                                   categories = c("Liked", "Insightful", "Inspiring",
                                                   "Confusing", "Polarizing", "Disliked"),
                                   category_colors = NULL,
                                   category_text_colors = NULL,
                                   data_export_tag = NULL) {
  if (is.null(data_export_tag)) data_export_tag <- qx_auto_tag("Q_HL")

  # NOTE: R's default arguments only apply when a parameter is omitted,
  # not when it's explicitly passed as NULL (e.g. by the book_excerpt
  # generator when the template doesn't specify colors) -- so NULL is
  # handled explicitly here rather than relying on the default above.
  if (is.null(category_colors)) {
    category_colors <- if (length(categories) == 6) {
      c("#2e7d32", "#8bc34a", "#d4e157", "#ffca28", "#ff8a65", "#e53935")
    } else {
      rep("#ffffbf", length(categories))
    }
  } else if (length(category_colors) == 1) {
    category_colors <- rep(category_colors, length(categories))
  }
  stopifnot(length(category_colors) == length(categories))

  if (is.null(category_text_colors)) {
    category_text_colors <- vapply(category_colors, qx_auto_text_color, character(1), USE.NAMES = FALSE)
  } else if (length(category_text_colors) == 1) {
    category_text_colors <- rep(category_text_colors, length(categories))
  }
  stopifnot(length(category_text_colors) == length(categories))

  words <- strsplit(text, "\\s+")[[1]]
  words <- words[nzchar(words)]
  word_choice_ids <- as.integer(seq_along(words))
  highlight_text <- qx_htmlify(text)

  answers <- stats::setNames(
    lapply(seq_along(categories), function(i) {
      list(Display = categories[i], BGColor = category_colors[i], TextColor = category_text_colors[i])
    }),
    as.character(seq_along(categories))
  )

  list(
    QuestionText        = "",
    QuestionDescription = "",
    DataExportTag       = data_export_tag,
    QuestionType        = "HL",
    Selector            = "Text",
    HighlightText       = highlight_text,
    WordChoiceIds       = word_choice_ids,
    Answers             = answers,
    Configuration       = list(QuestionDescriptionOption = "UseText"),
    Validation          = list(Settings = list(ForceResponse = "OFF", Type = "None")),
    Language            = list(),
    ExcludedWords       = list()
  )
}

#' Build a Multiple Choice question
#'
#' Confidence: BEST-EFFORT. Follows Qualtrics's commonly documented MC
#' schema (single-answer vertical radio buttons, or multi-answer
#' checkboxes), but has not been exercised against a live API call from
#' this package. If it errors, Qualtrics's validation error will
#' usually name the specific field to fix.
#'
#' @param text Character. The question wording.
#' @param choices Character vector of answer choice labels.
#' @param multi_select Logical. \code{FALSE} (default) for
#'   single-answer radio buttons (\code{Selector = "SAVR"}),
#'   \code{TRUE} for checkboxes / multi-select (\code{Selector = "MAVR"}).
#' @param data_export_tag Character, or \code{NULL} to auto-generate.
#'
#' @return A question payload list.
#' @export
qx_question_multiple_choice <- function(text, choices, multi_select = FALSE, data_export_tag = NULL) {
  if (is.null(data_export_tag)) data_export_tag <- qx_auto_tag("Q_MC")
  stopifnot(length(choices) > 0)

  choices_list <- stats::setNames(
    lapply(choices, function(c) list(Display = c)),
    as.character(seq_along(choices))
  )
  choice_order <- as.character(seq_along(choices))

  list(
    QuestionText  = text,
    DataExportTag = data_export_tag,
    QuestionType  = "MC",
    Selector      = if (multi_select) "MAVR" else "SAVR",
    SubSelector   = "TX",
    Configuration = list(QuestionDescriptionOption = "UseText"),
    Choices       = choices_list,
    ChoiceOrder   = choice_order,
    Validation    = list(Settings = list(ForceResponse = "OFF", Type = "None")),
    Language      = list()
  )
}

#' Build a Matrix (Likert-style table) question
#'
#' Confidence: BEST-EFFORT. Follows Qualtrics's commonly documented
#' Matrix schema (statements as rows/\code{Choices}, scale points as
#' columns/\code{Answers}), but has not been exercised against a live
#' API call from this package. Unlike the Highlight question type,
#' \code{AnswerOrder} IS expected here (Matrix is a much more
#' conventional, widely documented question type) -- but again, treat
#' this as a starting point to iterate from if Qualtrics's validation
#' disagrees.
#'
#' @param statements Character vector. Row labels.
#' @param scale_points Character vector. Column labels, e.g.
#'   \code{c("Strongly Disagree", ..., "Strongly Agree")}.
#' @param text Character. The question wording shown above the table.
#' @param data_export_tag Character, or \code{NULL} to auto-generate.
#'
#' @return A question payload list.
#' @export
qx_question_matrix <- function(text, statements, scale_points, data_export_tag = NULL) {
  if (is.null(data_export_tag)) data_export_tag <- qx_auto_tag("Q_MAT")
  stopifnot(length(statements) > 0, length(scale_points) > 0)

  choices_list <- stats::setNames(
    lapply(statements, function(s) list(Display = s)),
    as.character(seq_along(statements))
  )
  answers_list <- stats::setNames(
    lapply(scale_points, function(s) list(Display = s)),
    as.character(seq_along(scale_points))
  )

  list(
    QuestionText  = text,
    DataExportTag = data_export_tag,
    QuestionType  = "Matrix",
    Selector      = "Likert",
    SubSelector   = "SingleAnswer",
    Configuration = list(QuestionDescriptionOption = "UseText"),
    Choices       = choices_list,
    ChoiceOrder   = as.character(seq_along(statements)),
    Answers       = answers_list,
    AnswerOrder   = as.character(seq_along(scale_points)),
    Validation    = list(Settings = list(ForceResponse = "OFF", Type = "None")),
    Language      = list()
  )
}

#' Registry mapping template "type" strings to builder functions
#'
#' Used by \code{\link{qx_run_template}} to dispatch a literal
#' question-template entry (e.g. \code{{"type": "highlight", ...}}) to
#' the matching \code{qx_question_*()} builder. Exported so you can
#' inspect or extend it (e.g. \code{qx_builder_registry[["my_type"]] <- my_builder_fn}).
#'
#' @export
qx_builder_registry <- list(
  blank_text      = function(args) do.call(qx_question_blank_text, args),
  text_entry      = function(args) do.call(qx_question_text_entry, args),
  highlight       = function(args) do.call(qx_question_highlight, args),
  multiple_choice = function(args) do.call(qx_question_multiple_choice, args),
  matrix          = function(args) do.call(qx_question_matrix, args)
)
