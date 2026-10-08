## Pure helper for inserting Page Break elements into a block's
## BlockElements list. No network calls -- see qx_update_block_elements()
## in api-core.R for actually sending the result back to Qualtrics.

#' Insert Page Break elements into a block's element list
#'
#' Given a block's existing \code{BlockElements} (as returned by
#' \code{\link{qx_get_block}}) and the ordered list of QuestionIDs you
#' just created in it, returns a new elements list with
#' \code{{"Type": "Page Break"}} entries spliced in: one after every
#' \code{every}-th of your questions, and also immediately before any
#' of your questions flagged in \code{force_break_before} (e.g. a
#' question that starts with a detected section header, from
#' \code{\link{qx_chunk_text}}) -- even if that falls off the regular
#' \code{every}-th cadence. Never inserts two page breaks back to back.
#'
#' Confidence: BEST-EFFORT for the request/response cycle as a whole
#' (see \code{\link{qx_update_block_elements}}), but the
#' \code{{"Type": "Page Break"}} element shape itself has been
#' confirmed against a real exported .qsf file.
#'
#' @param elements A list of block elements (each a list with at least
#'   \code{Type}, and \code{QuestionID} for \code{Type == "Question"}).
#' @param question_ids Character vector: the QuestionIDs you created,
#'   in creation order. Only elements whose \code{QuestionID} appears
#'   here are counted for pagination -- pre-existing questions in the
#'   block are left untouched.
#' @param force_break_before Logical vector the same length as
#'   \code{question_ids}: \code{TRUE} means "always put a page break
#'   immediately before this question," regardless of the \code{every}
#'   cadence. Defaults to all \code{FALSE}.
#' @param every Integer. Insert a page break after every this-many of
#'   your questions (default 4). Set to \code{Inf} to disable the
#'   regular cadence and rely only on \code{force_break_before}.
#'
#' @return A new list of block elements, suitable for
#'   \code{\link{qx_update_block_elements}}.
#' @export
qx_insert_page_breaks <- function(elements,
                                   question_ids,
                                   force_break_before = rep(FALSE, length(question_ids)),
                                   every = 4) {
  stopifnot(length(force_break_before) == length(question_ids))

  new_elements <- list()
  created_seen <- 0

  for (idx in seq_along(elements)) {
    el <- elements[[idx]]

    if (!is.null(el$Type) && el$Type == "Question" &&
        !is.null(el$QuestionID) && el$QuestionID %in% question_ids) {
      created_seen <- created_seen + 1

      needs_break <- created_seen > 1 &&
        ( (is.finite(every) && (created_seen - 1) %% every == 0) ||
          isTRUE(force_break_before[created_seen]) )

      prev_is_break <- length(new_elements) > 0 &&
        !is.null(new_elements[[length(new_elements)]]$Type) &&
        new_elements[[length(new_elements)]]$Type == "Page Break"

      if (needs_break && !prev_is_break) {
        new_elements[[length(new_elements) + 1]] <- list(Type = "Page Break")
      }
    }

    new_elements[[length(new_elements) + 1]] <- el
  }

  new_elements
}
