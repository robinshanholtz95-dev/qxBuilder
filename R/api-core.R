## Low-level Qualtrics Survey Definitions API calls ----------------------
## Everything here just wraps httr + jsonlite around the endpoints this
## package uses. None of this is Qualtrics-question-type-specific; see
## question-builders.R for that.

#' @keywords internal
qx_url <- function(..., query = NULL) {
  creds <- qx_credentials()
  path <- paste(c(...), collapse = "/")
  url <- sprintf("https://%s/API/v3/%s", creds$base_url, path)
  if (!is.null(query) && length(query) > 0) {
    qs <- paste(
      vapply(names(query), function(n) paste0(n, "=", utils::URLencode(as.character(query[[n]]), reserved = TRUE)), character(1)),
      collapse = "&"
    )
    url <- paste0(url, "?", qs)
  }
  url
}

#' Low-level Qualtrics API request
#'
#' Sends one request to the Qualtrics Survey Definitions API and
#' returns a consistent result shape regardless of success or failure.
#' Every higher-level \code{qx_*} function in this package is built on
#' top of this one call.
#'
#' @param method One of \code{"GET"}, \code{"POST"}, \code{"PUT"}.
#' @param path Character vector of path segments joined with \code{/},
#'   e.g. \code{c("survey-definitions", survey_id, "questions")}.
#'   Do not include a leading \code{API/v3/} -- that's added for you.
#' @param query Named list of query string parameters (e.g.
#'   \code{list(blockId = "BL_xxx")}), or \code{NULL}.
#' @param body A list to be sent as the JSON request body (for POST/PUT),
#'   or \code{NULL} for GET.
#' @param stop_on_error Logical. If \code{TRUE} (the default), a
#'   non-2xx response raises an R error (via \code{stop()}) whose
#'   message includes the full pretty-printed error JSON from
#'   Qualtrics, so it's easy to read and act on. If \code{FALSE}, the
#'   failure is returned like any other result instead of raising --
#'   useful when looping over many calls and you don't want one
#'   failure to halt the whole loop.
#'
#' @return A list with elements \code{status} (HTTP status code),
#'   \code{ok} (logical), and \code{result} (the parsed JSON body,
#'   i.e. the full \code{list(meta = ..., result = ...)} Qualtrics
#'   returns).
#' @export
qx_api_request <- function(method, path, query = NULL, body = NULL, stop_on_error = TRUE) {
  creds <- qx_credentials()
  url <- qx_url(path, query = query)

  headers <- httr::add_headers(
    "X-API-TOKEN"  = creds$api_key,
    "Content-Type" = "application/json",
    "Accept"       = "application/json"
  )

  resp <- switch(
    method,
    GET  = httr::GET(url, headers),
    POST = httr::POST(url, headers, body = jsonlite::toJSON(body, auto_unbox = TRUE), encode = "raw"),
    PUT  = httr::PUT(url, headers, body = jsonlite::toJSON(body, auto_unbox = TRUE), encode = "raw"),
    stop("Unsupported method: ", method, call. = FALSE)
  )

  status <- httr::status_code(resp)
  parsed <- tryCatch(
    httr::content(resp, as = "parsed", type = "application/json"),
    error = function(e) list(meta = list(httpStatus = paste(status, "- Unparseable response")))
  )

  ok <- !is.null(parsed$meta$httpStatus) && startsWith(parsed$meta$httpStatus, "200")

  out <- list(status = status, ok = ok, result = parsed)

  if (!ok && stop_on_error) {
    stop(
      "Qualtrics API request failed (HTTP ", status, "). Full response:\n",
      jsonlite::toJSON(parsed, auto_unbox = TRUE, pretty = TRUE),
      call. = FALSE
    )
  }

  out
}

#' Create a question in a Qualtrics survey block
#'
#' Confidence: the request shape (POST to
#' \code{.../questions?blockId=...}) is verified against a live
#' Qualtrics account for the Text Entry and Highlight question types
#' built by this package's \code{qx_question_*()} functions. Any
#' \code{payload} you build yourself will use the same endpoint, but
#' its validity depends on the payload matching Qualtrics's schema for
#' that question type.
#'
#' @param survey_id Character, e.g. \code{"SV_xxx"}.
#' @param block_id Character, e.g. \code{"BL_xxx"}.
#' @param payload A list representing the question definition -- see
#'   the \code{qx_question_*()} builder functions.
#' @param stop_on_error See \code{\link{qx_api_request}}.
#'
#' @return The result of \code{\link{qx_api_request}}. On success,
#'   \code{result$result$QuestionID} holds the new question's ID.
#' @export
qx_create_question <- function(survey_id, block_id, payload, stop_on_error = TRUE) {
  qx_api_request(
    "POST",
    path = c("survey-definitions", survey_id, "questions"),
    query = list(blockId = block_id),
    body = payload,
    stop_on_error = stop_on_error
  )
}

#' Create a new, empty block in a Qualtrics survey
#'
#' Confidence: best-effort. The request body shape below (minimal
#' block with \code{Type}, \code{Description}, \code{Options}, and no
#' \code{ID}/\code{BlockElements}) matches the pattern documented in
#' Qualtrics's developer community as working for creating an empty
#' block, but the exact response field holding the new block's ID
#' (assumed to be \code{result$BlockID}) has not been confirmed
#' against a live call from this package. If creation appears to
#' succeed but \code{qx_create_block()} can't find the ID, it will
#' stop with the full response printed so the field name can be
#' corrected.
#'
#' @param survey_id Character, e.g. \code{"SV_xxx"}.
#' @param description Character. Block name as shown in the editor.
#' @param block_locking,randomize_questions,block_visibility Block
#'   options, passed through as-is; see Qualtrics's Block options for
#'   valid values.
#'
#' @return Character: the new block's ID.
#' @export
qx_create_block <- function(survey_id,
                             description,
                             block_locking = "false",
                             randomize_questions = "false",
                             block_visibility = "Collapsed") {
  payload <- list(
    Type        = "Standard",
    Description = description,
    Options     = list(
      BlockLocking       = block_locking,
      RandomizeQuestions = randomize_questions,
      BlockVisibility    = block_visibility
    )
  )

  out <- qx_api_request(
    "POST",
    path = c("survey-definitions", survey_id, "blocks"),
    body = payload,
    stop_on_error = TRUE
  )

  block_id <- out$result$result$BlockID
  if (is.null(block_id)) {
    stop(
      "Block was created, but 'BlockID' wasn't found where expected in the response.\n",
      "Full response below -- please report this so the field name can be corrected:\n",
      jsonlite::toJSON(out$result, auto_unbox = TRUE, pretty = TRUE),
      call. = FALSE
    )
  }

  block_id
}

#' Fetch a block's current definition
#'
#' @param survey_id Character, e.g. \code{"SV_xxx"}.
#' @param block_id Character, e.g. \code{"BL_xxx"}.
#'
#' @return The block definition list (Type, Description, ID,
#'   BlockElements, Options).
#' @export
qx_get_block <- function(survey_id, block_id) {
  out <- qx_api_request(
    "GET",
    path = c("survey-definitions", survey_id, "blocks", block_id),
    stop_on_error = TRUE
  )
  out$result$result
}

#' Replace a block's BlockElements (questions, page breaks, etc.)
#'
#' Confidence: best-effort. Follows the standard Qualtrics
#' get-modify-put pattern; the \code{"Page Break"} element shape
#' (\code{list(Type = "Page Break")}) has been confirmed against a
#' real exported .qsf file, but the full Update Block request/response
#' cycle has not been exercised against a live API call from this
#' package. If it errors, the response is included in the stop
#' message so the request shape can be adjusted.
#'
#' @param survey_id Character, e.g. \code{"SV_xxx"}.
#' @param block_id Character, e.g. \code{"BL_xxx"}.
#' @param block_elements A list of block elements, e.g.
#'   \code{list(list(Type = "Question", QuestionID = "QID1"), list(Type = "Page Break"), ...)}.
#' @param block_definition Optional: a block definition already
#'   fetched with \code{\link{qx_get_block}}, to avoid an extra GET.
#'   If \code{NULL} (the default), the block is fetched for you.
#'
#' @return The result of \code{\link{qx_api_request}}.
#' @export
qx_update_block_elements <- function(survey_id, block_id, block_elements, block_definition = NULL) {
  if (is.null(block_definition)) {
    block_definition <- qx_get_block(survey_id, block_id)
  }
  block_definition$BlockElements <- block_elements

  qx_api_request(
    "PUT",
    path = c("survey-definitions", survey_id, "blocks", block_id),
    body = block_definition,
    stop_on_error = TRUE
  )
}

#' Fetch a survey's full question definitions
#'
#' Handy for inspecting exactly what Qualtrics stored for a question
#' you just created (e.g. to debug an undocumented field), the same
#' way earlier ad-hoc scripts in this project did.
#'
#' @param survey_id Character, e.g. \code{"SV_xxx"}.
#'
#' @return A named list of question definitions, keyed by QuestionID.
#' @export
qx_get_survey_questions <- function(survey_id) {
  out <- qx_api_request(
    "GET",
    path = c("survey-definitions", survey_id),
    stop_on_error = TRUE
  )
  out$result$result$Questions
}
