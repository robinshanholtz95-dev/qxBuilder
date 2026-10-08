## End-to-end test of qx_run_template()'s ORCHESTRATION logic, with the
## network-touching functions mocked out (no live Qualtrics account is
## reachable from this test environment). This exercises the full path:
## block creation -> entry expansion (literal + generator) -> question
## creation loop -> page break insertion -- checking that the pieces
## are wired together correctly, even though none of the mocked
## functions' request/response shapes are themselves re-verified here
## (that's what test-question-builders.R and a live account are for).

mock_state <- new.env()
mock_state$created_blocks <- character(0)
mock_state$created_questions <- list()
mock_state$page_break_calls <- list()

mock_create_block <- function(survey_id, description, ...) {
  id <- paste0("BL_mock", length(mock_state$created_blocks) + 1)
  mock_state$created_blocks <- c(mock_state$created_blocks, id)
  id
}

mock_create_question <- function(survey_id, block_id, payload, stop_on_error = TRUE) {
  qid <- paste0("QID_mock", length(mock_state$created_questions) + 1)
  mock_state$created_questions[[length(mock_state$created_questions) + 1]] <- list(
    block_id = block_id, payload = payload, qid = qid
  )
  list(status = 200, ok = TRUE, result = list(meta = list(httpStatus = "200 - OK"), result = list(QuestionID = qid)))
}

mock_get_block <- function(survey_id, block_id) {
  qids <- vapply(mock_state$created_questions, function(x) x$qid, character(1))
  list(
    Type = "Standard", Description = "mock", ID = block_id,
    BlockElements = lapply(qids, function(q) list(Type = "Question", QuestionID = q)),
    Options = list(BlockLocking = "false", RandomizeQuestions = "false", BlockVisibility = "Collapsed")
  )
}

mock_update_block_elements <- function(survey_id, block_id, block_elements, block_definition = NULL) {
  mock_state$page_break_calls[[length(mock_state$page_break_calls) + 1]] <- block_elements
  list(status = 200, ok = TRUE, result = list(meta = list(httpStatus = "200 - OK")))
}

ns <- asNamespace("qxBuilder")
originals <- list(
  qx_create_block          = get("qx_create_block", envir = ns),
  qx_create_question       = get("qx_create_question", envir = ns),
  qx_get_block             = get("qx_get_block", envir = ns),
  qx_update_block_elements = get("qx_update_block_elements", envir = ns)
)

unlockBinding("qx_create_block", ns);          assign("qx_create_block", mock_create_block, envir = ns)
unlockBinding("qx_create_question", ns);       assign("qx_create_question", mock_create_question, envir = ns)
unlockBinding("qx_get_block", ns);             assign("qx_get_block", mock_get_block, envir = ns)
unlockBinding("qx_update_block_elements", ns); assign("qx_update_block_elements", mock_update_block_elements, envir = ns)

restore_mocks <- function() {
  for (nm in names(originals)) {
    assign(nm, originals[[nm]], envir = ns)
    lockBinding(nm, ns)
  }
}

# NOTE: on.exit() only defers to the exit of an enclosing FUNCTION call.
# At the top level of a source()'d script there is no such frame, so
# on.exit() here would fire immediately rather than at "script exit" --
# silently undoing the mocks before qx_run_template() is even called.
# Use tryCatch/finally instead, which works regardless of call context.
tryCatch({
  tmp <- tempfile(fileext = ".txt")
  writeLines(c(
    "HEADER ONE",
    "",
    paste(rep("Filler sentence for the excerpt body text here today.", 30), collapse = " "),
    "",
    "A short closing paragraph for this excerpt."
  ), tmp)

  template <- list(
    survey_id = "SV_test",
    block = list(mode = "new", description = "Test Block"),
    page_breaks = list(every = 4),
    questions = list(
      list(type = "blank_text"),
      list(type = "highlight", args = list(text = "hello there", categories = c("Yes", "No"))),
      list(type = "book_excerpt", generator = TRUE, args = list(file = tmp, max_words = 200))
    )
  )

  out <- qx_run_template(template)

  expect_true(out$block_id %in% mock_state$created_blocks, "run_template: uses the block ID from qx_create_block")
  expect_true(nrow(out$created) >= 3, "run_template: created at least the blank_text + highlight + >=1 generated question")
  expect_true(all(out$created$status == "created"), "run_template: every mocked creation reports status 'created'")
  expect_true(length(mock_state$page_break_calls) == 1, "run_template: page break insertion was called exactly once")

  last_elements <- mock_state$page_break_calls[[length(mock_state$page_break_calls)]]
  expect_true(any(vapply(last_elements, function(e) identical(e$Type, "Page Break"), logical(1))),
              "run_template: the final BlockElements sent back include at least one Page Break")

  unlink(tmp)
}, finally = restore_mocks())
