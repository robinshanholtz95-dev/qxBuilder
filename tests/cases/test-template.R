## Tests for R/template.R -- entry expansion logic only (no network calls;
## qx_run_template() itself isn't exercised here since it needs live
## credentials).

## -- Literal entry dispatches to the builder registry
entry_literal <- list(type = "highlight", args = list(text = "hello world", categories = c("Yes", "No")))
specs <- qxBuilder:::qx_expand_template_entry(entry_literal)
expect_equal(length(specs), 1L, "expand_template_entry: literal entry produces exactly one spec")
expect_equal(specs[[1]]$payload$QuestionType, "HL", "expand_template_entry: literal 'highlight' builds an HL payload")
expect_equal(specs[[1]]$force_break, FALSE, "expand_template_entry: force_break defaults to FALSE")

## -- Literal entry with force_page_break_before
entry_forced <- list(type = "blank_text", force_page_break_before = TRUE)
specs2 <- qxBuilder:::qx_expand_template_entry(entry_forced)
expect_equal(specs2[[1]]$force_break, TRUE, "expand_template_entry: force_page_break_before is honored")

## -- Unknown literal type raises an informative error
err <- tryCatch({ qxBuilder:::qx_expand_template_entry(list(type = "not_a_real_type")); NULL }, error = function(e) conditionMessage(e))
expect_true(!is.null(err) && grepl("Unknown question type", err), "expand_template_entry: unknown type raises a clear error")

## -- Generator entry (book_excerpt) expands into multiple specs
tmp <- tempfile(fileext = ".txt")
writeLines(c(
  "HEADER ONE",
  "",
  paste(rep("This is filler sentence text for the excerpt.", 40), collapse = " "),
  "",
  "HEADER TWO",
  "",
  "A short closing paragraph."
), tmp)

entry_gen <- list(type = "book_excerpt", generator = TRUE, args = list(file = tmp, max_words = 200))
specs3 <- qxBuilder:::qx_expand_template_entry(entry_gen)
expect_true(length(specs3) >= 2, "expand_template_entry: book_excerpt generator produces multiple specs")
expect_true(all(vapply(specs3, function(s) s$payload$QuestionType == "HL", logical(1))),
            "expand_template_entry: every generated spec is a Highlight question")
expect_true(any(vapply(specs3, function(s) s$force_break, logical(1))),
            "expand_template_entry: at least one generated spec forces a page break (the header chunks)")

unlink(tmp)

## -- Generator registry contains book_excerpt
expect_true("book_excerpt" %in% names(qx_generator_registry), "generator_registry: book_excerpt is registered")
