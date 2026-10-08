## Tests for R/question-builders.R -- structural checks only (no network).
## These check the payload SHAPE matches what was verified/documented,
## not that Qualtrics will accept it (that requires a live call).

## -- Blank text entry (VERIFIED type)
p1 <- qx_question_blank_text()
expect_equal(p1$QuestionType, "TE", "blank_text: QuestionType is TE")
expect_equal(p1$Selector, "SL", "blank_text: Selector is SL")
expect_equal(p1$QuestionText, "", "blank_text: QuestionText is blank")
expect_true(!is.null(p1$DataExportTag) && nchar(p1$DataExportTag) > 0, "blank_text: auto-generates a DataExportTag")

## -- Text entry, multi-line
p2 <- qx_question_text_entry("How do you feel?", multi_line = TRUE, data_export_tag = "Q_TEST")
expect_equal(p2$Selector, "ML", "text_entry: multi_line uses Selector ML")
expect_equal(p2$DataExportTag, "Q_TEST", "text_entry: respects an explicit DataExportTag")

## -- Highlight (VERIFIED type): the specific field-level gotchas we
##    hit against the live API
passage <- "the quick brown fox jumps over the lazy dog"
p3 <- qx_question_highlight(passage, categories = c("Like", "Dislike"))
expect_equal(p3$QuestionType, "HL", "highlight: QuestionType is HL")
expect_equal(p3$Selector, "Text", "highlight: Selector is Text")
expect_equal(p3$QuestionText, "", "highlight: QuestionText left blank (not duplicated)")
expect_equal(p3$QuestionDescription, "", "highlight: QuestionDescription left blank (not duplicated)")
expect_true(is.null(p3$AnswerOrder), "highlight: AnswerOrder is NOT set (Qualtrics rejects it for this type)")
expect_true(is.integer(p3$WordChoiceIds), "highlight: WordChoiceIds is an integer vector, not character")
expect_equal(length(p3$WordChoiceIds), 9L, "highlight: WordChoiceIds has one entry per word (9 in the pangram)")
expect_equal(p3$WordChoiceIds, 1:9, "highlight: WordChoiceIds is 1..N")
expect_true(grepl("^the quick brown fox", p3$HighlightText), "highlight: HighlightText carries the passage")
expect_equal(names(p3$Answers), c("1", "2"), "highlight: Answers keyed 1, 2 for the two categories")
expect_equal(p3$Answers[["1"]]$Display, "Like", "highlight: first category Display is correct")

## -- Highlight: HighlightText is HTML-ified, but WordChoiceIds count is
##    still based on the PLAIN text (the bug this was specifically
##    designed to avoid)
multi_para <- "First paragraph here.\n\nSecond paragraph here too."
p4 <- qx_question_highlight(multi_para)
expect_true(grepl("<br><br>", p4$HighlightText, fixed = TRUE), "highlight: HighlightText contains <br><br> for the paragraph break")
expect_equal(length(p4$WordChoiceIds), qx_count_words(multi_para), "highlight: WordChoiceIds count matches plain-text word count, unaffected by HTML-ification")

## -- Highlight: default categories are the six-point sentiment scale
##    (not the old Like/Dislike), with matching colors and auto-contrast text
p4b <- qx_question_highlight(passage)
expect_equal(
  vapply(p4b$Answers, function(a) a$Display, character(1)),
  c("1" = "Liked", "2" = "Insightful", "3" = "Inspiring", "4" = "Confusing", "5" = "Polarizing", "6" = "Disliked"),
  "highlight: default categories are the six-point sentiment scale"
)
expect_equal(p4b$Answers[["1"]]$BGColor, "#2e7d32", "highlight: default 'Liked' color is the dark green")
expect_equal(p4b$Answers[["6"]]$BGColor, "#e53935", "highlight: default 'Disliked' color is the red")
# Explicitly passing category_colors = NULL (as the book_excerpt generator
# does when a template doesn't specify colors) must still fall back
# sensibly, not error -- this was a real bug caught while wiring it up.
p4c <- qx_question_highlight(passage, category_colors = NULL)
expect_equal(p4c$Answers[["1"]]$BGColor, "#2e7d32", "highlight: explicit category_colors=NULL still falls back to the default palette")

## -- Auto text-color contrast picker
expect_equal(qx_auto_text_color("#2e7d32"), "#ffffff", "auto_text_color: dark green background gets white text")
expect_equal(qx_auto_text_color("#ffffff"), "#000000", "auto_text_color: white background gets black text")
expect_equal(qx_auto_text_color("#000000"), "#ffffff", "auto_text_color: black background gets white text")

## -- Multiple choice (BEST-EFFORT type)
p5 <- qx_question_multiple_choice("Pick one", choices = c("A", "B", "C"))
expect_equal(p5$QuestionType, "MC", "multiple_choice: QuestionType is MC")
expect_equal(p5$Selector, "SAVR", "multiple_choice: single-select uses SAVR")
expect_equal(length(p5$Choices), 3L, "multiple_choice: three choices built")
expect_equal(p5$ChoiceOrder, c("1", "2", "3"), "multiple_choice: ChoiceOrder matches Choices keys")

p6 <- qx_question_multiple_choice("Pick any", choices = c("A", "B"), multi_select = TRUE)
expect_equal(p6$Selector, "MAVR", "multiple_choice: multi-select uses MAVR")

## -- Matrix (BEST-EFFORT type): unlike Highlight, AnswerOrder IS included
p7 <- qx_question_matrix("Rate these", statements = c("Row 1", "Row 2"), scale_points = c("Disagree", "Agree"))
expect_equal(p7$QuestionType, "Matrix", "matrix: QuestionType is Matrix")
expect_true(!is.null(p7$AnswerOrder), "matrix: AnswerOrder IS set (this type is expected to want it)")
expect_equal(length(p7$Choices), 2L, "matrix: two statement rows built")
expect_equal(length(p7$Answers), 2L, "matrix: two scale-point columns built")

## -- Builder registry dispatch (used by the template runner)
expect_true(all(c("blank_text", "text_entry", "highlight", "multiple_choice", "matrix") %in% names(qx_builder_registry)),
            "builder_registry: all five builders are registered")
dispatched <- qx_builder_registry[["highlight"]](list(text = "hi there", categories = c("Yes", "No")))
expect_equal(dispatched$QuestionType, "HL", "builder_registry: dispatch to 'highlight' builds an HL payload")
