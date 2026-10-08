## Tests for R/chunking.R

## -- qx_split_paragraphs: blank-line boundaries kept, hard-wraps collapsed
raw <- "Line one\nLine two continues\n\nSecond paragraph\nstill going"
paras <- qx_split_paragraphs(raw)
expect_equal(length(paras), 2L, "split_paragraphs: two paragraphs")
expect_equal(paras[1], "Line one Line two continues", "split_paragraphs: hard-wrap collapsed to space")
expect_equal(paras[2], "Second paragraph still going", "split_paragraphs: second paragraph collapsed")

## -- qx_count_words
expect_equal(qx_count_words("the quick brown fox"), 4L, "count_words: basic")
expect_equal(qx_count_words("  extra   spaces  "), 2L, "count_words: ignores extra whitespace")

## -- qx_split_sentences: handles inline citation numbers like ".1034"
txt <- "We've known since 2014 that X causes Y.1034 But there weren't human studies until 2019."
sents <- qx_split_sentences(txt)
expect_equal(length(sents), 2L, "split_sentences: citation number doesn't block the split")
expect_true(grepl("Y\\.1034$", sents[1]), "split_sentences: citation number stays attached to first sentence")
expect_true(grepl("^But", sents[2]), "split_sentences: second sentence starts correctly")

## -- qx_is_section_header
expect_true(qx_is_section_header("INFLAMMATION"), "is_section_header: all-caps single word")
expect_true(!qx_is_section_header("This is a normal sentence."), "is_section_header: rejects normal sentence")
expect_true(!qx_is_section_header("THIS ALL CAPS LINE HAS WAY TOO MANY WORDS TO BE A HEADER REALLY"),
            "is_section_header: rejects overly long all-caps line")
expect_true(!qx_is_section_header("Title Case Heading"), "is_section_header: rejects title case (by design)")

## -- qx_htmlify: preserves line breaks and multi-spaces
html <- qx_htmlify("Para one.\n\nPara two  with  double spaces.")
expect_true(grepl("<br><br>", html, fixed = TRUE), "htmlify: blank line becomes <br><br>")
expect_true(grepl("&nbsp;", html, fixed = TRUE), "htmlify: double space becomes &nbsp;")

## -- qx_chunk_text: respects word cap, never splits a sentence
make_sentence <- function(i) paste0("Sentence ", i, " ", paste(rep("word", 48), collapse = " "), ".")
body <- paste(vapply(1:5, make_sentence, character(1)), collapse = " ")
chunks <- qx_chunk_text(body, max_words = 120)
expect_true(length(chunks) >= 2, "chunk_text: produces multiple chunks for long input")
for (ch in chunks) {
  wc <- qx_count_words(ch$plain_text)
  expect_true(wc <= 120, paste0("chunk_text: chunk word count (", wc, ") within cap"))
}
# Reconstruct: every sentence-ending period count should match total in original
orig_periods <- lengths(regmatches(body, gregexpr("\\.", body)))
chunked_periods <- sum(vapply(chunks, function(ch) lengths(regmatches(ch$plain_text, gregexpr("\\.", ch$plain_text))), integer(1)))
expect_equal(chunked_periods, orig_periods, "chunk_text: no sentence lost or duplicated across chunks")

## -- qx_chunk_text: a single oversized sentence becomes its own chunk (with a message)
huge_sentence <- paste(rep("word", 250), collapse = " ")
msgs <- character(0)
chunks2 <- withCallingHandlers(
  qx_chunk_text(paste0(huge_sentence, "."), max_words = 200),
  message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }
)
expect_equal(length(chunks2), 1L, "chunk_text: oversized single sentence stays as one chunk")
expect_true(any(grepl("over the", msgs)), "chunk_text: emits a note about the oversized sentence")

## -- qx_chunk_text: a section header always starts a new chunk
header_body <- paste0(
  "INFLAMMATION\n\n",
  paste(rep("This is body text for the header section.", 30), collapse = " "), "\n\n",
  "NEXT SECTION\n\n",
  "Short trailing paragraph here."
)
chunks3 <- qx_chunk_text(header_body, max_words = 200)
header_chunk_indices <- which(vapply(chunks3, function(c) c$starts_with_header, logical(1)))
expect_true(length(header_chunk_indices) >= 2, "chunk_text: detects both section headers")
for (i in header_chunk_indices) {
  first_word <- strsplit(chunks3[[i]]$plain_text, "\\s+")[[1]][1]
  expect_true(first_word %in% c("INFLAMMATION", "NEXT"), "chunk_text: header chunk actually begins with the header text")
}
