## Text chunking for the "book excerpt" Highlight-question pipeline -------
## Pure functions -- no network calls -- so they can be unit tested
## directly. This is the generalized version of the paragraph -> sentence
## -> word-capped-chunk logic worked out interactively for the book
## excerpt scripts.

#' Split raw text into paragraphs, normalizing hard-wrapped line breaks
#'
#' Paragraphs are assumed to be separated by one or more blank lines.
#' Within a paragraph, single newlines are treated as word-wrap
#' artifacts (common in plain text files exported at a fixed column
#' width) and collapsed to a single space -- only blank-line boundaries
#' are preserved as real paragraph breaks.
#'
#' @param text Character. Raw text (already read into R).
#'
#' @return Character vector of paragraphs.
#' @export
qx_split_paragraphs <- function(text) {
  paragraphs <- strsplit(text, "\n\\s*\n+")[[1]]
  paragraphs <- trimws(paragraphs, which = "both")
  paragraphs <- paragraphs[nzchar(paragraphs)]
  paragraphs <- vapply(paragraphs, function(p) gsub("[ \t]*\n[ \t]*", " ", p), character(1), USE.NAMES = FALSE)
  trimws(paragraphs, which = "both")
}

#' Count words in a string
#'
#' @param txt Character scalar.
#' @return Integer word count.
#' @export
qx_count_words <- function(txt) {
  w <- strsplit(txt, "\\s+")[[1]]
  length(w[nzchar(w)])
}

#' Split a paragraph into sentences
#'
#' Heuristic: a sentence boundary is a run of \code{.!?} (optionally
#' followed by citation-style digits/commas, e.g. \code{".1034"} or
#' \code{".1036,1037"}) followed by whitespace and then a capital
#' letter or opening quote/paren. This handles ordinary prose plus
#' inline footnote numbers, but perfect sentence splitting on arbitrary
#' text isn't solvable with a regex alone (e.g. abbreviations like
#' "Dr." can misfire) -- spot check the output on real data.
#'
#' @param txt Character scalar (normally one paragraph).
#'
#' @return Character vector of sentences.
#' @export
qx_split_sentences <- function(txt) {
  marked <- gsub("([.!?]+[0-9,]*)\\s+(?=[A-Z\"'(])", "\\1\u0001", txt, perl = TRUE)
  sentences <- strsplit(marked, "\u0001")[[1]]
  sentences <- trimws(sentences)
  sentences[nzchar(sentences)]
}

#' Heuristically detect a section-header line
#'
#' A paragraph is treated as a section header if it is short (at most
#' \code{max_words} words), has no sentence-ending punctuation, and is
#' effectively ALL CAPS (e.g. \code{"INFLAMMATION"}). This matches a
#' common nonfiction book-excerpt convention but won't catch Title Case
#' or mixed-case headers -- write your own detector and pass it to
#' \code{\link{qx_chunk_text}}'s \code{is_header} argument if your
#' source material uses a different style.
#'
#' @param paragraph_text Character scalar.
#' @param max_words Integer. Maximum word count to still be considered
#'   a header candidate.
#'
#' @return Logical scalar.
#' @export
qx_is_section_header <- function(paragraph_text, max_words = 10) {
  words <- strsplit(paragraph_text, "\\s+")[[1]]
  words <- words[nzchar(words)]
  if (length(words) == 0 || length(words) > max_words) return(FALSE)
  if (grepl("[.!?]\\s*$", paragraph_text)) return(FALSE)
  letters_only <- gsub("[^A-Za-z]", "", paragraph_text)
  if (nchar(letters_only) == 0) return(FALSE)
  letters_only == toupper(letters_only)
}

#' Preserve whitespace/line-break formatting as HTML
#'
#' HighlightText (and other Qualtrics text fields) render as HTML, so
#' plain newlines and repeated spaces get collapsed by the browser.
#' This converts blank lines to \code{"<br><br>"}, single line breaks
#' to \code{"<br>"}, and runs of 2+ spaces to repeated \code{"&nbsp;"}
#' so they render as typed.
#'
#' @param txt Character scalar.
#' @return Character scalar, HTML-safe for whitespace.
#' @export
qx_htmlify <- function(txt) {
  m <- gregexpr(" {2,}", txt)
  regmatches(txt, m) <- lapply(regmatches(txt, m), function(runs) {
    vapply(runs, function(r) paste(rep("&nbsp;", nchar(r)), collapse = ""), character(1))
  })
  txt <- gsub("\n\\s*\n+", "<br><br>", txt)
  txt <- gsub("\n", "<br>", txt)
  txt
}

#' @keywords internal
qx_build_chunk_text <- function(unit_list) {
  paragraphs_out <- character(0)
  buf <- character(0)
  buf_para <- NA
  for (u in unit_list) {
    if (length(buf) == 0 || u$paragraph == buf_para) {
      buf <- c(buf, u$text)
    } else {
      paragraphs_out <- c(paragraphs_out, paste(buf, collapse = " "))
      buf <- c(u$text)
    }
    buf_para <- u$paragraph
  }
  if (length(buf) > 0) paragraphs_out <- c(paragraphs_out, paste(buf, collapse = " "))
  paste(paragraphs_out, collapse = "\n\n")
}

#' Chunk text into word-capped groups, ready to become Highlight questions
#'
#' Splits \code{text} into paragraphs, then sentences, then packs whole
#' sentences into chunks of up to \code{max_words} words -- never
#' cutting a sentence across two chunks. If a single sentence alone
#' exceeds the cap, it's kept intact as its own oversized chunk rather
#' than being cut in half (a note is emitted via \code{message()} when
#' this happens). Any paragraph detected as a section header (see
#' \code{is_header}) always starts a new chunk, so headers never end up
#' stranded at the end of one.
#'
#' @param text Character. The raw text to chunk (already read into R
#'   -- see \code{\link{qx_chunk_file}} to read straight from a file).
#' @param max_words Integer. Maximum words per chunk (default 200).
#' @param max_header_words Integer. Passed to the default header
#'   detector; ignored if you supply your own \code{is_header}.
#' @param is_header A function taking one paragraph string and
#'   returning \code{TRUE}/\code{FALSE}. Defaults to
#'   \code{\link{qx_is_section_header}}.
#'
#' @return A list with one element per chunk:
#'   \describe{
#'     \item{plain_text}{The chunk's raw text (no HTML).}
#'     \item{highlight_text}{The chunk's text run through
#'       \code{\link{qx_htmlify}}, ready to use as \code{HighlightText}.}
#'     \item{starts_with_header}{Logical: does this chunk begin with a
#'       detected section header?}
#'   }
#' @export
qx_chunk_text <- function(text,
                           max_words = 200,
                           max_header_words = 10,
                           is_header = function(p) qx_is_section_header(p, max_header_words)) {
  paragraphs <- qx_split_paragraphs(text)
  if (length(paragraphs) == 0) stop("No paragraphs found in the supplied text.", call. = FALSE)

  units <- list()
  for (pi in seq_along(paragraphs)) {
    header_flag <- is_header(paragraphs[pi])
    sents <- qx_split_sentences(paragraphs[pi])
    for (si in seq_along(sents)) {
      units[[length(units) + 1]] <- list(
        paragraph = pi,
        text      = sents[si],
        words     = qx_count_words(sents[si]),
        is_header = header_flag && si == 1
      )
    }
  }

  chunks <- list()
  chunk_starts_with_header <- logical(0)

  current_units <- list()
  current_word_count <- 0
  current_is_header_start <- FALSE

  flush_chunk <- function() {
    if (length(current_units) > 0) {
      chunks[[length(chunks) + 1]] <<- qx_build_chunk_text(current_units)
      chunk_starts_with_header[length(chunk_starts_with_header) + 1] <<- current_is_header_start
    }
    current_units <<- list()
    current_word_count <<- 0
    current_is_header_start <<- FALSE
  }

  for (u in units) {
    if (u$is_header && length(current_units) > 0) {
      flush_chunk()
    }

    if (u$words > max_words) {
      flush_chunk()
      message(
        "Note: a single sentence has ", u$words, " words, over the ", max_words,
        " word cap -- keeping it intact as its own chunk."
      )
      chunks[[length(chunks) + 1]] <- qx_build_chunk_text(list(u))
      chunk_starts_with_header[length(chunk_starts_with_header) + 1] <- u$is_header
      next
    }

    if (current_word_count + u$words > max_words) {
      flush_chunk()
    }

    if (length(current_units) == 0) {
      current_is_header_start <- u$is_header
    }
    current_units[[length(current_units) + 1]] <- u
    current_word_count <- current_word_count + u$words
  }
  flush_chunk()

  lapply(seq_along(chunks), function(i) {
    list(
      plain_text         = chunks[[i]],
      highlight_text     = qx_htmlify(chunks[[i]]),
      starts_with_header = chunk_starts_with_header[i]
    )
  })
}

#' Chunk a text file into word-capped groups
#'
#' Convenience wrapper around \code{\link{qx_chunk_text}} that reads
#' the file first.
#'
#' @param path Character. Path to a text file.
#' @param ... Passed through to \code{\link{qx_chunk_text}}.
#'
#' @return See \code{\link{qx_chunk_text}}.
#' @export
qx_chunk_file <- function(path, ...) {
  raw_text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  qx_chunk_text(raw_text, ...)
}
