# qxBuilder

A thin R wrapper around the Qualtrics Survey Definitions API for programmatically
creating survey blocks and questions, and for driving that creation from a JSON
template file. This grew out of a series of hand-written scripts for adding
blank Text Entry and interactive "Highlight" questions to a Qualtrics survey
from a book-excerpt text file; this package generalizes that work into
reusable, testable building blocks.

## Installation

```r
install.packages("qxBuilder_0.1.0.tar.gz", repos = NULL, type = "source")
```

Or, for local development, from the unpacked source directory:

```r
# install.packages("pkgload")
pkgload::load_all("path/to/qxBuilder")
```

## Confidence tiers

Qualtrics doesn't publish the JSON schema for every question type, and this
package's functions are built with two different confidence levels, called
out in each function's documentation:

- **VERIFIED** -- exercised against a live Qualtrics account while building
  this package: `qx_question_blank_text()` / `qx_question_text_entry()`
  (Text Entry), `qx_question_highlight()` (the interactive "Highlight"
  question type, whose schema isn't documented anywhere publicly and had to
  be reverse-engineered from the API's own validation errors), and the
  `{"Type": "Page Break"}` block element (confirmed against a real exported
  `.qsf` file).
- **BEST-EFFORT** -- built from Qualtrics's own commonly-documented schema
  for the type, but not yet exercised against a live API call from this
  package: `qx_question_multiple_choice()`, `qx_question_matrix()`, and the
  `qx_create_block()` / `qx_update_block_elements()` request/response cycle
  as a whole.

If a best-effort call fails, Qualtrics's validation errors are normally
specific about which field is wrong -- that's exactly how the verified types
were worked out, by iterating against real error responses one field at a
time. Please share back the error and the payload if you hit one; it's a
quick fix.

## Quick start

```r
library(qxBuilder)

qx_set_credentials("YOUR_API_KEY", "yul1.qualtrics.com")

# Low-level: build a payload, then create it directly. categories/colors
# default to a six-point green-to-red sentiment scale (Liked, Insightful,
# Inspiring, Confusing, Polarizing, Disliked) -- pass your own to override.
payload <- qx_question_highlight(
  text = "the quick brown fox jumps over the lazy dog"
)
qx_create_question(survey_id = "SV_xxx", block_id = "BL_xxx", payload)

# Or create a whole new block:
block_id <- qx_create_block("SV_xxx", "My New Block")
```

## Template-driven creation

The more powerful path is `qx_run_template()`, which reads a JSON (or R list)
template describing a block and a list of questions -- literal ones, or
*generators* that expand into many questions at once (e.g. a whole book
excerpt file, chunked into Highlight questions) -- and creates all of it in
one call, including page breaks.

```r
qx_set_credentials("YOUR_API_KEY", "yul1.qualtrics.com")
result <- qx_run_template("inst/templates/example_template.json")
result$created        # data.frame of every question attempted + its status
result$block_id        # the block everything was created in
```

Template shape:

```json
{
  "survey_id": "SV_xxx",
  "block": { "mode": "new", "description": "My Block" },
  "page_breaks": { "every": 4 },
  "questions": [
    { "type": "blank_text" },
    {
      "type": "highlight",
      "args": { "text": "..." }
    },
    {
      "type": "book_excerpt",
      "generator": true,
      "args": {
        "file": "book_excerpt.txt",
        "max_words": 200,
        "max_header_words": 10
      }
    }
  ]
}
```

- `block.mode` is `"new"` (with a `description`) or `"existing"` (with a
  `block_id`).
- Each `questions[]` entry is either a **literal** question -- `type` names a
  builder in `qx_builder_registry` (`blank_text`, `text_entry`, `highlight`,
  `multiple_choice`, `matrix`), `args` are its arguments -- or a
  **generator** (`"generator": true`, `type` names a generator in
  `qx_generator_registry`; only `book_excerpt` ships built-in) that expands
  into many questions. A literal entry can also set
  `"force_page_break_before": true`.
- `highlight` and `book_excerpt` both default `categories`/`category_colors`
  to a six-point green-to-red sentiment scale (Liked, Insightful, Inspiring,
  Confusing, Polarizing, Disliked) -- pass your own `categories` (and
  optionally `category_colors`, `category_text_colors`) in `args` to
  override. Text color is chosen automatically for readability against
  whatever background color you give unless you set it explicitly.
- `page_breaks.every` controls the regular pagination cadence (default 4).
  The `book_excerpt` generator additionally forces a page break before any
  chunk that starts with a detected section header (an ALL-CAPS short line
  with no sentence-ending punctuation, e.g. `"INFLAMMATION"` -- see
  `qx_is_section_header()` if your source material uses a different
  convention and needs a custom detector).

## Extending it

Both registries are plain named lists, so adding a new question type or a
new generator is just adding an entry:

```r
qx_builder_registry[["my_type"]] <- function(args) {
  # return a Qualtrics question payload list, using args$...
}

qx_generator_registry[["my_generator"]] <- function(args) {
  # return list(list(payload = ..., force_break = TRUE/FALSE), ...)
}
```

## Package layout

- `R/zzz.R` -- credential storage (`qx_set_credentials()`).
- `R/api-core.R` -- low-level HTTP calls (`qx_api_request()`,
  `qx_create_question()`, `qx_create_block()`, `qx_get_block()`,
  `qx_update_block_elements()`, `qx_get_survey_questions()`).
- `R/question-builders.R` -- `qx_question_*()` payload builders and
  `qx_builder_registry`.
- `R/chunking.R` -- pure text-processing helpers (`qx_split_paragraphs()`,
  `qx_split_sentences()`, `qx_is_section_header()`, `qx_htmlify()`,
  `qx_chunk_text()` / `qx_chunk_file()`).
- `R/blocks.R` -- `qx_insert_page_breaks()`, a pure function for splicing
  Page Break elements into a block's element list.
- `R/template.R` -- `qx_run_template()` and `qx_generator_registry`.
- `tests/run_tests.R` + `tests/cases/test-*.R` -- a small dependency-free
  test suite (no testthat) covering every pure function plus a fully mocked
  end-to-end run of `qx_run_template()`. Run with
  `Rscript tests/run_tests.R` from the package root, or via `R CMD check`.
