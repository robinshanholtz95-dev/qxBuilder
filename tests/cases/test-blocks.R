## Tests for R/blocks.R (qx_insert_page_breaks)

mk_q <- function(id) list(Type = "Question", QuestionID = id)

## -- Basic every-N pagination, no pre-existing elements
elements <- lapply(paste0("QID", 1:8), mk_q)
qids <- paste0("QID", 1:8)

result <- qx_insert_page_breaks(elements, qids, every = 4)
types <- vapply(result, function(e) e$Type, character(1))
expect_equal(sum(types == "Page Break"), 1L, "insert_page_breaks: one break for 8 questions at every=4")
# Break should sit right after QID4, i.e. between QID4 and QID5
qid_positions <- which(vapply(result, function(e) !is.null(e$QuestionID) && e$QuestionID == "QID4", logical(1)))
expect_equal(result[[qid_positions + 1]]$Type, "Page Break", "insert_page_breaks: break immediately follows the 4th question")

## -- No trailing break after the very last question
expect_true(result[[length(result)]]$Type == "Question", "insert_page_breaks: no dangling break after the last question")

## -- Pre-existing elements (not in our created list) are left alone and not counted
elements2 <- c(list(mk_q("OLD1")), lapply(paste0("QID", 1:4), mk_q), list(mk_q("OLD2")))
result2 <- qx_insert_page_breaks(elements2, paste0("QID", 1:4), every = 4)
expect_equal(length(result2), length(elements2), "insert_page_breaks: pre-existing questions don't trigger extra breaks")

## -- force_break_before forces a break even off the regular cadence
elements3 <- lapply(paste0("QID", 1:3), mk_q)
qids3 <- paste0("QID", 1:3)
force3 <- c(FALSE, FALSE, TRUE)  # 3rd question should always get a break before it
result3 <- qx_insert_page_breaks(elements3, qids3, force_break_before = force3, every = 100)
qid3_pos <- which(vapply(result3, function(e) !is.null(e$QuestionID) && e$QuestionID == "QID3", logical(1)))
expect_equal(result3[[qid3_pos - 1]]$Type, "Page Break", "insert_page_breaks: forced break precedes the flagged question")

## -- Never two breaks back to back (forced + regular cadence coinciding)
elements4 <- lapply(paste0("QID", 1:4), mk_q)
qids4 <- paste0("QID", 1:4)
force4 <- c(FALSE, FALSE, FALSE, TRUE)  # 4th question coincides with every=4's natural boundary... but every=4 only
                                         # triggers AFTER the 4th, not before -- use every=3 to force an actual collision
result4 <- qx_insert_page_breaks(elements4, qids4, force_break_before = c(FALSE, FALSE, TRUE, FALSE), every = 3)
types4 <- vapply(result4, function(e) e$Type, character(1))
# No two consecutive "Page Break" entries anywhere
consecutive_breaks <- any(types4[-1] == "Page Break" & types4[-length(types4)] == "Page Break")
expect_true(!consecutive_breaks, "insert_page_breaks: never inserts two breaks back to back")
