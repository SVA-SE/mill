library("mill")

tools::assertError(mill:::orgmode_parse_author(1))
tools::assertError(mill:::orgmode_parse_author(c("A", "B")))
mill:::orgmode_parse_author("AUTHOR: Alice [Org A] <alice@example.org>")

tools::assertError(orgmode_parse_authors(1))
tools::assertError(orgmode_parse_authors(c("A", "B")))
tools::assertError(orgmode_parse_authors(c("A", "B", "C")))
lines <- c(":AUTHORS:",
           "AUTHOR: Alice [Org A] <alice@example.org>",
           "AUTHOR: Bob [Org B] <bob@example.org>",
           ":END:")
mill:::orgmode_parse_authors(lines)

## Parse drawer
lines <- c(":AUTHORS:",
           "AUTHOR: Alice [Org A] <alice@example.org>",
           "AUTHOR: Bob [Org B] <bob@example.org>",
           ":END:")

result_expected <- structure(list(
    result = structure(list(name = "AUTHORS",
                            contents = c("AUTHOR: Alice [Org A] <alice@example.org>",
                                         "AUTHOR: Bob [Org B] <bob@example.org>")),
                       .Names = c("name", "contents"),
                       class = "org_drawer"),
    remainder = c(":AUTHORS:",
                  "AUTHOR: Alice [Org A] <alice@example.org>",
                  "AUTHOR: Bob [Org B] <bob@example.org>",
                  ":END:")),
    .Names = c("result", "remainder"))

result_observed <- mill:::orgmode_parse_drawer(rep(lines, 2))

stopifnot(identical(result_observed, result_expected))

## Parse headline
tools::assertError(mill:::orgmode_parse_headline(1))
tools::assertError(mill:::orgmode_parse_headline(character(0)))

result_expected <- structure(list(
    result = structure(list(level = 1L, headline = "", contents = NULL),
                       .Names = c("level", "headline", "contents"),
                       class = "org_headline"),
    remainder = NULL),
    .Names = c("result", "remainder"))
result_observed <- mill:::orgmode_parse_headline("*")
stopifnot(identical(result_observed, result_expected))

result_expected <- structure(list(
    result = structure(list(level = 2L, headline = "DONE", contents = NULL),
                       .Names = c("level", "headline", "contents"),
                       class = "org_headline"),
    remainder = NULL), .Names = c("result", "remainder"))
result_observed <- mill:::orgmode_parse_headline("** DONE")
stopifnot(identical(result_observed, result_expected))

result_expected <- structure(list(
    result = structure(list(level = 3L, headline = "Some e-mail", contents = NULL),
                       .Names = c("level", "headline", "contents"),
                       class = "org_headline"),
    remainder = NULL), .Names = c("result", "remainder"))
result_observed <- mill:::orgmode_parse_headline("*** Some e-mail")
stopifnot(identical(result_observed, result_expected))

result_expected <- structure(list(
    result = structure(list(level = 4L,
                            headline = "TODO [#A] COMMENT Title :tag:a2%:",
                            contents = NULL),
                       .Names = c("level", "headline", "contents"),
                       class = "org_headline"),
    remainder = NULL), .Names = c("result", "remainder"))
result_observed <- mill:::orgmode_parse_headline("**** TODO [#A] COMMENT Title :tag:a2%:")
stopifnot(identical(result_observed, result_expected))

result_expected <- structure(list(
    result = structure(list(level = 1L,
                            headline = "Chapter 1",
                            contents = NULL),
                       .Names = c("level", "headline", "contents"),
                       class = "org_headline"),
    remainder = "* Chapter 2"),
    .Names = c("result", "remainder"))
result_observed <- mill:::orgmode_parse_headline(c("* Chapter 1", "* Chapter 2"))
stopifnot(identical(result_observed, result_expected))
