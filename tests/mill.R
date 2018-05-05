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
           "- Alice (Org A) <alice@example.org>",
           "- Bob (Org B) <bob@example.org>",
           ":END:")

result_expected <- structure(list(
    result = structure(list(name = "AUTHORS",
                            contents = c("- Alice (Org A) <alice@example.org>",
                                         "- Bob (Org B) <bob@example.org>")),
                       .Names = c("name", "contents"),
                       class = "org_drawer"),
    remainder = c(":AUTHORS:",
                  "- Alice (Org A) <alice@example.org>",
                  "- Bob (Org B) <bob@example.org>",
                  ":END:")),
    .Names = c("result", "remainder"))

result_observed <- mill:::org_drawer(rep(lines, 2))

stopifnot(identical(result_observed, result_expected))

######################
### Parse headline ###
######################

tools::assertError(mill:::org_headline(1))
tools::assertError(mill:::org_headline(character(0)))

##

result_expected <- structure(list(
    result = structure(list(level = 1L, headline = "", contents = NULL),
                       .Names = c("level", "headline", "contents"),
                       class = "org_headline"),
    remainder = NULL),
    .Names = c("result", "remainder"))
result_observed <- mill:::org_headline("*")
stopifnot(identical(result_observed, result_expected))

##

result_expected <- structure(list(
    result = structure(list(level = 2L, headline = "DONE", contents = NULL),
                       .Names = c("level", "headline", "contents"),
                       class = "org_headline"),
    remainder = NULL), .Names = c("result", "remainder"))
result_observed <- mill:::org_headline("** DONE")
stopifnot(identical(result_observed, result_expected))

result_expected <- structure(list(
    result = structure(list(level = 3L, headline = "Some e-mail", contents = NULL),
                       .Names = c("level", "headline", "contents"),
                       class = "org_headline"),
    remainder = NULL), .Names = c("result", "remainder"))
result_observed <- mill:::org_headline("*** Some e-mail")
stopifnot(identical(result_observed, result_expected))

##

result_expected <- structure(list(
    result = structure(list(level = 4L,
                            headline = "TODO [#A] COMMENT Title :tag:a2%:",
                            contents = NULL),
                       .Names = c("level", "headline", "contents"),
                       class = "org_headline"),
    remainder = NULL), .Names = c("result", "remainder"))
result_observed <- mill:::org_headline("**** TODO [#A] COMMENT Title :tag:a2%:")
stopifnot(identical(result_observed, result_expected))

##

result_expected <- structure(list(
    result = structure(list(level = 1L,
                            headline = "Chapter 1",
                            contents = NULL),
                       .Names = c("level", "headline", "contents"),
                       class = "org_headline"),
    remainder = "* Chapter 2"),
    .Names = c("result", "remainder"))
result_observed <- mill:::org_headline(c("* Chapter 1", "* Chapter 2"))
stopifnot(identical(result_observed, result_expected))

##########################
### Parse org document ###
##########################

result_expected <- structure(
    list(contents = list(structure(list(level = 1L,
                                        headline = "Chapter 1",
                                        contents = NULL),
                                   .Names = c("level", "headline", "contents"),
                                   class = "org_headline"),
                         structure(list(level = 1L,
                                        headline = "Chapter 2",
                                        contents = NULL),
                                   .Names = c("level", "headline", "contents"),
                                   class = "org_headline"))),
    .Names = "contents", class = "org_doc")
result_observed <- mill:::org_doc(c("* Chapter 1", "* Chapter 2"))
stopifnot(identical(result_observed, result_expected))
