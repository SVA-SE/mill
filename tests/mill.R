library("mill")

##################
### Parse list ###
##################

result_expected <- structure(
    list(result = structure(
             list(items = list(structure(
                      list(item = "- Alice (Org A) <alice@example.org>"),
                      .Names = "item", class = "org_item"),
                      structure(list(item = "- Bob (Org B) <bob@example.org>"),
                                .Names = "item", class = "org_item"))),
             .Names = "items", class = "org_list"),
         remainder = NULL),
    .Names = c("result", "remainder"))

result_observed <- mill:::org_list(c("- Alice (Org A) <alice@example.org>",
                                     "- Bob (Org B) <bob@example.org>"))

stopifnot(identical(result_observed, result_expected))

####################
### Parse drawer ###
####################

stopifnot(is.null(mill:::org_drawer(1)))
stopifnot(is.null(mill:::org_drawer(character(0))))
stopifnot(is.null(mill:::org_drawer(":LOGBOOK :")))
stopifnot(is.null(mill:::org_drawer(":LOGBOOK:")))
stopifnot(is.null(mill:::org_drawer(c(":LOGBOOK:", "content"))))

##

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

stopifnot(is.null(mill:::org_headline(1)))
stopifnot(is.null(mill:::org_headline(character(0))))

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
