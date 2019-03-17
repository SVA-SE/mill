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

result_expected <- structure(list(result = structure(list(name = "AUTHORS",
  contents = list(structure(list(items = list(structure(list(
  item = "- Alice (Org A) <alice@example.org>"), .Names = "item", class = "org_item"),
  structure(list(item = "- Bob (Org B) <bob@example.org>"), .Names = "item",
            class = "org_item"))), .Names = "items", class = "org_list"))),
  .Names = c("name", "contents"), class = "org_drawer"),
  remainder = c(":AUTHORS:", "- Alice (Org A) <alice@example.org>",
                "- Bob (Org B) <bob@example.org>", ":END:")),
  .Names = c("result", "remainder"))

result_observed <- mill:::org_drawer(rep(lines, 2))

stopifnot(identical(result_observed, result_expected))

##

lines <- c(":AUTHORS:",
           "- Alice (Org A) <alice@example.org>",
           "CLOCK: [2018-04-24 Tue 13:42]--[2018-04-24 Tue 13:43] =>  0:01",
           "- Bob (Org B) <bob@example.org>",
           ":END:")

result_expected <- structure(list(result = structure(list(name = "AUTHORS",
  contents = list(structure(list(items = list(structure(list(
  item = "- Alice (Org A) <alice@example.org>"), .Names = "item", class = "org_item"))),
  .Names = "items", class = "org_list"),
  structure(list(clock = "[2018-04-24 Tue 13:42]--[2018-04-24 Tue 13:43] =>  0:01",
                 class = "org_clock"), .Names = c("clock", "class")),
  structure(list(items = list(structure(list(item = "- Bob (Org B) <bob@example.org>"),
  .Names = "item", class = "org_item"))), .Names = "items", class = "org_list"))),
  .Names = c("name", "contents"), class = "org_drawer"), remainder = c(":AUTHORS:",
  "- Alice (Org A) <alice@example.org>", "CLOCK: [2018-04-24 Tue 13:42]--[2018-04-24 Tue 13:43] =>  0:01",
  "- Bob (Org B) <bob@example.org>", ":END:")), .Names = c("result", "remainder"))

result_observed <- mill:::org_drawer(rep(lines, 2))

stopifnot(identical(result_observed, result_expected))

######################
### Parse headline ###
######################

stopifnot(is.null(mill:::org_headline(1)))
stopifnot(is.null(mill:::org_headline(character(0))))

##

result_expected <- structure(list(
    result = structure(list(level = 1L, headline = "", tags = NULL, section = NULL),
                       .Names = c("level", "headline", "tags", "section"),
                       class = "org_headline"),
    remainder = NULL),
    .Names = c("result", "remainder"))
result_observed <- mill:::org_headline("*")
stopifnot(identical(result_observed, result_expected))

##

result_expected <- structure(list(
    result = structure(list(level = 2L, headline = "DONE", tags = NULL, section = NULL),
                       .Names = c("level", "headline", "tags", "section"),
                       class = "org_headline"),
    remainder = NULL), .Names = c("result", "remainder"))
result_observed <- mill:::org_headline("** DONE")
stopifnot(identical(result_observed, result_expected))

result_expected <- structure(list(
    result = structure(list(level = 3L, headline = "Some e-mail", tags = NULL, section = NULL),
                       .Names = c("level", "headline", "tags", "section"),
                       class = "org_headline"),
    remainder = NULL), .Names = c("result", "remainder"))
result_observed <- mill:::org_headline("*** Some e-mail")
stopifnot(identical(result_observed, result_expected))

##

result_expected <- structure(list(
    result = structure(list(level = 4L,
                            headline = "TODO [#A] COMMENT Title",
                            tags = c("tag", "a2%"),
                            section = NULL),
                       .Names = c("level", "headline", "tags", "section"),
                       class = "org_headline"),
    remainder = NULL), .Names = c("result", "remainder"))
result_observed <- mill:::org_headline("**** TODO [#A] COMMENT Title :tag:a2%:")
stopifnot(identical(result_observed, result_expected))

##

result_expected <- structure(list(
    result = structure(list(level = 1L,
                            headline = "Chapter 1",
                            tags = NULL,
                            section = NULL),
                       .Names = c("level", "headline", "tags", "section"),
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
                                        tags = NULL,
                                        section = NULL),
                                   .Names = c("level", "headline", "tags", "section"),
                                   class = "org_headline"),
                         structure(list(level = 1L,
                                        headline = "Chapter 2",
                                        tags = NULL,
                                        section = NULL),
                                   .Names = c("level", "headline", "tags", "section"),
                                   class = "org_headline"))),
    .Names = "contents", class = "org_doc")
result_observed <- mill:::org_doc(c("* Chapter 1", "* Chapter 2"))
stopifnot(identical(result_observed, result_expected))

#############################
### Parse a dynamic block ###
#############################

lines <- c("#+BEGIN: clocktable :maxlevel 2 :scope file",
           "#+CAPTION: Clock summary at [2015-12-20 Sun 21:28]",
           "| Headline     | Time   |",
           "|--------------+--------|",
           "| *Total time* | *0:00* |",
           "#+END:")

result_expected <- structure(list(
    result = structure(
        list(name = "clocktable",
             parameters = ":maxlevel 2 :scope file",
             contents = c("#+CAPTION: Clock summary at [2015-12-20 Sun 21:28]",
                          "| Headline     | Time   |",
                          "|--------------+--------|",
                          "| *Total time* | *0:00* |")),
        .Names = c("name", "parameters", "contents"),
        class = "org_dynamic_block"),
    remainder = NULL), .Names = c("result", "remainder"))

result_observed <- mill:::org_dynamic_block(lines)

stopifnot(identical(result_observed, result_expected))

##

lines <- c("#+BEGIN: clocktable",
           "#+CAPTION: Clock summary at [2015-12-20 Sun 21:28]",
           "| Headline     | Time   |",
           "|--------------+--------|",
           "| *Total time* | *0:00* |",
           "#+END:")

result_expected <- structure(list(
    result = structure(
        list(name = "clocktable",
             parameters = NULL,
             contents = c("#+CAPTION: Clock summary at [2015-12-20 Sun 21:28]",
                          "| Headline     | Time   |",
                          "|--------------+--------|",
                          "| *Total time* | *0:00* |")),
        .Names = c("name", "parameters", "contents"),
        class = "org_dynamic_block"),
    remainder = NULL), .Names = c("result", "remainder"))

result_observed <- mill:::org_dynamic_block(lines)

stopifnot(identical(result_observed, result_expected))

#######################
### Parse a keyword ###
#######################

result_expected <- structure(list(result = structure(list(key = "STARTUP",
                                                          value = "logdrawer"),
                                                     .Names = c("key", "value"),
                                                     class = "org_keyword"),
                                  remainder = NULL),
                             .Names = c("result", "remainder"))

result_observed <- mill:::org_keyword("#+STARTUP: logdrawer")

stopifnot(identical(result_observed, result_expected))

############################
### Parse a state change ###
############################

result_observed <- mill:::org_state_change("- State \"DONE\"       from \"STARTED(s!)|\" [2018-12-07 fre 20:12]")

result_expected <- structure(list(result = list(state_change = "\"DONE\"       from \"STARTED(s!)|\" [2018-12-07 fre 20:12]",
                                                class = "org_state_change"),
                                  remainder = NULL),
                             .Names = c("result", "remainder"))

stopifnot(identical(result_observed, result_expected))

########################
### Parse a schedule ###
########################

schedule1 <- "SCHEDULED: <2018-11-26 mån> DEADLINE: <2018-11-27 tis>"
schedule1_exp <- list(result = structure(list(scheduled = "<2018-11-26 mån>",
                                              deadline = "<2018-11-27 tis>"),
                                         class = "org_schedule"),
                      remainder = NULL)

schedule2 <- "SCHEDULED: <2018-11-26 mån 08:05>--<2018-11-26 mån 17:00> DEADLINE: <2018-11-27 tis>"
schedule2_exp <- list(result = structure(list(scheduled = "<2018-11-26 mån 08:05>--<2018-11-26 mån 17:00>",
                                              deadline = "<2018-11-27 tis>"),
                                         class = "org_schedule"),
                      remainder = NULL)

schedule3 <- "DEADLINE: <2018-11-27 tis>"
schedule3_exp <- list(result = structure(list(scheduled = NULL, deadline = "<2018-11-27 tis>"),
                                         class = "org_schedule"),
                      remainder = NULL)

schedule4 <- "DEADLINE: <2018-11-27 tis> SCHEDULED: <2018-11-26 mån 08:05>--<2018-11-26 mån 17:00>"
schedule4_exp <- list(result = structure(list(scheduled = "<2018-11-26 mån 08:05>--<2018-11-26 mån 17:00>",
                                              deadline = "<2018-11-27 tis>"),
                                         class = "org_schedule"),
                      remainder = NULL)

schedule5 <- "SCHEDULED: <2018-11-26 mån 08:05>--<2018-11-26 mån 17:00>"
schedule5_exp <- list(result = structure(list(scheduled = "<2018-11-26 mån 08:05>--<2018-11-26 mån 17:00>",
                                              deadline = NULL),
                                         class = "org_schedule"),
                      remainder = NULL)

schedule6 <- "foobar"
schedule6_exp <- NULL

schedule7  <- ""
schedule7_exp <- NULL

stopifnot(identical(mill:::org_schedule(schedule1), schedule1_exp))
stopifnot(identical(mill:::org_schedule(schedule2), schedule2_exp))
stopifnot(identical(mill:::org_schedule(schedule3), schedule3_exp))
stopifnot(identical(mill:::org_schedule(schedule4), schedule4_exp))
stopifnot(identical(mill:::org_schedule(schedule5), schedule5_exp))
stopifnot(identical(mill:::org_schedule(schedule6), schedule6_exp))
stopifnot(identical(mill:::org_schedule(schedule7), schedule7_exp))
