##' @noRd
org_paragraph <- function(x) {
    if (length(x) < 1)
        return(NULL)

    paragraph <- x[1]

    ## Extract remainder
    if (length(x) > 1) {
        remainder <- x[-1]
    } else {
        remainder <- NULL
    }

    list(result = structure(list(paragraph = paragraph),
                            class = "org_paragraph"),
         remainder = remainder)
}

##' @noRd
org_keyword <- function(x) {
    if (!identical(grep("^#[+][^\\s:]+:", x[1]), 1L))
        return(NULL)

    ## Extract key and value
    key <- sub("^#[+]", "", regmatches(x[1], regexpr("#[+][^:]+", x[1])))
    value <- trimws(substr(x[1], nchar(key) + 4, nchar(x[1])))

    ## Extract remainder
    if (length(x) > 1) {
        remainder <- x[-1]
    } else {
        remainder <- NULL
    }

    list(result = structure(list(key = key, value = value),
                            class = "org_keyword"),
         remainder = remainder)
}

##' @noRd
org_dynamic_block <- function(x) {
    if (!identical(grep("^#[+]BEGIN:\\s[^\\s]+", x[1]), 1L))
        return(NULL)

    ## Find end of dynamic block
    end <- grep("^#[+]END:$", x)
    if (identical(length(end), 0L))
        return(NULL)
    end <- min(end)

    ## Extract remainder
    if (end < length(x)) {
        remainder <- x[seq(from = end + 1, to = length(x), by = 1)]
    } else {
        remainder <- NULL
    }

    ## Extract name and parameters of the dynamic block.
    parameters <- trimws(sub("^#[+]BEGIN:", "", x[1]))
    name <- regmatches(parameters, regexpr("^[^[:space:]]+", parameters))
    parameters <- trimws(substr(parameters, nchar(name) + 1, nchar(parameters)))
    if (identical(nchar(parameters), 0L))
        parameters <- NULL

    ## Extract content of the dynaminc block
    if (end > 2) {
        x <- x[seq(from = 2, to = end - 1, by = 1)]
    } else {
        x <- NULL
    }

    list(result = structure(list(name = name,
                                 parameters = parameters,
                                 contents = x),
                            class = "org_dynamic_block"),
         remainder = remainder)
}

##' @noRd
org_clock <- function(x) {
    if (!identical(grep("^CLOCK:", x[1]), 1L))
        return(NULL)

    clock <- structure(list(clock = trimws(sub("^CLOCK:", "", x[1])),
                            class = "org_clock"))

    ## Extract remainder
    if (length(x) > 1) {
        x <- x[-1]
    } else {
        x = NULL
    }

    list(result = clock, remainder = x)
}

##' @noRd
org_state_change <- function(x) {
    if (!identical(grep("^- State ", x[1]), 1L))
        return(NULL)

    state_change <- structure(list(state_change = trimws(sub("^- State", "", x[1])),
                                   class = "org_state_change"))

    ## Extract remainder
    if (length(x) > 1) {
        x <- x[-1]
    } else {
        x = NULL
    }

    list(result = state_change, remainder = x)
}

##' @noRd
org_schedule <- function(x) {
    if (!identical(grep("SCHEDULED:|DEADLINE:", x[1]), 1L))
        return(NULL)

    words <- regmatches(x[1], gregexpr("SCHEDULED:|DEADLINE:", x[1]), invert = NA)[[1]]
    deadline <- trimws(words[grep("DEADLINE:", words)+1])
    if(identical(grep("<[^>]*>", deadline), integer(0))) deadline  <- NULL
    scheduled <- trimws(words[grep("SCHEDULED:", words)+1])
    if(identical(grep("<[^>]*>", scheduled), integer(0))) scheduled  <- NULL

    schedule <- structure(list(scheduled = scheduled,
                               deadline = deadline),
                          class = "org_schedule")

    ## Extract remainder
    if (length(x) > 1) {
        x <- x[-1]
    } else {
        x = NULL
    }

    list(result = schedule, remainder = x)
}

##' @noRd
org_list <- function(x) {
    if (!identical(grep("^-", x[1]), 1L))
        return(NULL)

    items <- list()
    repeat {
        if (!identical(grep("^-", x[1]), 1L))
            break
        items[[length(items) + 1]] <- structure(list(item = x[1]),
                                                class = "org_item")
        x <- x[-1]
        if (identical(length(x), 0L))
            x <- NULL
    }

    list(result = structure(list(items= items), class = "org_list"),
         remainder = x)
}

##' @noRd
org_drawer <- function(x) {
    if (!identical(grep("^:[^\\s:]+:$", x[1]), 1L))
        return(NULL)

    ## Find end of drawer
    end <- grep("^:END:$", x)
    if (identical(length(end), 0L))
        return(NULL)
    end <- min(end)

    ## Extract remainder
    if (end < length(x)) {
        remainder <- x[seq(from = end + 1, to = length(x), by = 1)]
    } else {
        remainder <- NULL
    }

    ## Extract name of drawer
    name <- sub("^:", "", sub(":$", "", x[1]))

    ## Extract content of drawer
    if (end > 2) {
        x <- x[seq(from = 2, to = end - 1, by = 1)]
    } else {
        x <- NULL
    }

    ## Parse content of drawer
    contents <- list()
    if (!is.null(x)) {
        repeat {
            org <- org_list(x)
            if (is.null(org))
                org <- org_clock(x)
            if (is.null(org))
                org <- org_paragraph(x)

            contents[[length(contents) + 1]] <- org$result
            x <- org$remainder
            if (is.null(x))
                break
        }
    }

    drawer <- structure(list(name = name, contents = contents),
                        class = "org_drawer")

    list(result = drawer, remainder = remainder)
}

##' @noRd
org_tags <- function(x) {
    tags <- regmatches(x, regexpr(":.*:$", x))
    tags <- unlist(strsplit(tags, ":"))
    tags[tags != ""]
}

##' STARS KEYWORD PRIORITY TITLE TAGS
##' @noRd
org_headline <- function(x) {
    if (!identical(grep("^[*]+(\\s|$)", x[1]), 1L))
        return(NULL)

    ## Extract the level of the headline
    level <- nchar(regmatches(x[1], regexpr("^[*]+", x[1])))

    ## Extract the headline
    headline_text <- trimws(sub("[*]*", "", x[1]))
    headline <- trimws(strsplit(headline_text, ":")[[1]][1])
    if(is.na(headline)) headline <- ""

    ## Extract the tags
    tags <- org_tags(headline_text)

    ##
    x <- x[-1]

    ## Find end of contents under headline i.e. search for lines that
    ## start new headlines at the same or a lower level.
    remainder <- NULL
    i <- grep("^[*]+(\\s|$)", x)
    i <- i[which(nchar(regmatches(x[i], regexpr("^[*]+", x[i]))) <= level)]
    if (length(i)) {
        ## Extract remainder.
        i <- min(i)
        remainder <- x[seq(from = i, to = length(x), by = 1)]
        if (i > 1) {
            x <- x[seq(from = 1, to = i - 1, by = 1)]
        } else {
            x <- NULL
        }
    } else if (identical(length(x), 0L)) {
        x <- NULL
    }

    ## Parse content of headline
    if (!is.null(x)) {
        section <- list()

        repeat {
            org <- org_headline(x)
            if (is.null(org))
                org <- org_drawer(x)
            if(is.null(org))
                org <- org_dynamic_block(x)
            if(is.null(org))
                org <- org_keyword(x)
            if(is.null(org))
                org <- org_state_change(x)
            if(is.null(org))
                org <- org_schedule(x)
            if (is.null(org))
                org <- org_paragraph(x)

            section[[length(section) + 1]] <- org$result
            x <- org$remainder
            if (is.null(x))
                break
        }
    } else {
        section <- NULL
    }

    list(result = structure(list(level = level,
                                 headline = headline,
                                 tags = tags,
                                 section = section),
                            class = "org_headline"),
         remainder = remainder)
}

##' @noRd
org_doc <- function(x) {
    stopifnot(is.character(x), length(x) > 0)

    contents <- list()

    repeat {
        org <- org_headline(x)
        if (is.null(org))
            stop(x[1], "\nNot implemented")

        contents[[length(contents) + 1]] <- org$result
        x <- org$remainder
        if (is.null(x))
            break
    }

    structure(list(contents = contents), class = "org_doc")
}
