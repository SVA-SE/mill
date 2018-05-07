##' @keywords internal
to_orgmode <- function(x) UseMethod("to_orgmode")

##' @keywords internal
to_orgmode.report <- function(x) {
    c(to_orgmode(x$chapters),
      "* Time",
      "#+BEGIN: clocktable :maxlevel 2 :scope file",
      "#+CAPTION: Clock summary at [2015-12-20 Sun 21:28]",
      "| Headline     | Time   |",
      "|--------------+--------|",
      "| *Total time* | *0:00* |",
      "#+END:",
      "* Org-mode configuration",
      paste0("#+TODO: TODO(t!) UPDATE(u!) SUBMITTED(s!) CONVERTED(c!) ",
             "AUTHOR-FEEDBACK(a!) TYPESET(y!) PROOF(p!) AUTHOR-OK(o!) ",
             "REVIEW(r!) EDITOR(e!) | DONE(d@/!)"),
      "#+STARTUP: indent",
      "#+STARTUP: hidestars",
      "#+STARTUP: logdrawer",
      paste0("#+TITLE: ", x$report))
}

##' @keywords internal
to_orgmode.chapters <- function(x) {
    c("* [[file+emacs:chapters][Chapters]] [0%]",
      unlist(lapply(x, function(y) to_orgmode(y))))
}

##' @keywords internal
to_orgmode.chapter <- function(x) {

    items <- character(0)

    lines <- paste0("** TODO [[file+emacs:chapters/",
                    gsub("[[:space:]]", "%20", x$title),
                    "][", x$title, "]]")

    files <- c(basename(figure_files(x, "R")),
               basename(table_files(x, "tex")))

    if (length(files)) {
        items <- sapply(files, function(filename) {
            paste0("*** TODO [[file:chapters/",
                   gsub("[[:space:]]", "%20", x$title),
                   "/", filename,
                   "][", tools::file_path_sans_ext(filename), "]]")
        }, USE.NAMES = FALSE)
    }

    lines <- c(lines, items)

    lines
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
                stop("Not implemented")

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

##' STARS KEYWORD PRIORITY TITLE TAGS
##' @noRd
org_headline <- function(x) {
    if (!identical(grep("^[*]+(\\s|$)", x[1]), 1L))
        return(NULL)

    ## Extract the level of the headline
    level <- nchar(regmatches(x[1], regexpr("^[*]+", x[1])))

    ## Extract the headline
    headline <- trimws(sub("[*]*", "", x[1]))
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

    list(result = structure(list(level = level,
                                 headline = headline,
                                 contents = x),
                            class = "org_headline"),
         remainder = remainder)
}

##' @noRd
org_doc <- function(x) {
    stopifnot(is.character(x), length(x) > 0)

    contents <- list()

    repeat {
        if (identical(grep("^[*]+(\\s|$)", x[1]), 1L)) {
            org <- org_headline(x)
            contents[[length(contents) + 1]] <- org$result
        } else {
            stop("Not implemented")
        }

        x <- org$remainder
        if (is.null(x))
            break
    }

    structure(list(contents = contents), class = "org_doc")
}
