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

##' @keywords internal
orgmode_parse_author <- function(x) {
    stopifnot(is.character(x),
              identical(length(x), 1L),
              identical(grep("^AUTHOR:", x), 1L))

    ## Extract author name
    x <- trimws(sub("^AUTHOR:", "", x))
    x <- unlist(strsplit(x, "[[]"))
    stopifnot(identical(length(x), 2L))
    n <- trimws(x[1])

    ## Extract organization
    x <- trimws(x[2])
    x <- unlist(strsplit(x, "[]]"))
    stopifnot(identical(length(x), 2L))
    o <- trimws(x[1])

    ## Extract email
    x <- trimws(x[2])
    stopifnot(identical(grep("^<.+>$", x), 1L))
    e <- sub("^<", "", sub(">$", "", x))

    structure(list(name = n,
                   email = e,
                   organisation = o),
              class = "author")
}

##' @keywords internal
orgmode_parse_authors <- function(x) {
    stopifnot(is.character(x),
              length(x) > 2,
              identical(grep("^:AUTHORS:", x), 1L))

    a <- list()
    x <- x[-1]
    repeat {
        if (!identical(grep("^AUTHOR:", x[1]), 1L))
            break
        a[[length(a)+1]] <- orgmode_parse_author(x[1])
        x <- x[-1]
    }
    a
}

##' @keywords internal
orgmode_parse_drawer <- function(x) {
    stopifnot(is.character(x),
              length(x) > 1,
              identical(grep("^:[^:]+:$", x[1]), 1L))

    ## Extract name of drawer
    name <- sub("^:", "", sub(":$", "", x[1]))

    ## Find end of drawer
    end <- grep("^:END:$", x)
    stopifnot(length(end) > 0)
    end <- min(end)

    ## Extract content of drawer
    if (end > 2) {
        contents <- x[seq(from = 2, to = end - 1, by = 1)]
    } else {
        contents <- character(0)
    }

    drawer <- structure(list(name = name, contents = contents), class = "org_drawer")

    ## Extract remainder
    if (end < length(x)) {
        remainder <- x[seq(from = end + 1, to = length(x), by = 1)]
    } else {
        remainder <- NULL
    }

    list(result = drawer, remainder = remainder)
}
