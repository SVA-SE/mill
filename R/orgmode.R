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
