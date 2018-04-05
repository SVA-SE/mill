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
