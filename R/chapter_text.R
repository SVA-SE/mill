##' Build chapters
##'
##' @param x The report object, chapter object or the path to the text
##'     tex file.
##' @return invisible NULL
##' @export
build_text <- function(x) UseMethod("build_text")

##' @export
build_text.report <- function(x) {
    build_text(x$chapters)
}

##' @export
build_text.chapters <- function(x) {
    lapply(x, function(y) build_text(y))
    invisible()
}

##' @export
build_text.chapter <- function(x) {
    preview <- tempfile(tmpdir = x$path, fileext = ".tex")
    on.exit(unlink(preview))
    on.exit(unlink(file.path(x$path, "typeset.tex")), add = TRUE)

    ## read in the pieces of the chapter
    a <- assets(x)
    apply_patch(x)
    text <- readLines(file.path(x$path, "typeset.tex"))

    ## Stitch together the chapter
    tex <- c(readLines(file.path(a, "figure-preview/pre-snippet.tex")),
             "\\begin{document}",
             text,
             "\\end{document}")
    writeLines(tex, preview)

    ## Build the preview pdf file.
    luatex(preview)

    ## Copy the pdf preview to 'preview-figure.pdf'
    from <- paste0(tools::file_path_sans_ext(preview), ".pdf")
    to <- file.path(x$path,
                    paste0(gsub(" ", "-", x$title), ".pdf"))
    if (file.exists(to))
        file.remove(to)
    file.rename(from, to)
}
