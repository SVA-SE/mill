##' @keywords internal
text_pattern <- function(fileext = c("tex")) {
    fileext <- switch(match.arg(fileext),
                      tex = "tex$")

    paste0("^text[.]", fileext)
}

##' Get the path(s) of the chapter text.tex file(s)
##'
##' @export
##' @param x The chapter object
##' @return path
text_files <- function(x) UseMethod("text_files")

##' @export
text_files.chapter <- function(x) {
    list.files(path = x$path,
               pattern = text_pattern(),
               full.names = TRUE)
}

##' Preview chapters
##'
##' @param x The report object, chapter object or the path to the text
##'     tex file.
##' @return invisible NULL
##' @export
preview_text <- function(x) UseMethod("preview_text")

##' @export
preview_text.report <- function(x) {
    preview_text(x$chapters)
}

##' @export
preview_text.chapters <- function(x) {
    lapply(x, function(y) preview_text(y))
    invisible()
}

##' @export
preview_text.chapter <- function(x) {
    preview <- tempfile(tmpdir = x$path, fileext = ".tex")
    on.exit(unlink(preview))

    ## read in the pieces of the chapter
    a <- assets(x)
    text <- readLines(text_files(x))
    figures <- do.call("c", lapply(figure_files(x, "tex"), readLines))
    tables <- do.call("c", lapply(table_files(x, "tex"), readLines))

    ## Stitch together the chapter
    tex <- c(readLines(file.path(a, "figure-preview/pre-snippet.tex")),
             "\\begin{document}",
             text,
             figures,
             tables,
             "\\end{document}")
    writeLines(tex, preview)

    ## Build the preview pdf file.
    luatex(preview)

    ## Copy the pdf preview to 'preview-figure.pdf'
    from <- paste0(tools::file_path_sans_ext(preview), ".pdf")
    to <- file.path(x$path,
                    paste0("preview-text.pdf"))
    if (file.exists(to))
        file.remove(to)
    file.rename(from, to)
}
