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
    text <- readLines(chapter_tex_files(x, "text"))
    figures <- NULL
    tables <- NULL
    chapter_figure_files <- basename(figure_files(x, "tex"))
    chapter_table_files <- basename(table_files(x, "tex"))
    if (length(chapter_figure_files))
        figures <- paste0("\\input{", chapter_figure_files, "}")
    if (length(chapter_table_files))
        tables <- paste0("\\input{", chapter_table_files, "}")
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
