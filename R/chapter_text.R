##' Build chapters
##'
##' @param x The report object, chapter object or the path to the text
##'     tex file.
##' @return invisible NULL
##' @export
build_text <- function(x) UseMethod("build_text")

##' @export
build_text.report <- function(x) {
    preview <- tempfile(tmpdir = x$path, fileext = ".tex")
    on.exit(unlink(preview))
    cleanup_patched_files <- function(x) {
        lapply(x$chapters, function(y){
            unlink(file.path(y$path, "typeset.tex"))
        })
    }
    on.exit(cleanup_patched_files(x), add = TRUE)

    ## read in the pieces of the chapters
    a <- assets(x)
    apply_patch(x)
    presnippet <- readLines(file.path(a, "latex/pre-snippet.tex"))
    presnippet <- gsub("assets/", paste0(a, "/"), presnippet)
    text <- readLines(file.path(a, "latex/report.tex"))
    text <- gsub("assets/", paste0(a, "/"), text)
    ## Stitch together the chapter
    tex <- c(presnippet,
             "\\begin{document}",
             text,
             "\\end{document}")
    writeLines(tex, preview)

    ## Build the preview pdf file.
    luatex(preview)

    ## Copy the pdf preview to 'preview-figure.pdf'
    from <- paste0(tools::file_path_sans_ext(preview), ".pdf")
    to <- file.path(x$path,"report.pdf")
    if (file.exists(to))
        file.remove(to)
    file.rename(from, to)
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
    presnippet <- readLines(file.path(a, "latex/pre-snippet.tex"))
    presnippet <- gsub("assets/", paste0(a, "/"), presnippet)
    ## Stitch together the chapter
    tex <- c(presnippet,
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
