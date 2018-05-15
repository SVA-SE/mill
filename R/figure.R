##' @keywords internal
figure_pattern <- function(fileext = c("all", "R", "tex", "xlsx", "pdf", "jpg")) {
    fileext <- switch(match.arg(fileext),
                      all  = "((tex)|(R)|(xlsx)|(pdf)|(jpg))$",
                      R    = "R$",
                      xlsx = "xlsx$",
                      tex  = "tex$",
                      pdf  = "pdf$")

    paste0("^fig_[^.]+[.]", fileext)
}

##' List figure files
##'
##' A method to discover the figures in a report or a chapter.
##'
##' @export
##' @param fileext The extension of the files you want to find
figure_files <- function(fileext = "all") {
    if (in_chapter())
        return(list.files(pattern = figure_pattern(fileext)))
    stop("Not implemented")
}

##' @keywords internal
preview_pattern <- function(items = c("all", "figure", "table")) {
    items <- switch(match.arg(items),
                    all    = "((table)|(figure))",
                    figure = "figure",
                    table  = "table")

    paste0("^preview-", items, "-[^.]*[.]pdf$")
}

##' @keywords internal
preview_files <- function(x, items) UseMethod("preview_files")

##' @keywords internal
preview_files.chapter <- function(x, items = "all") {
    list.files(path = x$path,
               pattern = preview_pattern(items),
               full.names = TRUE)
}

##' Build figures
##'
##' @return invisible NULL
##' @export
build_figures <- function() {
    if (in_chapter()) {
        lapply(figure_files("R"), function(figure) {
            source(figure, local = TRUE, chdir = TRUE)
        })
    } else if (in_report()) {
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            build_figures()
            setwd(wd)
        })
    }

    invisible(NULL)
}

##' Preview figures
##'
##' @param x The report object or chapter object
##' @return invisible NULL
##' @export
preview_figures <- function(x) UseMethod("preview_figures")

##' @export
preview_figures.report <- function(x) {
    preview_figures(x$chapters)
}

##' @export
preview_figures.chapters <- function(x) {
    lapply(x, function(y) preview_figures(y))
    invisible()
}

##' @export
preview_figures.chapter <- function(x) {
    lapply(figure_files(x, "tex"), preview_figure)
    invisible()
}

##' Preview a figure
##'
##' @param figure The path to the figure tex file
##' @importFrom tools file_path_sans_ext
##' @export
preview_figure <- function(figure) {
    preview <- tempfile(tmpdir = dirname(figure), fileext = ".tex")
    on.exit(unlink(preview))

    ## Create a tex file with the context to create a preview.
    a <- assets(figure)
    tex <- c(readLines(file.path(a, "figure-preview/pre-snippet.tex")),
             "\\captionsetup{labelformat = empty}",
             "\\begin{document}",
             "\\begin{LARGE}",
             explain_labeling(),
             "\\newline\\newline",
             "\\hl{",
             get_label(figure, "word"),
             "}",
             "\\end{LARGE}",
             readLines(figure),
             "\\end{document}")
    writeLines(tex, preview)

    ## Build the preview pdf file.
    luatex(preview)

    ## Copy the pdf preview to 'preview-figure.pdf'
    from <- paste0(file_path_sans_ext(preview), ".pdf")
    to <- file.path(dirname(figure),
                    paste0("preview-", file_path_sans_ext(basename(figure)), ".pdf"))
    if (file.exists(to))
        file.remove(to)
    file.rename(from, to)
}

##' Get the label from a figure path
##'
##' @keywords internal
get_label <- function(figure, format = c("latex", "word")) {
    format <- match.arg(format)
    tex <- readLines(figure)
    m <- regmatches(tex, regexec("[\\]label[{]([^}]*)", tex))
    labels <- unlist(lapply(m, function(x) {if (length(x)) x[2] else x}))
    format_labels(labels, format)
}

##' Format the label from a figure path
##'
##' @keywords internal
format_labels <- function(labels, format = c("latex", "word")) {
    format <- match.arg(format)
    if (identical(format, "word")) {
        labels <- gsub("([^:]*)[:][^:]*[:]([^:]*)", "{[}\\1:\\2{]}", labels)
        return(labels)
    }
    return(labels)
}

##'Explain the labeling
##'
##' @keywords internal
explain_labeling <- function() {
    paste("\\noindent To reference this figure or table in the word document",
          "you need to insert the following \\textbf{label(s)} into",
          "the text:")
}
