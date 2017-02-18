##' @keywords internal
figure_pattern <- function(fileext = c("all", "R", "tex", "xlsx", "pdf")) {
    fileext <- switch(match.arg(fileext),
                      all  = "(tex)|(R)|(csv)|(pdf)$",
                      R    = "R$",
                      xlsx = "xlsx$",
                      tex  = "tex$",
                      pdf  = "pdf$")

    paste0("^figure-[^.]+[.]", fileext)
}

##' @keywords internal
figure_files <- function(x, fileext) UseMethod("figure_files")

##' @keywords internal
figure_files.chapter <- function(x, fileext = "all") {
    list.files(path = x$path,
               pattern = figure_pattern(fileext),
               full.names = TRUE)
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
##' @param x The report object or chapter object
##' @return invisible NULL
##' @export
build_figures <- function(x) UseMethod("build_figures")

##' @export
build_figures.report <- function(x) {
    build_figures(x$chapters)
}

##' @export
build_figures.chapters <- function(x) {
    lapply(x, function(y) build_figures(y))
    invisible()
}

##' @export
build_figures.chapter <- function(x) {
    lapply(figure_files(x, "R"), build_figure)
    invisible()
}

##' Build a figure
##'
##' @param figure The path to the figure R script
##' @export
build_figure <- function(figure) {
    source(figure, local = TRUE, chdir = TRUE)
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

##' Get Assets
##'
##' Determine the assets directory given a report, chapter or chapter
##' file in the report project.
##' @param x The report object, chapter object or filename.
##' @return character string with path to assets.
##' @export
assets <- function(x) UseMethod("assets")

##' @export
assets.report <- function(x) {
    file.path(x$path, "assets")
}

##' @export
assets.chapter <- function(x) {
    file.path(dirname(dirname(x$path)), "assets")
}

##' @keywords internal
assets.character <- function(x) {
    file.path(dirname(dirname(dirname(x))), "assets")
}

##' Preview a figure
##'
##' @param figure The path to the figure tex file
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
    from <- paste0(tools::file_path_sans_ext(preview), ".pdf")
    to <- file.path(dirname(figure),
                    paste0("preview-", tools::file_path_sans_ext(basename(figure)), ".pdf"))
    if (file.exists(to))
        file.remove(to)
    file.rename(from, to)
}

##' Get figure labels
##'
##' @param x The report object or chapter object
##' @return invisible NULL
##' @export
get_labels <- function(x) UseMethod("get_labels")

##' @export
get_labels.report <- function(x) {
    get_labels(x$chapters)
}

##' @export
get_labels.chapters <- function(x) {
    lapply(x, function(y) get_labels(y))
}

##' @export
get_labels.chapter <- function(x) {
    unlist(lapply(figure_files(x, "tex"), get_label))
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

##' Format the label from a figure path
##'
##' @keywords internal
explain_labeling <- function() {
    paste("\\noindent To reference this figure or table in the word document",
          "you need to insert the following \\textbf{label(s)} into",
          "the text:")
}
