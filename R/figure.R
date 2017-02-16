##' @keywords internal
figure_pattern <- function(fileext = c("all", "R", "tex", "csv", "pdf")) {
    fileext <- switch(match.arg(fileext),
                      all = "(tex)|(R)|(csv)|(pdf)$",
                      R   = "R$",
                      csv = "csv$",
                      tex = "tex$",
                      pdf = "pdf$")

    paste0("^figure-[^.]+[.]", fileext)
}

##' @keywords internal
figure_files <- function(x, fileext) UseMethod("figure_files")

figure_files.chapter <- function(x, fileext = "all") {
    list.files(path = x$path,
               pattern = figure_pattern(fileext),
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

##' Embed a tex file in a context
##'
##' @param tex The tex file to embed.
##' @param pre_tex A tex snippet to add before the tex file.
##' @param post_tex A tex snippet to add after the tex file.
##' @return The filename to the temporary file with the embeded tex.
##' @keywords internal
embed_tex <- function(tex, pre_tex, post_tex) {
    filename <- tempfile(tmpdir = dirname(tex), fileext = ".tex")
    writeLines(c(readLines(pre_tex),
                 get_label(tex, "word"),
                 readLines(tex),
                 readLines(post_tex)),
               filename)
    filename
}

##' Get the assets directory
##'
##' Determine the assets directory given a chapter file in the report
##' project.
##' @param filename The filename.
##' @return path to the assets directory.
##' @keywords internal
assets <- function(filename) {
    file.path(dirname(dirname(dirname(filename))), "assets")
}

##' Preview a figure
##'
##' @param figure The path to the figure tex file
##' @export
preview_figure <- function(figure) {
    ## Create a tex file with the context to create a preview.
    a <- assets(figure)
    pre_tex <- file.path(a, "figure-preview/pre-snippet.tex")
    post_tex <- file.path(a, "figure-preview/post-snippet.tex")
    preview <- embed_tex(figure, pre_tex, post_tex)
    on.exit(unlink(preview))

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
        labels <- unlist(lapply(labels, function(label) {
            a <- strsplit(label, ":")[[1]]
            paste(a[1], a[length(a)], sep = ":")
        }))
        return(labels)
    }
    return(labels)
}
