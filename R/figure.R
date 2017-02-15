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

##' @export
build_figures <- function(x, png) UseMethod("build_figures")

##' @export
build_figures.report <- function(x, png = FALSE) {
    build_figures(x$chapters, png)
}

##' @export
build_figures.chapters <- function(x, png = FALSE) {
    lapply(x, function(y) build_figures(y, png))
    invisible()
}

##' @export
build_figures.chapter <- function(x, png = FALSE) {
    lapply(figure_files(x, "R"), do_build_figure, png)
    invisible()
}

##' @keywords internal
do_build_figure <- function(figure, png) {
    source(figure, local = TRUE, chdir = TRUE)
}

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
    lapply(figure_files(x, "tex"), do_preview_figure)
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
    writeLines(c(readLines(pre_tex), readLines(tex), readLines(post_tex)),
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

##' Run LuaTeX
##'
##' @param texname Run LuaTeX on texname.
##' @keywords internal
luatex <- function(filename) {
    wd <- setwd(dirname(filename))
    onexit(setwd(wd))
    system(paste0("lualatex ", shQuote(basename(filename))))
}

##' @keywords internal
do_preview_figure <- function(figure) {
    ## Create a tex file with the context to create a preview.
    a <- assets(figure)
    pre_tex <- file.path(a, "figure-preview/pre-snippet.tex")
    post_tex <- file.path(a, "figure-preview/post-snippet.tex")
    preview <- embed_tex(figure, pre_tex, post_tex)
    onexit(unlink(paste0(tools::file_path_sans_ext(preview), "*")))

    ## Build the preview pdf file.
    luatex(preview)

    ## Copy the pdf preview to 'preview-figure.pdf'
    from <- paste0(tools::file_path_sans_ext(preview), ".pdf")
    to <- paste0("preview-", tools::file_path_sans_ext(basename(figure)), ".pdf")
    file.copy(from, file.path(dirname(figure), to), overwrite = TRUE)
}
