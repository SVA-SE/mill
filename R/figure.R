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
