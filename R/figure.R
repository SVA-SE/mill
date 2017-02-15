##' @keywords internal
figure_pattern <- function(fileext = c("all", "R", "tex", "csv")) {
    switch(match.arg(fileext),
           all = "^figure-[^.]+[.](tex)|(R)|(csv)$",
           R = "^figure-[^.]+[.]R$",
           csv = "^figure-[^.]+[.]csv$",
           tex = "^figure-[^.]+[.]tex$")
}

##' @keywords internal
figure_files <- function(x, fileext = "all") UseMethod("figure_files")

figure_files.chapter <- function(x, fileext) {
    list.files(path = x$path,
               pattern = figure_pattern(fileext),
               full.names = TRUE)
}

##' @export
build_figures <- function(x, png) UseMethod("build_figures", ...)

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
