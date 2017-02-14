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
    lapply(figure_files(x, "R"), do_build_figure)
    invisible()
}

##' @keywords internal
do_build_figure <- function(figure) {
    source(figure, local = TRUE, chdir = TRUE)
}
