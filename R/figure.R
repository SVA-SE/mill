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


###########################################################33

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

##' @keywords internal
do_preview_figure <- function(figure) {
    fig_dir <- dirname(figure)
    fig_name <- paste0("preview-", tools::file_path_sans_ext(basename(figure)), ".pdf")
    filename <- tempfile(tmpdir = fig_dir, fileext = ".tex")
    assets <- file.path(dirname(dirname(fig_dir)), "assets")
    tex <- c(readLines(file.path(assets, "figure-preview/snippet1.tex")),
             readLines(figure),
             readLines(file.path(assets, "figure-preview/snippet2.tex")))
    writeLines(tex, filename)
    system(paste("cd ", gsub(" ", "\\\\ ", fig_dir), ";lualatex ", basename(filename)))
    file.copy(paste0(tools::file_path_sans_ext(filename), ".pdf"),
              file.path(fig_dir, fig_name), overwrite = TRUE)
    unlink(paste0(tools::file_path_sans_ext(filename), "*"))
}
