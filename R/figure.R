##' @noRd
figure_pattern <- function(ext = c("all", "R", "tex", "xlsx", "pdf", "jpg")) {
    ext <- switch(match.arg(ext),
                      all  = "((tex)|(R)|(xlsx)|(pdf)|(jpg)|(png)|(eps))$",
                      R    = "R$",
                      xlsx = "xlsx$",
                      tex  = "tex$",
                      png  = "png$",
                      pdf  = "pdf$")

    paste0("^fig_[^.]+[.]", ext)
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

##' @noRd
preview_pattern <- function(items = c("all", "figure", "table")) {
    items <- switch(match.arg(items),
                    all    = "((tab)|(fig))",
                    figure = "fig",
                    table  = "tab")

    paste0("^preview-", items, "_[^.]*[.]pdf$")
}

##' @noRd
preview_files <- function(items = "all") {
    stopifnot(in_chapter())
    list.files(pattern = preview_pattern(items))
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

        ## Convert figure files to png
        lapply(figure_files("pdf"), function(from) {
            to <- paste0(file_path_sans_ext(from), ".png")
            system(paste("convert", from, "-flatten", to))
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

##' Get the label from a figure path
##'
##' @noRd
get_label <- function(figure, format = c("latex", "word")) {
    format <- match.arg(format)
    tex <- readLines(figure)
    m <- regmatches(tex, regexec("[\\]label[{]([^}]*)", tex))
    labels <- unlist(lapply(m, function(x) {
        if (length(x))
            return(x[2])
        x
    }))
    format_labels(labels, format)
}

##' Format the label from a figure path
##'
##' @noRd
format_labels <- function(labels, format = c("latex", "word")) {
    format <- match.arg(format)
    if (identical(format, "word")) {
        labels <- gsub("([^:]*)[:][^:]*[:]([^:]*)", "{[}\\1:\\2{]}", labels)
        return(labels)
    }
    return(labels)
}
