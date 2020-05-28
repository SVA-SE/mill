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

##' Build figures
##'
##' @param png Convert figure to png?
##' @return invisible NULL
##' @export
build_figures <- function(png = FALSE) {
    if (in_chapter()) {
        chapter <- basename(getwd())
        cat(sprintf("Build figures: %s\n", chapter))

        lapply(figure_files("R"), function(figure) {
            cat(sprintf("  - Run script: %s\n", figure))
            tryCatch(
                source(figure, local = TRUE, chdir = TRUE),
                error = function(e) {
                    cat("   *** ERROR ***\n")
                }
            )
        })

        if (isTRUE(png)) {
            ## Convert figure files to png
            files <- figure_files("pdf")

            if (length(files) && !requireNamespace("magick"))
                stop("Package 'magick' is required to convert pdf-files.")

            lapply(files, function(from) {
                to <- paste0(file_path_sans_ext(from), ".png")
                cat(sprintf("  - Convert: %s -> %s\n", from, to))
                fig <- magick::image_read_pdf(from)
                fig <- magick::image_convert(fig, "png")
                fig <- magick::image_write(fig, to, flatten = TRUE)
            })
        }
    } else if (in_report()) {
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            build_figures(png = png)
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
