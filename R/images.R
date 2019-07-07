##' Reduce Images
##'
##' Reduce the resolution of images.
##'
##' @param from filename of the original image
##' @param to filename of the reduced image
##' @return invisible NULL
##' @export
reduce_image <- function(from, to) {
    from <- normalizePath(from, mustWork = TRUE)
    fromdir <- dirname(from)
    frombase <- basename(from)
    fromweb <- file.path(fromdir, paste0("web_", frombase))
    if (file.exists(fromweb)) {
        file.copy(fromweb, to)
        return(invisible(NULL))
    } else {
        file.copy(from, to)
        return(invisible(NULL))
    }
}

##' Compare Images
##'
##' Submit an a reference image and a new image and get back the
##' percent difference in all colour channels.
##'
##' Requires imagemagick
##' Requires pdftk
##'
##' @title image_diff
##' @param reference Path to a reference image
##' @param new Path to an image to compare to the reference image
##' @param dir directory to perform the comparison inside
##' @return numeric The percent difference between the two images
##' @importFrom tools file_ext
image_diff <- function(reference, new, dir) {
    if(is.null(dir)){
        dir <- tempdir()
    }
    if(system2("convert", "-version", stdout = FALSE) != 0) {
        stop("In order to use this tool you need to install 'imagemagick'")
    }
    fileext1 <- file_ext(reference)
    fileext2 <- file_ext(new)
    stopifnot(fileext1 == fileext2)
    if(fileext1 == "pdf") {
        stopifnot(pdf_np(reference) == 1)
        stopifnot(pdf_np(new) == 1)
    }
    output <- tempfile(tmpdir = dir, fileext = paste0(".", fileext1))
    reference <- normalizePath(reference, mustWork = TRUE)
    new <- normalizePath(new, mustWork = TRUE)
    args <- c("-verbose", "-metric MAE")
    a <- system2("compare",
                 args = c(args, shQuote(new), shQuote(reference), shQuote(output)),
                 stdout = TRUE,
                 stderr = TRUE)
    index <- grep("all: ", a)
    list(as.numeric(gsub("\\(", "", regmatches(a[index], regexpr("\\([^\\)]*", a[index])))),
         output)
}

##' Number of pages in a pdf
##'
##' Get the number of pages of a pdffile
##'
##' Requires pdftk
##'
##' @title pdf_np
##' @param path the path to the pdf file
##' @return numeric The number of pages
pdf_np <- function(path) {
    if(system2("pdftk", "-version", stdout = FALSE) != 0) {
        stop("In order to use this tool you need to install 'pdftk'")
    }
    path <- normalizePath(path, mustWork = TRUE)
    args <- "dump_data"
    a <- system2("pdftk", args = c(path, args),
                 stdout = TRUE,
                 stderr = TRUE)
    index <- grep("NumberOfPages", a)
    as.numeric(gsub(":", "", regmatches(a[index], regexpr(":[^$]*", a[index]))))
}

##' Split up a pdf
##'
##' Split a pdf into it's component pages and return a list of
##' filenames.
##'
##' @title pdf_split
##' @param path The path to the pdffile
##' @param dir directory to perform the comparison inside
##' @return list A list of the filenames of the resultant 1 page pdfs
pdf_split <- function(path, dir) {
    if(is.null(dir)){
        dir <- tempdir()
    }
    if(system2("pdftk", "-version", stdout = FALSE) != 0) {
        stop("In order to use this tool you need to install 'pdftk'")
    }
    path <- normalizePath(path, mustWork = TRUE)
    np <- pdf_np(path)
    if (requireNamespace("progress", quietly = TRUE)) {
        pb <- progress::progress_bar$new(total = np)
    }
    files <- lapply(seq_len(np), function(i) {
        outfile <- tempfile(tmpdir = dir, fileext = ".pdf")
        arg1 <- paste("cat", i)
        arg2 <- paste("output", outfile)
        system2("pdftk", args = c(path, arg1, arg2),
                stdout = TRUE,
                stderr = TRUE)
        if (requireNamespace("progress", quietly = TRUE)) {
            pb$tick()
        }
        outfile
    })
    unlist(files)
}

##' Difference of two pdf:s
##'
##' Submitt two pdfs of the same number of pages to this function and
##' get back a data.frame with the same number of rows as the pdf with
##' the percent pixel difference for each page and a path to a
##' composed image of each page highlighting the differences in red.
##'
##' @title pdf_diff
##' @param reference Path to a pdf file
##' @param new Path to a pdf file
##' @param dir directory to perform the comparison inside
##' @return data.frame A data.frame with 3 columns: page,
##'     percent_diff, composite.
##' @export
pdf_diff <- function(reference, new, dir = NULL) {
    if(is.null(dir)){
        dir <- tempdir()
    }
    reference <- normalizePath(reference, mustWork = TRUE)
    new <- normalizePath(new, mustWork = TRUE)
    stopifnot(pdf_np(reference) == pdf_np(new))
    np <- pdf_np(reference)
    if (requireNamespace("progress", quietly = TRUE)) {
        pb_outer <- progress::progress_bar$new(total = np)
    } else {
        message("If you want a nice progress bar for this function, install the \"progress\" package.")
    }
    message("Splitting the \"reference\" pdf into individual pages")
    pages_ref <- pdf_split(reference, dir)
    message("Splitting the \"new\" PDF into individual pages")
    pages_new <- pdf_split(new, dir)
    message("Comparing individual pages")
    df <- do.call("rbind", lapply(seq_len(np), function(i) {
        id <- image_diff(pages_ref[i], pages_new[i], dir)
        if (requireNamespace("progress", quietly = TRUE)) {
            pb_outer$tick()
        }
        data.frame(page = i,
                   percent_diff = id[[1]],
                   composite = id[[2]],
                   original = pages_ref[i],
                   modified = pages_new[i],
                   stringsAsFactors = FALSE)
    }))
    class(df) <- c("pdf_diff_df", class(df))
    df
}

plot.pdf_diff_df <- function(df) {
    plot(df$page,
         df$percent_diff,
         pch = 20,
         col = "grey40",
         type = "p",
         ylab = "Percent difference of page pixels",
         xlab = "Page number")
    text(df$page, df$percent_diff, df$page, pos = 4)
}

}
