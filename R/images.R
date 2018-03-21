##' Reduce Images
##'
##' Reduce the resolution of images. Requires imagemagick.
##'
##' @param from filename of the original image
##' @param to filename of the reduced image
##' @importFrom tools file_ext
##' @importFrom tools file_path_sans_ext
##' @return invisible NULL
##' @export
reduce_image <- function(from, to) {
    from <- normalizePath(from, mustWork = TRUE)

    args <- switch(file_ext(from),
                   jpg = c("-resize 50%", "-strip", "-interlace Plane", "-gaussian-blur 0.05", "-quality 70%"),
                   png = c("-resize 300x"),
                   NULL)

    if (file_ext(from) == "png2") {
        ## Just copy file
        file.copy(from, paste0(file_path_sans_ext(to), ".png"))
        return(invisible(NULL))
    }

    if (is.null(args)) {
        ## Just copy file
        file.copy(from, to)
        return(invisible(NULL))
    }

    system2("convert",
            args = c(shQuote(from), args, shQuote(to)),
            stdout = TRUE,
            stderr = TRUE)

    invisible()
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
##' @return numeric The percent difference between the two images
##' @importFrom tools file_ext
image_diff <- function(reference, new) {
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
    output <- tempfile(fileext = paste0(".", fileext1))
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
##' @return list A list of the filenames of the resultant 1 page pdfs
pdf_split <- function(path) {
    if(system2("pdftk", "-version", stdout = FALSE) != 0) {
            stop("In order to use this tool you need to install 'pdftk'")
    }
    path <- normalizePath(path, mustWork = TRUE)
    np <- pdf_np(path)
    files <- lapply(seq_len(np), function(i) {
        outfile <- tempfile(fileext = ".pdf")
        arg1 <- paste("cat", i)
        arg2 <- paste("output", outfile)
        system2("pdftk", args = c(path, arg1, arg2),
                stdout = TRUE,
                stderr = TRUE)
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
##' @return data.frame A data.frame with 3 columns: page,
##'     percent_diff, composite.
##' @export
pdf_diff <- function(reference, new) {
    reference <- normalizePath(reference, mustWork = TRUE)
    new <- normalizePath(new, mustWork = TRUE)
    stopifnot(pdf_np(reference) == pdf_np(new))
    np <- pdf_np(reference)
    pages_ref <- pdf_split(reference)
    pages_new <- pdf_split(new)
    do.call("rbind", lapply(seq_len(np), function(i) {
        id <- image_diff(pages_ref[i], pages_new[i])
        data.frame(page = i,
                   percent_diff = id[[1]],
                   composite = id[[2]],
                   stringsAsFactors = FALSE)
    }))
}
