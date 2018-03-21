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
##' percent difference in all colour channels. Optionally you can also
##' save the output comparison image that highlights the differences
##' between them.
##'
##' Requires imagemagick
##'
##' @title image_diff
##' @param reference Path to a reference image
##' @param new Path to an image to compare to the reference image
##' @param output Optional path to the output image
##' @return numeric The percent difference between the two images
##' @export
image_diff <- function(reference, new, output = tempfile()) {
    reference <- normalizePath(reference, mustWork = TRUE)
    new <- normalizePath(new, mustWork = TRUE)
    args <- c("-verbose", "-metric MAE")
    a <- system2("compare",
                 args = c(args, shQuote(new), shQuote(reference), shQuote(output)),
                 stdout = TRUE,
                 stderr = TRUE)
    index <- grep("all: ", a)
    as.numeric(gsub("\\(", "", regmatches(a[index], regexpr("\\([^\\)]*", a[index]))))
}
