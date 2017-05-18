##' Reduce Images
##'
##' @param from filename of the original image
##' @param to filename of the reduced image
##' @return invisible NULL
##' @export
reduce_image <- function(from, to) {
    from <- normalizePath(from, mustWork = TRUE)

    args <- switch(tools::file_ext(from),
                   jpg = c("-resize 1000x", "-compress JPEG", "-quality 50"),
                   png = "-resize 75%",
                   NULL)

    if (is.null(args))
        stop("Unsupported image format")

    system2("convert",
            args = c(shQuote(from), args, to),
            stdout = TRUE,
            stderr = TRUE)

    invisible()
}
