##' Reduce Images
##'
##' @param path The original
##' @return A path to the reduced image
##' @export
reduce_image <- function(path) {
    ext <- tools::file_ext(path)
    stopifnot(ext %in% c("jpg", "png"))
    reduced <- tempfile(fileext = paste0(".", ext))
    path <- normalizePath(path)

    args <- switch(ext,
                   jpg = c("-resize 1000x", "-compress JPEG", "-quality 50"),
                   png = "-resize 75%")

    system2("convert",
            args = c(shQuote(path), args, reduced),
            stdout = TRUE,
            stderr = TRUE)

    reduced
}
