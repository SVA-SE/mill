##' Reduce Images
##'
##' @param from filename of the original image
##' @param to filename of the reduced image
##' @return invisible NULL
##' @export
reduce_image <- function(from, to) {
    from <- normalizePath(from, mustWork = TRUE)

    args <- switch(tools::file_ext(from),
                   jpg = c("-resize 50%", "-strip", "-interlace Plane", "-gaussian-blur 0.05", "-quality 70%"),
                   png = c("-resize 300x"),
                   NULL)

    if (tools::file_ext(from) == "png2") {
        ## Just copy file
        file.copy(from, paste0(tools::file_path_sans_ext(to), ".png"))
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
