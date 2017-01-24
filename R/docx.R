##' @export
to_docx <- function(x) UseMethod("to_docx")

##' @export
to_docx.report <- function(x) {
    to_docx(x$chapters)
    invisible()
}

##' @export
to_docx.chapters <- function(x) {
    lapply(x, function(y) to_docx(y))
    invisible()
}

##' @export
to_docx.chapter <- function(x) {
    stop("Not implemented")
    invisible()
}
