##' @export
export_docx <- function(x, to) UseMethod("export_docx")

##' @export
export_docx.report <- function(x, to) {
    export_docx(x$chapters, to)
    invisible()
}

##' @export
export_docx.chapters <- function(x, to) {
    lapply(x, function(y) export_docx(y, file.path(to, "chapters")))
    invisible()
}

##' @export
export_docx.chapter <- function(x, to) {
    from <- file.path(x$path, "text.docx")
    if (!file.exists(from))
        to_docx(x)

    to <- file.path(to, x$title)
    if (!dir.exists(to))
        dir.create(to, recursive = TRUE)
    to <- paste0(file.path(to, x$title), ".docx")
    file.copy(from, to, overwrite = TRUE)

    invisible()
}

##' Convert from tex to docx
##'
##' Use pandoc (http://pandoc.org/) to convert from 'tex' to
##' 'docx'. The chapter 'text.tex' is converted to 'text.docx'.
##' @param x The object to convert.
##' @return invisible NULL.
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
    tex <- clean_tex(readLines(file.path(x$path, "text.tex")))
    f_tex <- tempfile(pattern = "text-", tmpdir = x$path, fileext = ".tex")
    writeLines(tex, con = f_tex)
    f_docx <- file.path(x$path, "text.docx")
    unlink(f_docx)
    system(paste0("pandoc \"", f_tex, "\" -o \"", f_docx, "\""))
    unlink(f_tex)
    invisible()
}
