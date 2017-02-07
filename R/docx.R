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
    from <- paste0(file.path(x$path, x$title), ".docx")
    if (!file.exists(from))
        to_docx(x)

    to <- file.path(to, x$title)
    if (!dir.exists(to))
        dir.create(to, recursive = TRUE)
    to <- paste0(file.path(to, x$title), ".docx")
    file.copy(from, to, overwrite = TRUE)
        
    invisible()
}

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
    filename <- tempfile(pattern = "text-", tmpdir = x$path, fileext = ".tex")
    writeLines(tex, con = filename)
    unlink(file.path(x$path, "text.docx"))
    system(paste0("pandoc ", filename, " -o text.docx"))
    unlink(filename)
    invisible()
}
