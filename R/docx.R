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
##' 'docx'. The chapter 'text.tex' is converted to 'text.docx'. Each
##' chapter 'text.docx' is added, but not commited, to the report git
##' repository.
##' @param x The object to convert.
##' @param repo The git repository to add the 'docx' to.
##' @param ... Additional arguments.
##' @return invisible NULL.
##' @export
to_docx <- function(x, ...) UseMethod("to_docx")

##' @importFrom git2r repository
##' @export
to_docx.report <- function(x, ...) {
    if (length(list(...)) > 0)
        warning("Additional arguments ignored")
    repo <- git2r::repository(x$path)
    to_docx(x$chapters, repo)
    invisible()
}

##' @export
to_docx.chapters <- function(x, repo, ...) {
    if (length(list(...)) > 0)
        warning("Additional arguments ignored")
    lapply(x, function(y) to_docx(y, repo))
    invisible()
}

##' @importFrom git2r add
##' @export
to_docx.chapter <- function(x, repo, ...) {
    if (length(list(...)) > 0)
        warning("Additional arguments ignored")
    tex <- clean_tex(readLines(file.path(x$path, "text.tex")))
    f_tex <- tempfile(pattern = "text-", tmpdir = x$path, fileext = ".tex")
    writeLines(tex, con = f_tex)
    f_docx <- file.path(x$path, "text.docx")
    unlink(f_docx)
    system(paste0("pandoc \"", f_tex, "\" -o \"", f_docx, "\""))
    git2r::add(repo, f_docx)
    unlink(f_tex)
    invisible()
}
