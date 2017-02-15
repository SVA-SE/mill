##' Export docx files
##'
##' @param x The object to export.
##' @param to The destination of the export. If the argument is
##'     missing, the docx files are exported to a folder named from
##'     the report title.
##' @return invisible NULL.
##' @export
export_docx <- function(x, to) UseMethod("export_docx")

##' @export
export_docx.report <- function(x, to) {
    if (missing(to))
        to <- x$report
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

##' Convert from docx to tex
##'
##' Use pandoc (http://pandoc.org/) to convert from 'docx' to
##' 'tex'. The chapter 'text.docx' is converted to 'text.tex'. Each
##' chapter 'text.tex' is added, but not commited, to the report git
##' repository.
##' @param x The object to convert.
##' @param repo The git repository to add the 'tex' to.
##' @param ... Additional arguments.
##' @return invisible NULL.
##' @export
from_docx <- function(x, ...) UseMethod("from_docx")

##' @importFrom git2r repository
##' @export
from_docx.report <- function(x, ...) {
    if (length(list(...)) > 0)
        warning("Additional arguments ignored")
    from_docx(x$chapters, repo = git2r::repository(x$path))
}

##' @export
from_docx.chapters <- function(x, repo = NULL, ...) {
    if (length(list(...)) > 0)
        warning("Additional arguments ignored")
    lapply(x, function(y) from_docx(y, repo))
    invisible()
}

##' @importFrom git2r add
##' @export
from_docx.chapter <- function(x, repo = NULL, ...) {
    if (length(list(...)) > 0)
        warning("Additional arguments ignored")
    f_tex <- file.path(x$path, "text.tex")
    unlink(f_tex)
    f_docx <- file.path(x$path, "text.docx")
    pandoc(paste0("\"", f_docx, "\" -o \"", f_tex, "\""))
    if (!is.null(repo))
        git2r::add(repo, f_tex)
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
    to_docx(x$chapters, repo = git2r::repository(x$path))
}

##' @export
to_docx.chapters <- function(x, repo = NULL, ...) {
    if (length(list(...)) > 0)
        warning("Additional arguments ignored")
    lapply(x, function(y) to_docx(y, repo))
    invisible()
}

##' @importFrom git2r add
##' @export
to_docx.chapter <- function(x, repo = NULL, ...) {
    if (length(list(...)) > 0)
        warning("Additional arguments ignored")
    f_tex <- clean_tex(x$path)
    on.exit(unlink(f_tex))
    f_docx <- file.path(x$path, "text.docx")
    unlink(f_docx)
    pandoc(paste0("\"", f_tex, "\" -o \"", f_docx, "\""))
    if (!is.null(repo))
        git2r::add(repo, f_docx)
    invisible()
}
