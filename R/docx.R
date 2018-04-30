##' Export files to workspace
##'
##' Authors make contributions to chapters in the workspace.
##' @param x The object to export. The docx files are exported to
##'     workspace/chapters/title.
##' @return invisible NULL.
##' @export
export <- function(x) UseMethod("export")

##' @export
export.default <- function(x) {
    export(load_report())
}

##' @export
export.report <- function(x) {
    lapply(x$chapters, function(y) export(y))
    invisible()
}

##' @export
export.chapter <- function(x) {
    to <- file.path("workspace", "chapters", x$title)
    if (!dir.exists(to))
        dir.create(to, recursive = TRUE)

    ## Export text.docx to renamed title.docx
    from <- file.path(x$path, "text.docx")
    if (!file.exists(from))
        to_docx(x)
    file.copy(from, paste0(file.path(to, x$title), ".docx"), overwrite = TRUE)

    ## Export data and preview files
    lapply(c(figure_files(x, "xlsx"), preview_files(x)), function(from) {
        file.copy(from, file.path(to, basename(from)), overwrite = TRUE)
    })

    invisible()
}

##' Import files
##'
##' @param x The object to import.
##' @param from The source of the import. If the argument is missing,
##'     the docx files are imported from a folder named from the
##'     report title.
##' @return invisible NULL.
##' @export
import <- function(x, from) UseMethod("import")

##' @export
import.default <- function(x, from) {
    import(load_report())
}

##' @export
import.report <- function(x, from) {
    if (missing(from))
        from <- x$report
    from <- file.path(from, "chapters")
    lapply(x$chapters, function(y) import(y, from = from))
    invisible()
}

##' @export
import.chapter <- function(x, from) {
    from <- file.path(from, x$title)
    if (!dir.exists(from))
        stop("Invalid directory")

    ## Import title.docx to text.docx
    from <- paste0(file.path(from, x$title), ".docx")
    to <- file.path(x$path, "text.docx")
    file.copy(from, to, overwrite = TRUE)

    invisible()
}

##' Convert from docx to tex
##'
##' Use pandoc (http://pandoc.org/) to convert from 'docx' to
##' 'tex'. The chapter 'text.docx' is converted to 'text.tex'. Each
##' chapter 'text.tex' is added, but not commited, to the report git
##' repository.
##' @param x The report object to convert.
##' @param ... Additional arguments.
##' @return invisible NULL.
##' @export
from_docx <- function(x, ...) UseMethod("from_docx")

##' @importFrom git2r repository
##' @export
from_docx.report <- function(x, ...) {
    if (length(list(...)) > 0)
        warning("Additional arguments ignored")

    repo <- repository(x$path)
    lapply(x$chapters, function(y) from_docx(y, repo = repo))
    invisible()
}

##' @importFrom git2r add
##' @export
from_docx.chapter <- function(x, repo = NULL, ...) {
    if (length(list(...)) > 0)
        warning("Additional arguments ignored")

    ## Convert the docx to a temporary tex file.
    f_tex <- tempfile(fileext = ".tex")
    on.exit(file.remove(f_tex))
    f_docx <- file.path(x$path, "text.docx")
    pandoc(paste("--top-level-division=chapter ",
                 shQuote(f_docx), "-o", shQuote(f_tex)))

    ## Tweak incoming tex file
    tex <- readLines(f_tex)
    tex <- convert_docx_ref_to_ref(tex, x$title)
    tex <- make_labels_chapter_specific(tex, x$title)
    tex <- asterisk(tex, "add")
    writeLines(tex, file.path(x$path, "text.tex"))

    if (!is.null(repo))
        add(repo, file.path(x$path, "text.tex"))
    invisible()
}

normalize_title <- function(title) {
    gsub("[[:space:]]+", "-", tolower(title))
}

##' Convert the docx references to tex ref
##'
##' @param tex The tex character vector
##' @param title The chapter title
##' @return tex character vector
##' @keywords internal
convert_docx_ref_to_ref <- function(tex, title) {
    title <- normalize_title(title)
    pattern <- "[{][[][}]([^:]*)[:]([^{]*)[{}[]][}]"
    replacement <- paste0("\\\\ref{\\1:", title, ":\\2}")
    gsub(pattern, replacement, tex)
}

##' Convert the tex ref to docx ref
##'
##' @param tex The tex character vector
##' @return tex character vector
##' @keywords internal
convert_ref_to_docx_ref <- function(tex) {
    pattern <- "\\\\ref[{]([^:]*)[:][^:]*[:]([^}]*)[}]"
    replacement <- "[\\1:\\2]"
    gsub(pattern, replacement, tex)
}

##' Make tex labels chapter specific in the report
##'
##' @param tex The tex character vector
##' @param title The chapter title
##' @return tex character vector
##' @keywords internal
make_labels_chapter_specific <- function(tex, title) {
    title <- normalize_title(title)
    pattern <- "[\\]label[{]([^}]*)[}]"
    replacement <- paste0("\\\\label{sec:", title, ":", "\\1}")
    gsub(pattern, replacement, tex)
}

##' Add asterisk to sections
##'
##' @param tex The tex character vector
##' @return tex character vector
##' @keywords internal
asterisk <- function(tex, direction = c("add", "remove")) {
    replacement <- switch(match.arg(direction),
                          add = "\\1*\\3",
                          remove = "\\1\\3"
                          )
    patterns <- c("(\\\\chapter)([\\*]?)(\\{)",
                  "(\\\\section)([\\*]?)(\\{)",
                  "(\\\\subsection)([\\*]?)(\\{)",
                  "(\\\\subsubsection)([\\*]?)(\\{)",
                  "(\\\\paragraph)([\\*]?)(\\{)")
    for (i in 1:5) {
            tex <- gsub(patterns[i], replacement, tex)
        }
    return(tex)
}

##' Convert from tex to docx
##'
##' Use pandoc (http://pandoc.org/) to convert from 'tex' to
##' 'docx'. The chapter 'text.tex' is converted to 'text.docx'. Each
##' chapter 'text.docx' is added, but not commited, to the report git
##' repository.
##' @param x The report object to convert.
##' @param ... Additional arguments.
##' @return invisible NULL.
##' @export
to_docx <- function(x, ...) UseMethod("to_docx")

##' @importFrom git2r repository
##' @export
to_docx.report <- function(x, ...) {
    if (length(list(...)) > 0)
        warning("Additional arguments ignored")

    repo <- repository(x$path)
    lapply(x$chapters, function(y) to_docx(y, repo = repo))
    invisible()
}

##' @importFrom git2r add
##' @export
to_docx.chapter <- function(x, repo = NULL, ...) {
    if (length(list(...)) > 0)
        warning("Additional arguments ignored")
    f_tex <- file.path(x$path, "text.tex")
    tex <- readLines(f_tex)

    ## Clean up changes made in from_docx_chapter()
    tex <- asterisk(tex, "remove")
    tex <- convert_ref_to_docx_ref(tex)
    f_tex <- tempfile(fileext = ".tex")
    writeLines(tex, f_tex)
    f_docx <- file.path(x$path, "text.docx")
    unlink(f_docx)

    ## Convert to docx
    pandoc(paste("--top-level-division=chapter ",
                 shQuote(f_tex), "-o", shQuote(f_docx)))
    if (!is.null(repo))
        add(repo, f_docx)
    invisible()
}

##' Roundtrip tex to docx
##'
##' @param x The object to convert.
##' @param ... Additional arguments.
##' @return invisible NULL.
##' @export
roundtrip <- function(x, ...) UseMethod("roundtrip")

##' @importFrom git2r repository
##' @export
roundtrip.report <- function(x, ...) {
    if (length(list(...)) > 0)
        warning("Additional arguments ignored")
    repo <- repository(x$path)
    lapply(x$chapters, function(y) roundtrip(y, repo = repo))
    invisible()
}

##' @importFrom git2r diff
##' @importFrom git2r repository
##' @importFrom git2r reset
##' @importFrom git2r status
##' @export
roundtrip.chapter <- function(x, repo = NULL, ...) {
    if (length(list(...)) > 0)
        warning("Additional arguments ignored")

    if (is.null(repo))
        repo <- repository()

    ## Check if the working tree is clean
    d <- diff(repo)
    if (length(d@files))
        stop("Working tree is not clean")

    to_docx(x, repo = NULL)
    from_docx(x, repo = NULL)

    ## The roundtrip is clean if the tex-file is unchanged
    unstaged <- unlist(status(repo)$unstaged)
    if (!(file.path("chapters", x$title, "text.tex") %in% unstaged))
        reset(commits(repo, n = 1)[[1]], "hard")

    invisible()
}
