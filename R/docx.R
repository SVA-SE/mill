##' Export files to workspace
##'
##' Authors make contributions to chapters in the workspace. The docx
##' files are exported to workspace/chapters/title.
##' @return invisible NULL.
##' @export
export <- function() {
    if (in_chapter()) {
        chapter <- basename(getwd())

        to <- paste0("../../workspace/chapters/", chapter)
        if (!dir.exists(to))
            dir.create(to, recursive = TRUE)

        ## Export text.docx to renamed title.docx
        from <- "text.docx"
        if (!file.exists(from))
            to_docx()
        file.copy(from, paste0(to, "/", chapter, ".docx"), overwrite = TRUE)

        ## Export data and preview files
        lapply(c(figure_files("xlsx"), preview_files()), function(from) {
            file.copy(from, file.path(to, basename(from)), overwrite = TRUE)
        })
    } else if (in_report()) {
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            export()
            setwd(wd)
        })
    }

    invisible()
}

##' Check if current working directory is in a chapter
##' @noRd
in_chapter <- function() {
    if (!file.exists("README.org")) {
        if (identical(basename(dirname(getwd())), "chapters"))
            return(file.exists("../../README.org"))
    }
    FALSE
}

##' Check if current working directory is in a report
##' @noRd
in_report <- function() {
    file.exists("README.org")
}

##' Import chapter docx file from workspace
##'
##' @return invisible NULL.
##' @export
import <- function() {
    if (in_chapter()) {
        chapter <- basename(getwd())
        from <- paste0("../../workspace/chapters/", chapter)
        if (!dir.exists(from))
            stop("Invalid directory")

        ## Import title.docx to text.docx
        from <- paste0(from, "/", chapter, ".docx")
        to <- "text.docx"
        file.copy(from, to, overwrite = TRUE)
    } else if (in_report()) {
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            import()
            setwd(wd)
        })
    }

    invisible()
}

##' Convert from docx to tex
##'
##' Use pandoc (http://pandoc.org/) to convert from 'docx' to
##' 'tex'. The chapter 'text.docx' is converted to 'text.tex'. Each
##' chapter 'text.tex' is added, but not commited, to the report git
##' repository.
##' @param repo the report git repository.
##' @return invisible NULL.
##' @importFrom git2r add
##' @importFrom git2r repository
##' @export
from_docx <- function(repo = NULL) {
    if (in_chapter()) {
        chapter <- basename(getwd())

        ## Convert the docx to a temporary tex file.
        f_tex <- tempfile(fileext = ".tex")
        f_docx <- "text.docx"
        pandoc(paste("--top-level-division=chapter ",
                     shQuote(f_docx), "-o", shQuote(f_tex)))

        ## Tweak incoming tex file
        tex <- readLines(f_tex)
        file.remove(f_tex)
        tex <- convert_docx_ref_to_ref(tex, chapter)
        tex <- make_labels_chapter_specific(tex, chapter)
        tex <- make_hypertargets_chapter_specific(tex, chapter)
        tex <- asterisk(tex, "add")
        writeLines(tex, "text.tex")
        if (!is.null(repo))
            add(repo, paste0("chapters/", chapter, "/text.tex"))
    } else if (in_report()) {
        repo <- repository()
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            from_docx(repo = repo)
            setwd(wd)
        })
    }

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
##' @noRd
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
##' @noRd
convert_ref_to_docx_ref <- function(tex) {
    pattern <- "\\\\ref[{]([^:]*)[:][^:]*[:]([^}]*)[}]"
    replacement <- "[\\1:\\2]"
    gsub(pattern, replacement, tex)
}

##' Make tex hypertargets chapter specific in the report
##'
##' @param tex The tex character vector
##' @param title The chapter title
##' @return tex character vector
##' @noRd
make_hypertargets_chapter_specific <- function(tex, title) {
    title <- normalize_title(title)
    pattern <- "[\\]hypertarget[{]([^}]*)[}]"
    replacement <- paste0("\\\\hypertarget{sec:", title, ":", "\\1}")
    gsub(pattern, replacement, tex)
}

##' Make tex labels chapter specific in the report
##'
##' @param tex The tex character vector
##' @param title The chapter title
##' @return tex character vector
##' @noRd
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
##' @noRd
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
##' @param repo the report git repository.
##' @return invisible NULL.
##' @export
to_docx <- function(repo = NULL) {
    if (in_chapter()) {
        ## Clean up changes made in from_docx_chapter()
        tex <- readLines("text.tex")
        tex <- asterisk(tex, "remove")
        tex <- convert_ref_to_docx_ref(tex)
        f_tex <- tempfile(fileext = ".tex")
        writeLines(tex, f_tex)
        f_docx <- "text.docx"
        unlink(f_docx)

        ## Convert to docx
        pandoc(paste("--top-level-division=chapter ",
                     shQuote(f_tex), "-o", shQuote(f_docx)))
        if (!is.null(repo))
            add(repo, paste0("chapters/", basename(getwd()), "/text.docx"))
    } else if (in_report()) {
        repo <- repository()
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            to_docx(repo = repo)
            setwd(wd)
        })
    }

    invisible()
}

##' Roundtrip tex to docx
##'
##' @return invisible NULL.
##' @importFrom git2r repository
##' @importFrom git2r reset
##' @importFrom git2r status
##' @export
roundtrip <- function() {
    if (in_chapter()) {
        ## Check if the working tree is clean
        repo <- repository("../..")
        s <- status(repo)
        if (length(c(s$staged, s$unstaged)))
            stop("Working tree is not clean")

        to_docx(repo = NULL)
        from_docx(repo = NULL)

        ## The roundtrip is clean if the tex-file is unchanged
        unstaged <- unlist(status(repo)$unstaged)
        if (!(paste0("chapters/", basename(getwd()), "/text.tex") %in% unstaged))
            reset(commits(repo, n = 1)[[1]], "hard")
    } else if (in_report()) {
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            roundtrip()
            setwd(wd)
        })
    }

    invisible()
}
