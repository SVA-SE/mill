##' @noRd
table_pattern <- function(fileext = c("all", "tex", "pdf")) {
    fileext <- switch(match.arg(fileext),
                      all = "((tex)|(pdf))$",
                      tex = "tex$",
                      pdf = "pdf$")

    paste0("^tab_[^.]+[.]", fileext)
}

##' @noRd
table_files <- function(fileext = "all") {
    if (in_chapter())
        return(list.files(pattern = table_pattern(fileext)))
    stop("Not implemented")
}

##' Preview tables
##'
##' @return invisible NULL
##' @export
preview_tables <- function() {
    if (in_chapter()) {
        lapply(table_files("tex"), preview_table)
    } else if (in_report()) {
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            preview_tables()
            setwd(wd)
        })
    }

    invisible(NULL)
}

##' Preview a table
##'
##' @param table The path to the table tex file
##' @importFrom tools file_path_sans_ext
##' @export
preview_table <- function(table) {
    preview <- tempfile(tmpdir = dirname(table), fileext = ".tex")
    on.exit(unlink(preview))

    ## Read in the pieces of the table
    text <- readLines(table)
    presnippet <- readLines("../../assets/latex/pre-snippet.tex")

    ## Create a tex file with the context to create a preview.
    tex <- c(presnippet,
             "\\captionsetup{labelformat = empty}",
             "\\begin{document}",
             "\\begin{LARGE}",
             explain_labeling(),
             "\\newline\\newline",
             "\\hl{",
             get_label(table, "word"),
             "}",
             "\\end{LARGE}",
             text,
             "\\end{document}")
    writeLines(tex, preview)

    ## Build the preview pdf file.
    luatex(preview)

    ## Copy the pdf preview to 'preview-figure.pdf'
    from <- paste0(file_path_sans_ext(preview), ".pdf")
    to <- file.path(dirname(table),
                    paste0("preview-", file_path_sans_ext(basename(table)), ".pdf"))
    if (file.exists(to))
        file.remove(to)
    file.rename(from, to)
    for (ext in c(".log", ".aux", ".out")) {
        file <- paste0(file_path_sans_ext(preview), ext)
        if (file.exists(file))
            file.remove(file)
    }

    invisible(NULL)
}
