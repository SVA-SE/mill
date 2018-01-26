##' @keywords internal
table_pattern <- function(fileext = c("all", "tex", "pdf")) {
    fileext <- switch(match.arg(fileext),
                      all = "(tex)|(pdf)$",
                      tex = "tex$",
                      pdf = "pdf$")

    paste0("^table-[^.]+[.]", fileext)
}

##' @keywords internal
table_files <- function(x, fileext) UseMethod("table_files")

table_files.chapter <- function(x, fileext = "all") {
    list.files(path = x$path,
               pattern = table_pattern(fileext),
               full.names = TRUE)
}

##' Preview tables
##'
##' @param x The report object or chapter object
##' @return invisible NULL
##' @export
preview_tables <- function(x) UseMethod("preview_tables")

##' @export
preview_tables.report <- function(x) {
    preview_tables(x$chapters)
}

##' @export
preview_tables.chapters <- function(x) {
    lapply(x, function(y) preview_tables(y))
    invisible()
}

##' @export
preview_tables.chapter <- function(x) {
    lapply(table_files(x, "tex"), preview_table)
    invisible()
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
    a <- assets(table)
    text <- readLines(table)
    presnippet <- readLines(file.path(a, "latex/pre-snippet.tex"))
    presnippet <- gsub("assets/", paste0(a, "/"), presnippet)

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
}
