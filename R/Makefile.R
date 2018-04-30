##' Create a Makefile in each chapter of the report
##'
##' @param x The report object
##' @param ... Additional arguments.
##' @return invisible NULL
##' @export
create_Makefile <- function(x, ...) UseMethod("create_Makefile")

##' @importFrom git2r repository
##' @importFrom git2r add
##' @export
create_Makefile.report <- function(x, ...) {
    repo <- repository(x$path)

    lines <- c("all:",
               "\tRscript -e 'mill::to_pdf()'",
               "",
               "web:",
               "\tRscript -e \"mill::to_pdf(mill::load_report(), type = 'web')\"",
               "",
               "check:",
               "\tRscript -e \"mill::check()\"",
               "",
               "export:",
               "\tRscript -e \"mill::export()\"",
               "",
               "roundtrip:",
               "\tRscript -e \"mill::round_trip(mill::load_report())\"",
               "",
               ".PHONY: all check export roundtrip web")

    writeLines(lines, file.path(x$path, "Makefile"))

    if (!is.null(repo))
        add(repo, file.path(x$path, "Makefile"))

    lapply(x$chapters, function(y) create_Makefile(y, repo = repo))
    invisible()
}

##' @importFrom git2r add
##' @export
create_Makefile.chapter <- function(x, repo, ...) {
    prefix <- "\tcd ../.. && Rscript -e \"library('mill'); r <- load_report();"

    title_regexp <- paste0("^", x$title, "$$")

    lines <- c("pdf:",
               sprintf("%s to_pdf(r[['%s']])\"", prefix, title_regexp),
               "",
               "import:",
               sprintf("%s import(r[['%s']], %s); from_docx(r[['%s']])\"",
                       prefix, title_regexp, "'Surveillance 2017/chapters'", title_regexp),
               "",
               "diff:",
               "\tdiff -c --label=text --label=typeset text.tex typeset.tex > typeset.patch; [ $$? -eq 1 ]",
               "",
               "patch:",
               "\tpatch text.tex -i typeset.patch -o typeset.tex",
               "",
               "roundtrip:",
               sprintf("%s roundtrip(r[['%s']])\"", prefix, title_regexp),
               "",
               "table_preview:",
               sprintf("%s preview_tables(r[['%s']])\"", prefix, title_regexp),
               "",
               "build_figures:",
               sprintf("%s build_figures(r[['%s']])\"", prefix, title_regexp),
               "rpd: roundtrip patch diff",
               "",
               "PHONY: pdf import diff patch roundtrip rpd",
               "")

    writeLines(lines, file.path(x$path, "Makefile"))

    if (!is.null(repo))
        add(repo, file.path(x$path, "Makefile"))

    invisible()
}
