##' Create a Makefile in each chapter of the report
##'
##' @param x The report object
##' @param repo The git repository to add the 'Makefile' to.
##' @param ... Additional arguments.
##' @return invisible NULL
##' @export
create_Makefile <- function(x, ...) UseMethod("create_Makefile")

##' @export
create_Makefile.report <- function(x, ...) {
    repo <- git2r::repository(x$path)

    lines <- c("all:",
               "\tRscript -e 'relax::to_pdf()'",
               "",
               "web:",
               "\tRscript -e \"relax::to_pdf(relax::load_report(), type = 'web')\"",
               "",
               "check:",
               "\tRscript -e \"relax::check()\"",
               "",
               "export:",
               "\tRscript -e \"relax::export()\"",
               "",
               "roundtrip:",
               "\tRscript -e \"relax::round_trip(relax::load_report())\"",
               "",
               ".PHONY: all check export roundtrip web")

    writeLines(lines, file.path(x$path, "Makefile"))

    if (!is.null(repo))
        git2r::add(repo, file.path(x$path, "Makefile"))

    lapply(x$chapters, function(y) create_Makefile(y, repo = repo))
    invisible()
}


##' @export
create_Makefile.chapter <- function(x, repo, ...) {
    lines <- c("pdf:",
               sprintf("\tcd ../.. && Rscript -e \"library('relax'); r <- load_report(); to_pdf(r[['%s']])\"",
                       x$title),
               "diff:",
               "\tdiff -c --label=text --label=typeset text.tex typeset.tex > typeset.patch; [ $$? -eq 1 ]",
               "",
               "patch:",
               "\tpatch text.tex -i typeset.patch -o typeset.tex",
               "",
               "roundtrip:",
               sprintf("\tcd ../.. && Rscript -e \"library('relax'); r <- load_report(); roundtrip(r[['%s']])\"",
                       x$title),
               "",
               "rpd: roundtrip patch diff",
               "",
               "PHONY: pdf diff patch roundtrip rpd",
               "")

    writeLines(lines, file.path(x$path, "Makefile"))

    if (!is.null(repo))
        git2r::add(repo, file.path(x$path, "Makefile"))

    invisible()
}
