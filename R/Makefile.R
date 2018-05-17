##' Create a Makefile in each chapter of the report
##'
##' @return invisible NULL
##' @importFrom git2r repository
##' @importFrom git2r add
##' @export
create_Makefile <- function() {
    if (in_chapter()) {
        lines <- c("pdf:",
                   "\tRscript -e \"mill::to_pdf()\"",
                   "",
                   "import:",
                   "\tRscript -e \"mill::import(); mill::from_docx()\"",
                   "",
                   "diff:",
                   "\tdiff -c --label=text --label=typeset text.tex typeset.tex > typeset.patch; [ $$? -eq 1 ]",
                   "",
                   "patch:",
                   "\tpatch text.tex -i typeset.patch -o typeset.tex",
                   "",
                   "roundtrip:",
                   "\tRscript -e \"mill::roundtrip()\"",
                   "",
                   "table_preview:",
                   "\tRscript -e \"mill::preview_tables()\"",
                   "",
                   "build_figures:",
                   "\tRscript -e \"mill::build_figures()\"",
                   "",
                   "rpd: roundtrip patch diff",
                   "",
                   "export:",
                   "\tRscript -e \"mill::export()\"",
                   "",
                   "PHONY: pdf import diff patch roundtrip table_preview build_figures rpd export",
                   "")

        writeLines(lines, "Makefile")
        add(repository("../.."), paste0("chapters/", basename(getwd()), "/Makefile"))
    } else if (in_report()) {
        lines <- c("all:",
                   "\tRscript -e 'mill::to_pdf()'",
                   "",
                   "web:",
                   "\tRscript -e \"mill::to_pdf(type = 'web')\"",
                   "",
                   "check:",
                   "\tRscript -e \"mill::check()\"",
                   "",
                   "export:",
                   "\tRscript -e \"mill::export()\"",
                   "",
                   "roundtrip:",
                   "\tRscript -e \"mill::roundtrip()\"",
                   "",
                   ".PHONY: all check export roundtrip web")

        writeLines(lines, "Makefile")
        add(repository(), "Makefile")

        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            create_Makefile()
            setwd(wd)
        })
    }

    invisible()
}
