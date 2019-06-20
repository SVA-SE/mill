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
                   "\tRscript -e \"mill::import(); mill::from_docx(git2r::repository())\"",
                   "",
                   "diff:",
                   "\tRscript -e \"mill::create_patch()\"",
                   "",
                   "patch:",
                   "\tRscript -e \"mill::apply_patch()\"",
                   "",
                   "roundtrip:",
                   "\tRscript -e \"mill::roundtrip()\"",
                   "",
                   "build_figures:",
                   "\tRscript -e \"mill::build_figures()\"",
                   "",
                   "rpd: roundtrip patch diff",
                   "",
                   "export:",
                   "\tRscript -e \"mill::export()\"",
                   "",
                   "clean:",
                   "\tRscript -e \"mill::cleanup()\"",
                   "",
                   "PHONY: pdf import diff patch roundtrip build_figures rpd export clean",
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
                   "patch:",
                   "\tRscript -e \"mill::apply_patch()\"",
                   "",
                   "roundtrip:",
                   "\tRscript -e \"mill::roundtrip()\"",
                   "",
                   ".PHONY: all check export patch roundtrip web")

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
