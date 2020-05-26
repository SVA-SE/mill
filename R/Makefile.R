##' Create a Makefile in each chapter of the report
##'
##' @return invisible NULL
##' @importFrom git2r repository
##' @importFrom git2r add
##' @export
create_Makefile <- function() {
    if (in_chapter()) {
        lines <- c(".PHONY: pdf",
                   "pdf:",
                   "\tRscript -e \"mill::to_pdf()\"",
                   "",
                   ".PHONY: import",
                   "import:",
                   paste0("\tRscript -e \"mill::import(); ",
                          "mill::from_docx(git2r::repository())\""),
                   "",
                   ".PHONY: check",
                   "check:",
                   "\tRscript -e \"mill::check()\"",
                   "",
                   ".PHONY: diff",
                   "diff:",
                   "\tRscript -e \"mill::create_patch()\"",
                   "",
                   ".PHONY: patch",
                   "patch:",
                   "\tRscript -e \"mill::apply_patch()\"",
                   "",
                   ".PHONY: roundtrip",
                   "roundtrip:",
                   "\tRscript -e \"mill::roundtrip()\"",
                   "",
                   ".PHONY: build_figures",
                   "build_figures:",
                   "\tRscript -e \"mill::build_figures()\"",
                   "",
                   ".PHONY: rpd",
                   "rpd: roundtrip patch diff",
                   "",
                   "export:",
                   "\tRscript -e \"mill::export()\"",
                   "",
                   ".PHONY: clean",
                   "clean:",
                   "\tRscript -e \"mill::cleanup()\"")

        writeLines(lines, "Makefile")
        add(repository("../.."),
            paste0("chapters/", basename(getwd()), "/Makefile"))
    } else if (in_report()) {
        lines <- c(".PHONY: all",
                   "all:",
                   "\tRscript -e 'mill::to_pdf()'",
                   "",
                   ".PHONY: web",
                   "web:",
                   "\tRscript -e \"mill::to_pdf(type = 'web')\"",
                   "",
                   ".PHONY: check",
                   "check:",
                   "\tRscript -e \"mill::check()\"",
                   "",
                   ".PHONY: export",
                   "export:",
                   "\tRscript -e \"mill::export()\"",
                   "",
                   ".PHONY: patch",
                   "patch:",
                   "\tRscript -e \"mill::apply_patch()\"",
                   "",
                   ".PHONY: roundtrip",
                   "roundtrip:",
                   "\tRscript -e \"mill::roundtrip()\"")

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
