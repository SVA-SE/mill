##' Apply patch
##'
##' @return invisible(NULL)
##' @export
apply_patch <- function() {
    if (in_chapter()) {
        output <- tryCatch(system2("patch",
                                   args = c("text.tex", "-i", "typeset.patch",
                                            "-o", "typeset.tex"),
                                   stdout = TRUE, stderr = TRUE),
                           warning = function(w) w)

        if (!identical(output,
                       "patching file typeset.tex (read from text.tex)")) {
            stop(paste("Unable to apply patch: ", chapter_title(x)))
        }
    } else if (in_report()) {
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            apply_patch()
            setwd(wd)
        })
    }

    invisible(NULL)
}
