##' Apply patch
##'
##' @return invisible(NULL)
##' @export
apply_patch <- function() {
    if (in_chapter()) {
        if (!file.exists("typeset.patch")) {
            file.copy(from = "text.tex", to = "typeset.tex", overwrite = TRUE)
            return(invisible(NULL))
        }

        output <- tryCatch(system2("patch",
                                   args = c("text.tex", "-i", "typeset.patch",
                                            "-o", "typeset.tex"),
                                   stdout = TRUE, stderr = TRUE),
                           warning = function(w) w)

        if (!identical(output,
                       "patching file typeset.tex (read from text.tex)")) {
            stop(paste("Unable to apply patch: ", basename(getwd())))
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

##' Create patch
##'
##' @return invisible(NULL)
##' @export
create_patch <- function() {
    if (in_chapter()) {
        if (!file.exists("typeset.tex"))
            stop("Missing file 'typeset.tex'")

        output <- tryCatch(system2("diff",
                                   args = c("-c", "--label=text",
                                            "--label=typeset", "text.tex",
                                            "typeset.tex > typeset.patch"),
                                   stdout = TRUE, stderr = TRUE),
                           warning = function(w) w)

        ## if (!identical(output,
        ##                "patching file typeset.tex (read from text.tex)")) {
        ##     stop(paste("Unable to create patch: ", basename(getwd())))
        ## }
    } else if (in_report()) {
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            create_patch()
            setwd(wd)
        })
    }

    invisible(NULL)
}
