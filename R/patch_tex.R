do_apply_patch <- function(from, patchfile, to)
{
    if (!file.exists(patchfile)) {
        file.copy(from = from, to = to, overwrite = TRUE)
        return(NULL)
    }

    output <- tryCatch(system2("patch",
                               args = c(from,
                                        "-i", patchfile,
                                        "-o", to),
                               stdout = TRUE, stderr = TRUE),
                       warning = function(w) w)

    if (!identical(output,
                   paste0("patching file ", to, " (read from ", from, ")")))
    {
        stop(paste("Unable to apply patch: ", basename(getwd())))
    }

    NULL
}

##' Apply patch
##'
##' @return invisible(NULL)
##' @export
apply_patch <- function() {
    if (in_chapter()) {
        do_apply_patch("text.tex", "typeset.patch", "typeset.tex")
    } else if (in_report()) {
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            apply_patch()
            setwd(wd)
        })
    }

    invisible(NULL)
}

do_create_patch <- function(from, to, patchfile)
{
    if (!file.exists(to))
        stop(paste0("Missing file '", to, "'"))

    if (!file.exists(from))
        stop(paste0("Missing file '", from, "'"))

    system2("diff",
            args = c("-c", "--label=text",
                     "--label=typeset",
                     from, to, ">", patchfile))

    ## Drop empty patch
    if (!file.size(patchfile))
        unlink(patchfile)

    NULL
}

##' Create patch
##'
##' @return invisible(NULL)
##' @export
create_patch <- function() {
    if (in_chapter()) {
        do_create_patch("text.tex", "typeset.tex", "typeset.patch")
    } else if (in_report()) {
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            create_patch()
            setwd(wd)
        })
    }

    invisible(NULL)
}
