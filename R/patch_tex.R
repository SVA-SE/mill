##' Apply_patch
##'
##' @export
##' @param x A chapter
apply_patch <- function(x) {
    UseMethod("apply_patch")
}

##' Apply_patch.report
##'
##' @export
##' @param x A report object
apply_patch.report <- function(x) {
    lapply(chapters(x)$section, function(y) apply_patch(y))
    invisible()
}

##' Apply_patch
##'
##' Apply the patch to the chapter
##'
##' @param x A chapter
##' @return invisible(NULL)
##' @export
apply_patch.chapter <- function(x) {
    output <- tryCatch(system2("patch",
                               args = c("text.tex", "-i", "typeset.patch",
                                        "-o", "typeset.tex"),
                               stdout = TRUE, stderr = TRUE),
                       warning = function(w) w)
    if (!identical(output,
                   "patching file typeset.tex (read from text.tex)")) {
        cat(output, "\n")
        stop(paste("Unable to apply patch: ", x$title))
    }

    invisible(NULL)
}
