##' Run pandoc
##'
##' @param cmd The command to pandoc.
##' @keywords internal
pandoc <- function(cmd) {
    system(paste0("pandoc ", cmd))
}
