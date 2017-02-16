##' Select a luatex system call
##'
##' @return character string, giving the luatex command to run. On
##'     Windows: \code{texify --pdf --engine=luatex
##'     --max-iterations=50}, else \code{lualatex}.
##' @keywords internal
luatex_cmd <- function() {
    if(.Platform$OS.type == "windows")
        return("texify --pdf --engine=luatex --max-iterations=50")
    return("lualatex")
}

##' Run LuaTeX
##'
##' @param texname Run LuaTeX on texname.
##' @param clean logical. If \code{TRUE}, auxiliary files (aux, log,
##'     out) created by LuaTeX are removed.
##' @return invisible NULL.
##' @keywords internal
luatex <- function(texname, clean = TRUE) {
    wd <- setwd(dirname(texname))
    on.exit(setwd(wd))

    texname <- basename(texname)
    stopifnot(file.exists(texname))
    system(paste(luatex_cmd(), shQuote(texname)))

    if (identical(clean, TRUE)) {
        f <- tools::file_path_sans_ext(texname)
        file.remove(paste0(f, ".aux"))
        file.remove(paste0(f, ".log"))
        file.remove(paste0(f, ".out"))
    }

    invisible()
}
