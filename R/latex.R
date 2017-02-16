##' Select a luatex system call
##'
##' 
##' @keywords internal
select_luatex <- function() {
    if(.Platform$OS.type == "windows")
        return("texify --pdf --engine=luatex --max-iterations=50 ")
    return("lualatex ")
}


##' Run LuaTeX
##'
##' @param texname Run LuaTeX on texname.
##' @param clean logical. If \code{TRUE}, auxiliary files (aux, log)
##'     created by LuaTeX are removed.
##' @return invisible NULL.
##' @keywords internal
luatex <- function(texname, clean = TRUE) {
    wd <- setwd(dirname(texname))
    onexit(setwd(wd))

    texname <- basename(texname)
    stopifnot(file.exists(texname))
    system(paste0(select_luatex(), shQuote(texname)))

    if (identical(clean, TRUE)) {
        f <- tools::file_path_sans_ext(texname)
        file.remove(paste0(f, ".aux"))
        file.remove(paste0(f, ".log"))
    }

    invisible()
}
