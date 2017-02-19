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
    system(paste(luatex_cmd(), shQuote(texname)))

    if (identical(clean, TRUE)) {
        f <- tools::file_path_sans_ext(texname)
        file.remove(paste0(f, ".aux"))
        file.remove(paste0(f, ".log"))
        file.remove(paste0(f, ".out"))
    }

    invisible()
}

##' TeX references
##'
##' @param x the report or chapter object.
##' @param reftype the type of references to search for.
##' @return character vector with found references.
##' @keywords internal
references <- function(x, reftype) UseMethod("references")

##' @keywords internal
references.report <- function(x, reftype = c("all", "fig", "tab")) {
    references(x$chapters, reftype)
}

##' @keywords internal
references.chapters <- function(x, reftype = c("all", "fig", "tab")) {
    unlist(lapply(x, function(y) references(y, reftype)))
}

##' @keywords internal
references.chapter <- function(x, reftype = c("all", "fig", "tab")) {
    pattern <- switch(match.arg(reftype),
                      all = "\\\\ref[{][^}]*[}]",
                      fig = "\\\\ref[{]fig:[^:]+:[^}]+[}]",
                      tab = "\\\\ref[{]tab:[^:]+:[^}]+[}]")

    tex <- readLines(file.path(x$path, "text.tex"))
    m <- regmatches(tex, gregexpr(pattern, tex))
    unlist(lapply(m, function(x) {
        regmatches(x, regexec(pattern, x))
    }))
}
