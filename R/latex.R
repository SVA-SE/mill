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
##' @return data.frame with the found references.
##' @export
references <- function(x) UseMethod("references")

##' @export
references.report <- function(x) {
    references(x$chapters)
}

##' @export
references.chapters <- function(x) {
    do.call("rbind", lapply(x, function(y) references(y)))
}

##' @export
references.chapter <- function(x) {
    pattern <- "[\\]label[{][^}]*[}]|[\\]ref[{][^}]*[}]"
    files <- chapter_tex_files(x)

    do.call("rbind", (lapply(files, function(filename) {
        tex <- readLines(filename)
        m <- regmatches(tex, gregexpr(pattern, tex))
        m <- unlist(lapply(m, function(y) {
            regmatches(y, regexec(pattern, y))
        }))

        if (length(m)) {
            filename = file.path("chapters", x$title, basename(filename))
            tex <- m
            cmd <- sub("[\\]([^{]+)[{][^}]*[}]", "\\1", tex)
            marker <- sub("[\\][^{]+[{]([^}]*)[}]", "\\1", tex)
            reftype <- sapply(strsplit(marker, ":"), "[", 1)
        } else {
            filename = character(0)
            tex <- character(0)
            cmd <- character(0)
            marker <- character(0)
            reftype <- character(0)
        }

        data.frame(filename = filename,
                   tex      = tex,
                   cmd      = cmd,
                   marker   = marker,
                   reftype  = reftype,
                   stringsAsFactors = FALSE)
    })))
}

##' Get the chapter tex files
##'
##' @param x the chapter object
##' @keywords internal
chapter_tex_files <- function(x, type = c("all", "text", "fig", "table")) {

    type = match.arg(type)
    stopifnot(methods::is(x, "chapter"))

    text_files <- NULL
    fig_files <- NULL
    tab_file <- NULL

    if (type %in% c("all", "text"))
        text_files <- file.path(x$path, "text.tex")
    if (type %in% c("all", "fig"))
        fig_files <- figure_files(x, "tex")
    if (type %in% c("all", "table"))
        tab_files <- table_files(x, "tex")

    c(text_files, fig_files, tab_files)
}
