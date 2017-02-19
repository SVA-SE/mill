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
##' @export
references <- function(x, reftype) UseMethod("references")

##' @export
references.report <- function(x, reftype = c("all", "fig", "tab")) {
    references(x$chapters, reftype)
}

##' @export
references.chapters <- function(x, reftype = c("all", "fig", "tab")) {
    unlist(lapply(x, function(y) references(y, reftype)))
}

##' @export
references.chapter <- function(x, reftype = c("all", "fig", "tab")) {
    pattern <- switch(match.arg(reftype),
                      all = "\\\\ref[{][^}]*[}]",
                      fig = "\\\\ref[{]fig:[^:]+:[^}]+[}]",
                      tab = "\\\\ref[{]tab:[^:]+:[^}]+[}]")

    tex <- readLines(file.path(x$path, "text.tex"))
    m <- regmatches(tex, gregexpr(pattern, tex))
    unlist(lapply(m, function(y) {
        regmatches(y, regexec(pattern, y))
    }))
}

##' Get figure labels
##'
##' @param x The report object or chapter object
##' @return invisible NULL
##' @export
get_labels <- function(x, labeltype, include) UseMethod("get_labels")

##' @export
get_labels.report <- function(x,
                              labeltype = c("all", "sec", "fig", "tab"),
                              include   = c("all", "text", "figures", "tables"))
{
    get_labels(x$chapters, labeltype, include)
}

##' @export
get_labels.chapters <- function(x,
                                labeltype = c("all", "sec", "fig", "tab"),
                                include   = c("all", "text", "figures", "tables"))
{
    do.call("rbind", lapply(x, function(y) get_labels(y, labeltype, include)))
}

##' @export
get_labels.chapter <- function(x,
                               labeltype = c("all", "sec", "fig", "tab"),
                               include   = c("all", "text", "figures", "tables"))
{
    pattern <- switch(match.arg(labeltype),
                      all = "[^}]*",
                      sec = "sec:[^:]+:[^}]+",
                      fig = "fig:[^:]+:[^}]+",
                      tab = "tab:[^:]+:[^}]+")
    pattern <- paste0("[\\]label[{]", pattern, "[}]")

    include <- match.arg(include)
    if (identical(include, "all"))
        include <- c("text", "figures", "tables")
    files <- NULL
    if ("text" %in% include)
        files <- c(files, file.path(x$path, "text.tex"))
    if ("figures" %in% include)
        files <- c(files, figure_files(x, "tex"))
    if ("tables" %in% include)
        files <- c(files,
                   file.path(x$path, list.files(x$path, "^table-[^.]*[.]tex")))

    do.call("rbind", (lapply(files, function(filename) {
        tex <- readLines(filename)
        m <- regmatches(tex, gregexpr(pattern, tex))
        lbl <- unlist(lapply(m, function(y) {
            regmatches(y, regexec(pattern, y))
        }))

        if (length(lbl))
            return(data.frame(filename = filename, label = lbl))
        data.frame(filename = character(0), lbl = character(0))
    })))
}

##' Get the chapter tex files
##'
##' @param x the chapter object
##' @param include the type of chapter tex files to include.
##' @keywords internal
chapter_tex_files <- function(x, include = c("all", "text", "figures", "tables")) {
    stopifnot(is(x, "chapter"))
    include <- match.arg(include)
    if (identical(include, "all"))
        include <- c("text", "figures", "tables")
    files <- NULL
    if ("text" %in% include)
        files <- c(files, file.path(x$path, "text.tex"))
    if ("figures" %in% include)
        files <- c(files, figure_files(x, "tex"))
    if ("tables" %in% include)
        files <- c(files,
                   file.path(x$path, list.files(x$path, "^table-[^.]*[.]tex")))
    files
}
