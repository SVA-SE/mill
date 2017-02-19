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
##' @param cmd what to search for, either \\label{.} or \\ref{.}.
##' @param reftype the type of references to search for.
##' @return data.frame with the found references.
##' @export
references <- function(x, cmd, reftype) UseMethod("references")

##' @export
references.report <- function(x,
                              cmd     = c("label", "ref"),
                              reftype = c("all", "sec", "fig", "tab"))
{
    references(x$chapters, cmd, reftype)
}

##' @export
references.chapters <- function(x,
                                cmd     = c("label", "ref"),
                                reftype = c("all", "sec", "fig", "tab"))
{
    do.call("rbind",
            lapply(x, function(y) references(y, cmd, reftype)))
}

##' @export
references.chapter <- function(x,
                               cmd     = c("label", "ref"),
                               reftype = c("all", "sec", "fig", "tab"))
{
    pattern <- reference_pattern(cmd, reftype)
    files <- chapter_tex_files(x)

    do.call("rbind", (lapply(files, function(filename) {
        tex <- readLines(filename)
        m <- regmatches(tex, gregexpr(pattern, tex))
        m <- unlist(lapply(m, function(y) {
            regmatches(y, regexec(pattern, y))
        }))

        if (length(m)) {
            return(data.frame(filename = file.path("chapters",
                                                   x$title,
                                                   basename(filename)),
                              tex = m,
                              cmd = sub("[\\]([^{]+)[{][^}]*[}]", "\\1", m),
                              marker = sub("[\\][^{]+[{]([^}]*)[}]", "\\1", m),
                              stringsAsFactors = FALSE))
        }

        data.frame(filename = character(0),
                   tex = character(0),
                   cmd = character(0),
                   marker = character(0),
                   stringsAsFactors = FALSE)
    })))
}

##' Get figure labels
##'
##' @param x The report object or chapter object
##' @return invisible NULL
##' @export
get_labels <- function(x, what, reftype, include)
    UseMethod("get_labels")

##' @export
get_labels.report <- function(x,
                              what    = c("label", "ref"),
                              reftype = c("all", "sec", "fig", "tab"),
                              include = c("all", "text", "figures", "tables"))
{
    get_labels(x$chapters, reftype, include)
}

##' @export
get_labels.chapters <- function(x,
                                what    = c("label", "ref"),
                                reftype = c("all", "sec", "fig", "tab"),
                                include = c("all", "text", "figures", "tables"))
{
    do.call("rbind", lapply(x, function(y) get_labels(y, reftype, include)))
}

##' @export
get_labels.chapter <- function(x,
                               what    = c("label", "ref"),
                               reftype = c("all", "sec", "fig", "tab"),
                               include = c("all", "text", "figures", "tables"))
{
    pattern <- reference_patter(what, reftype)
    files <- chapter_tex_files(x, include)

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

##' Create a regular expression pattern to search for labels or
##' references
##' @param cmd what to search for, either \\label{.} or \\ref{.}.
##' @param reftype the type of references to search for.
##' @keywords internal
reference_pattern <- function(cmd     = c("label", "ref"),
                              reftype = c("all", "sec", "fig", "tab"))
{
    cmd <- match.arg(cmd, choices = cmd)
    reftype <- match.arg(reftype, choices = reftype)
    if (identical(reftype, "all")) {
        marker <- "[^}]*"
    } else {
        marker <- paste0(reftype, ":[^:]+:[^}]+")
    }
    paste0("[\\]", cmd, "[{]", marker, "[}]")
}

##' Get the chapter table tex files
##'
##' @param x the chapter object
##' @keywords internal
chapter_table_tex_files <- function(x) {
    stopifnot(methods::is(x, "chapter"))
    file.path(x$path, list.files(x$path, "^table-[^.]*[.]tex"))
}

##' Get the chapter tex files
##'
##' @param x the chapter object
##' @keywords internal
chapter_tex_files <- function(x) {
    stopifnot(methods::is(x, "chapter"))
    c(file.path(x$path, "text.tex"),
      figure_files(x, "tex"),
      chapter_table_tex_files(x))
}
