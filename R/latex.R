##' Select a luatex system call
##'
##' @return character string, giving the luatex command to run. On
##'     Windows: \code{texify --pdf --engine=luatex
##'     --max-iterations=50}, else \code{lualatex}.
##' @noRd
luatex_cmd <- function(options = c("--interaction=nonstopmode", "-V 7")) {
    if (.Platform$OS.type == "windows")
        return("texify --pdf --engine=luatex --max-iterations=50")
    paste("lualatex", options)
}

##' Run LuaTeX
##'
##' @param texname Run LuaTeX on texname.
##' @param clean logical. If \code{TRUE}, auxiliary files (aux, log,
##'     out) created by LuaTeX are removed.
##' @importFrom tools file_path_sans_ext
##' @return invisible NULL.
##' @noRd
luatex <- function(texname, clean = FALSE) {
    wd <- setwd(dirname(texname))
    on.exit(setwd(wd))

    cat("Build the pdf\n\n")
    texname <- basename(texname)
    stopifnot(file.exists(texname))
    system(paste(luatex_cmd(), shQuote(texname)))
    if (.Platform$OS.type != "windows")
        system(paste(luatex_cmd(), shQuote(texname)))

    if (identical(clean, TRUE)) {
        f <- file_path_sans_ext(texname)
        file.remove(paste0(f, ".aux"))
        file.remove(paste0(f, ".log"))
        file.remove(paste0(f, ".out"))
    }

    invisible(NULL)
}

##' TeX references
##'
##' @return data.frame with the found references.
##' @export
references <- function() {
    if (in_chapter()) {
        return(references_chapter())
    } else if (in_report()) {
        refs <- lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            ref <- references()
            setwd(wd)
            ref
        })

        return(do.call("rbind", refs))
    }

    stop("Unexpected error")
}

references_chapter <- function() {
    pattern <- "[\\]label[{][^}]*[}]|[\\]ref[{][^}]*[}]"
    files <- chapter_tex_files()

    do.call("rbind", (lapply(files, function(filename) {
        if (!file.exists(filename))
            stop("Missing file:", paste0(getwd(), "/", filename))
        tex <- readLines(filename)
        m <- regmatches(tex, gregexpr(pattern, tex))
        m <- unlist(lapply(m, function(y) {
            regmatches(y, regexec(pattern, y))
        }))

        if (length(m)) {
            filename <- paste0("chapters/", basename(getwd()), "/", filename)
            tex <- m
            cmd <- sub("[\\]([^{]+)[{][^}]*[}]", "\\1", tex)
            marker <- sub("[\\][^{]+[{]([^}]*)[}]", "\\1", tex)
            reftype <- sapply(strsplit(marker, ":"), "[", 1)
        } else {
            filename <- character(0)
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
##' @importFrom methods is
##' @noRd
chapter_tex_files <- function(type = c("all", "text", "fig", "table")) {
    type <- match.arg(type)

    text_files <- NULL
    fig_files  <- NULL
    tab_files  <- NULL

    if (type %in% c("all", "text"))
        text_files <- "text.tex"
    if (type %in% c("all", "fig"))
        fig_files <- figure_files("tex")
    if (type %in% c("all", "table"))
        tab_files <- table_files("tex")

    c(text_files, fig_files, tab_files)
}
