##' Build report
##'
##' @param x The report object
##' @return invisible NULL
##' @export
to_pdf <- function(x, ...) UseMethod("to_pdf")

##' @export
to_pdf.report <- function(x, ...) {
    ## Nuke previous build
    unlink("build", recursive = TRUE)
    dir.create("build")
    wd <- setwd(file.path(x$path, "build"))
    on.exit(setwd(wd))

    lapply(x$chapters, function(y) to_pdf(y, build = FALSE, ...))

    ## Copy the directories in assets: cover, front-matter and back-matter
    lapply(c("cover", "front-matter", "back-matter"), function(dir) {
        files <- list.files(file.path("../assets", dir), pattern = "[^auto]")
        lapply(files, function(filename) {
            file.copy(file.path("../assets", dir, filename), to = filename)
        })
    })

    ## Copy fonts
    file.copy("../assets/fonts", ".", recursive = TRUE)

    ## We need to build report...
    presnippet <- readLines("../assets/latex/pre-snippet.tex")
    text <- readLines("../assets/latex/report.tex")
    ## Stitch together the chapter
    tex <- c(presnippet,
             "\\begin{document}",
             text,
             "\\end{document}")
    writeLines(tex, "report.tex")

    ## Build the preview pdf file.
    luatex("report.tex")

    invisible()
}

##' @export
to_pdf.chapter <- function(x, build = TRUE, ...) {
    wd <- setwd(x$path)
    on.exit(unlink("typeset.tex"))
    if (build) {
        filename <- paste0(x$title, ".tex")
        on.exit(unlink(filename), add = TRUE)
    }
    on.exit(setwd(wd), add = TRUE)

    ## Create typeset.tex
    apply_patch(x)

    if (build) {
        ## read in the pieces of the chapter
        a <- assets(x)
        presnippet <- readLines(file.path(a, "latex/pre-snippet.tex"))
        presnippet <- gsub("fonts/", paste0(a, "/fonts/"), presnippet)

        ## Stitch together the chapter
        tex <- c(presnippet,
                 "\\begin{document}",
                 readLines("typeset.tex"),
                 "\\end{document}")
        writeLines(tex, filename)

        ## Build the filename pdf file.
        luatex(filename)
    } else {
        file.copy("typeset.tex", paste0("../../build/",
                                        gsub(" ", "-", tolower(x$title)), ".tex"))
        ref <- references(x)
        lapply(ref[ref$reftype == "fig", "marker"], function(marker) {
            marker <- paste0(gsub(":", "_", marker), c(".pdf", ".tex"))
            file.copy(marker, paste0("../../build/", marker))
        })
        lapply(ref[ref$reftype == "tab", "marker"], function(marker) {
            marker <- paste0(gsub(":", "_", marker), ".tex")
            file.copy(marker, paste0("../../build/", marker))
        })
    }

    invisible(NULL)
}

##' Build chapters
##'
##' @param x The report object, chapter object or the path to the text
##'     tex file.
##' @return invisible NULL
##' @export
build_text <- function(x) UseMethod("build_text")

##' @export
build_text.report <- function(x) {
    preview <- tempfile(tmpdir = x$path, fileext = ".tex")
    on.exit(unlink(preview))
    cleanup_patched_files <- function(x) {
        lapply(x$chapters, function(y){
            unlink(file.path(y$path, "typeset.tex"))
        })
    }
    on.exit(cleanup_patched_files(x), add = TRUE)

    ## read in the pieces of the chapters
    a <- assets(x)
    apply_patch(x)
    presnippet <- readLines(file.path(a, "latex/pre-snippet.tex"))
    presnippet <- gsub("fonts/", paste0(a, "/fonts/"), presnippet)
    text <- readLines(file.path(a, "latex/report.tex"))
    ## Stitch together the chapter
    tex <- c(presnippet,
             "\\begin{document}",
             text,
             "\\end{document}")
    writeLines(tex, preview)

    ## Build the preview pdf file.
    luatex(preview)

    ## Copy the pdf preview to 'preview-figure.pdf'
    from <- paste0(tools::file_path_sans_ext(preview), ".pdf")
    to <- file.path(x$path,"report.pdf")
    if (file.exists(to))
        file.remove(to)
    file.rename(from, to)
}

##' @export
build_text.chapters <- function(x) {
    lapply(x, function(y) build_text(y))
    invisible()
}

##' @export
build_text.chapter <- function(x) {
    preview <- tempfile(tmpdir = x$path, fileext = ".tex")
    on.exit(unlink(preview))
    on.exit(unlink(file.path(x$path, "typeset.tex")), add = TRUE)

    ## read in the pieces of the chapter
    a <- assets(x)
    apply_patch(x)
    text <- readLines(file.path(x$path, "typeset.tex"))
    presnippet <- readLines(file.path(a, "latex/pre-snippet.tex"))
    presnippet <- gsub("fonts/", paste0(a, "/fonts/"), presnippet)
    ## Stitch together the chapter
    tex <- c(presnippet,
             "\\begin{document}",
             text,
             "\\end{document}")
    writeLines(tex, preview)

    ## Build the preview pdf file.
    luatex(preview)

    ## Copy the pdf preview to 'preview-figure.pdf'
    from <- paste0(tools::file_path_sans_ext(preview), ".pdf")
    to <- file.path(x$path,
                    paste0(gsub(" ", "-", x$title), ".pdf"))
    if (file.exists(to))
        file.remove(to)
    file.rename(from, to)
}
