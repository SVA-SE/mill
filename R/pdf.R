##' Build report
##'
##' @param x The report object
##' @param ... Additional parameters
##' @return invisible NULL
##' @export
to_pdf <- function(x, ...) UseMethod("to_pdf")

##' @export
to_pdf.default <- function(x, ...) {
    to_pdf(load_report())
}

##' @export
to_pdf.report <- function(x, type = c("print", "web"), ...) {
    type <- match.arg(type)
    ## Nuke previous build
    unlink("build", recursive = TRUE)
    dir.create("build")
    wd <- setwd(file.path(x$path, "build"))
    on.exit(setwd(wd))

    lapply(x$chapters, function(y) to_pdf(y, build = FALSE, type = type, ...))

    ## Copy the directories in assets: cover, front-matter and back-matter
    lapply(c("cover", "front-matter", "back-matter"), function(dir) {
        files <- list.files(file.path("../assets", dir), pattern = "[^auto]")
        lapply(files, function(to) {
            from <- file.path("../assets", dir, to)
            if(startsWith(to, "img") && type == "web") {
                reduce_image(from, to)
            } else {
                file.copy(from, to)
            }
        })
    })

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
to_pdf.chapter <- function(x, build = TRUE, type = c("print", "web"), ...) {
    type <- match.arg(type)
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

        ## Stitch together the chapter
        tex <- c(presnippet,
                 "\\begin{document}",
                 readLines("typeset.tex"),
                 "\\end{document}")
        writeLines(tex, filename)

        ## Build the filename pdf file.
        luatex(filename)
    } else {
        file.copy("typeset.tex",
                  paste0("../../build/", normalize_title(x$title), ".tex"))

        ## Copy the figures (.pdf, .png and .tex) and tables (.tex)
        ref <- references(x)
        lapply(ref[ref$reftype == "fig", "marker"], function(marker) {
            marker <- paste0(gsub(":", "_", marker), c(".pdf", ".tex", ".png"))
            file.copy(marker, paste0("../../build/", marker))
        })
        lapply(ref[ref$reftype == "tab", "marker"], function(marker) {
            marker <- paste0(gsub(":", "_", marker), ".tex")
            file.copy(marker, paste0("../../build/", marker))
        })

        ## Copy any images in the chapter
        files <- list.files(x$path, pattern = "^img_")
        lapply(files, function(from) {
            to <- paste0("../../build/", from)
            if(type == "web") {
                reduce_image(from, to)
            } else {
                file.copy(from, to)
            }
        })
    }

    invisible(NULL)
}
