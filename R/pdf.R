##' Build report
##'
##' @param type The report type to create
##' @return invisible NULL
##' @export
to_pdf <- function(type = c("print", "web")) {
    if (in_chapter()) {
        to_pdf_chapter(build = TRUE, type = type)
    } else if (in_report()) {
        to_pdf_report(type = type)
    }

    invisible(NULL)
}

##' @noRd
to_pdf_report <- function(type = c("print", "web")) {
    type <- match.arg(type)
    ## Nuke previous build
    unlink("build", recursive = TRUE)
    dir.create("build")

    lapply(list.files("chapters"), function(chapter) {
        wd <- setwd(paste0("chapters/", chapter))
        to_pdf_chapter(build = FALSE, type = type)
        setwd(wd)
    })

    wd <- setwd("build")
    on.exit(setwd(wd))

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

    invisible(NULL)
}

##' @noRd
to_pdf_chapter <- function(build = TRUE, type = c("print", "web")) {
    type <- match.arg(type)
    chapter <- basename(getwd())
    on.exit(unlink("typeset.tex"), add = TRUE)
    if (build) {
        filename <- paste0(chapter, ".tex")
        on.exit(unlink(filename), add = TRUE)
    }

    ## Create typeset.tex
    apply_patch()

    if (isTRUE(build)) {
        ## read in the pieces of the chapter
        presnippet <- readLines("../../assets/latex/pre-snippet.tex")

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
                  paste0("../../build/", normalize_title(chapter), ".tex"))

        ## Copy the figures (.pdf, .png and .tex) and tables (.tex)
        ref <- references()
        lapply(ref[ref$reftype == "fig", "marker"], function(marker) {
            marker <- paste0(gsub(":", "_", marker), c(".pdf", ".tex", ".png", ".eps"))
            file.copy(marker, paste0("../../build/", marker))
        })
        lapply(ref[ref$reftype == "tab", "marker"], function(marker) {
            marker <- paste0(gsub(":", "_", marker), ".tex")
            file.copy(marker, paste0("../../build/", marker))
        })

        ## Copy any images in the chapter
        files <- list.files(pattern = "^img_")
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
