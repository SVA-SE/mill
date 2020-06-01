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
    cat("Nuke previous build\n")
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
            cat(sprintf("Add to build dir: %s\n", to))
            if (startsWith(to, "img") && type == "web") {
                reduce_image(from, to)
            } else {
                file.copy(from, to)
            }
        })
    })

    ## We need to build report...
    cat(sprintf("Add to build dir: %s\n", "report.tex"))
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
    if (build) {
        filename <- paste0(chapter, ".tex")
        on.exit(unlink(filename), add = TRUE)
    }

    ## Create typeset.tex
    apply_patch()

    ## Ectract all references in the chapter.
    ref <- references()

    ## Specify the build directory.
    if (isTRUE(build)) {
        build_dir <- "./"
    } else {
        build_dir <- "../../build/"
    }

    ## Copy all figure files to the build directory.
    markers <- unique(ref[ref$reftype == "fig", "marker"])
    lapply(markers, function(marker) {
        ## The marker has the format 'fig:chapter:identifier'. Split
        ## the string to determine the identifier.
        identifier <- unlist(strsplit(marker, ":"))[3]

        ## Copy figure tex-file. If we are building a chapter, then
        ## the file already exists because of the patching.
        from <- paste0("fig_", normalize_title(chapter),
                       "_", identifier, ".tex")
        to <- paste0(build_dir, from)
        if (!file.exists(to)) {
            cat(sprintf("Add to build dir: %s\n", to))
            file.copy(from, to)
        }

        ## Search for files that fits the pattern
        ## 'fig_identifier.ext'.
        pattern <- paste0("^fig_", identifier, "[.](eps|pdf|png)")
        files <- list.files(pattern = pattern)
        if (!length(files))
            stop(sprintf("Missing ref file: %s\n", marker))

        ## Copy the files to the build directory after injecting the
        ## chapter name into the filename.
        lapply(files, function(from) {
            to <- paste0(build_dir, "fig_", normalize_title(chapter),
                         "_", identifier, ".", file_ext(from))
            cat(sprintf("Add to build dir: %s\n", to))
            file.copy(from, to)
        })
    })

    if (isTRUE(build)) {
        ## read in the pieces of the chapter

        svaclass <- system.file("assets/SVAchapter.cls", package = "mill")

        if (svaclass == "")
            stop(
                paste("Could not find SVAchapter.cls in mill package.",
                      "Ensure that you have the latest version of mill",
                      "(from Azure)")
            )

        file.copy(svaclass, ".")

        ## Stitch together the chapter
        tex <- c("\\documentclass{SVAchapter}",
                 "\\begin{document}",
                 "\\renewcommand\\headrulewidth{0pt}",
                 "\\renewcommand\\footrulewidth{0.5pt}",
                 "\\restoregeometry",
                 "\\pagestyle{fancy}",
                 "\\fancyhead{} % clear all header fields",
                 "\\fancyfoot{}",
                 "\\fancyfoot[LO,RE]{\\fontspec{Lato",
                 paste0("Light}\\textcolor{svared}{\\uppercase{",
                        "Disease Surveillance 2019}}}"),
                 "\\fancyfoot[LE,RO]{\\thepage}",
                 readLines("typeset.tex"),
                 "\\end{document}")
        placement <- grep("\\\\begin\\{multi", tex)[1]
        tex <- c(tex[seq_len(placement - 1)],
                 "\\thispagestyle{fancy}",
                 tex[seq(placement, length(tex))])
        writeLines(tex, filename)

        ## Build the filename pdf file.
        luatex(filename)
    } else {
        from <- "typeset.tex"
        to <- paste0(build_dir, normalize_title(chapter), ".tex")
        cat(sprintf("Add to build dir: %s\n", to))
        file.copy(from, to)

        ## Copy the tables (.tex)
        lapply(ref[ref$reftype == "tab", "marker"], function(marker) {
            from <- paste0(gsub(":", "_", marker), ".tex")
            to <- paste0(build_dir, from)
            cat(sprintf("Add to build dir: %s\n", to))
            file.copy(from, to)
        })

        ## Copy any images in the chapter
        files <- list.files(pattern = "^img_")
        lapply(files, function(from) {
            to <- paste0(build_dir, from)
            cat(sprintf("Add to build dir: %s\n", to))
            if (type == "web") {
                reduce_image(from, to)
            } else {
                file.copy(from, to)
            }
        })

        ## Copy any 'infocus' tex-files in the chapter.
        pattern <- paste0("^infocus_", normalize_title(chapter), "[^.]*[.]tex$")
        files <- list.files(pattern = pattern)
        lapply(files, function(from) {
            to <- paste0(build_dir, from)
            cat(sprintf("Add to build dir: %s\n", to))
            file.copy(from, to)
        })
    }

    invisible(NULL)
}
