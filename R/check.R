##' Check report
##'
##' @return invisible \code{FALSE} if OK, else invisible \code{TRUE}.
##' @importFrom utils capture.output
##' @importFrom utils packageVersion
##' @importFrom git2r repository
##' @export
check <- function() {
    cat("* using 'mill' version",
        as.character(packageVersion("mill")), "\n")

    if (check_expect_pandoc_is_installed())
        return(invisible(TRUE))
    if (check_expect_patch_is_installed())
        return(invisible(TRUE))

    cat("* loading report ... ")
    report <- tryCatch(load_report(), error = function(e) NULL)
    if (is.null(report)) {
        cat("ERROR\n")
        return(invisible(TRUE))
    }
    cat("OK\n    ")
    cat(capture.output(report)[1:4], sep = "\n    ")

    if (check_expect_clean_repository())
        return(invisible(TRUE))

    result <- check_tex_to_docx_round_trip()
    result <- c(result, check_open_track_changes())
    result <- c(result, check_apply_typeset_patch())
    result <- c(result, check_reference_format())
    result <- c(result, check_missing_figure_reference_files())
    result <- c(result, check_missing_table_reference_files())
    result <- c(result, check_range_character())
    result <- c(result, check_thousand_separator())
    result <- c(result, check_pattern("[.]\\s*[.]", "checking multiple dots"))
    result <- c(result, check_pattern("[,]\\s*[,]", "checking multiple commas"))
    result <- c(result, check_pattern("\u00b4",
                                      "checking for incorrect apostrophe character"))
    result <- c(result, check_pattern("[/]\\s*[\\]numprint[{]100000[}]",
                                      "checking for incorrect 'per 100000 inhabitants'"))
    result <- c(result, check_pattern("[0-9]\\s+[\\][%]",
                                      "checking for space between digit and '%'"))
    result <- c(result, check_pattern("http[:]//",
                                      "checking for 'http://'"))
    ## result <- c(result, check_pattern("2017",
    ##                                   "checking for '2017'"))
    result <- c(result, check_pattern("(?<![a-z\\{])figure(?![a-z])",
                                      "checking for lowercase 'figure'",
                                      perl = TRUE))
    result <- c(result, check_pattern("(?<![a-z\\{])table(?![a-z])",
                                      "checking for lowercase 'table'",
                                      perl = TRUE))
    result <- c(result, check_pattern("[fF]ig(?![u:_])",
                                      "checking for shortform of figure",
                                      perl = TRUE))
    result <- c(result, check_pattern("(?<![a-z\\{])[tT]ab(?![l:_])",
                                      "checking for shortform of table",
                                      perl = TRUE))
    result <- c(result, check_highlight())

    invisible(any(result))
}

##' Check that repository is clean
##'
##' To protect against overwriting un-committed changes.
##' @importFrom git2r diff
##' @importFrom git2r repository
##' @noRd
check_expect_clean_repository <- function() {
    cat("* checking that repository is clean ... ")

    ## Check if the working tree is clean
    s <- status(repository())
    if (length(s$unstaged) || length(s$staged)) {
        cat("ERROR\n")
        return(TRUE)
    }

    cat("OK\n    ")
    cat(capture.output(repository()), sep = "\n    ")
    FALSE
}

##' Check that pandoc is installed
##'
##' pandoc is required to check for example 'tex' to 'docx' round
##' trip.
##' @noRd
check_expect_pandoc_is_installed <- function() {
    cat("* checking that 'pandoc' is installed ... ")

    output <- tryCatch(system("pandoc --version", intern = TRUE,
                              ignore.stderr = TRUE),
                       error = function(e) character(0))
    ver <- grep("^pandoc[[:space:]]+[.0-9]*", output)
    if (!length(ver)) {
        cat("ERROR\n")
        return(TRUE)
    }

    cat("OK\n   ", output[ver], "\n")
    FALSE
}

##' Check that GNU patch is installed
##'
##' patch is required to check that applying patches work.
##' @noRd
check_expect_patch_is_installed <- function() {
    cat("* checking that 'patch' is installed ... ")

    output <- tryCatch(system("patch --version", intern = TRUE,
                              ignore.stderr = TRUE),
                       error = function(e) character(0))
    ver <- grep("^GNU patch[[:space:]]+[.0-9]*", output)
    if (!length(ver)) {
        cat("ERROR\n")
        return(TRUE)
    }

    cat("OK\n   ", output[ver], "\n")
    FALSE
}

##' Check conversion between 'tex' and 'docx'
##'
##' Checking that converting to 'docx' from 'tex' and converting back
##' to 'tex' doesn't generate changes.
##' @importFrom git2r commits
##' @importFrom git2r reset
##' @importFrom git2r status
##' @noRd
check_tex_to_docx_round_trip <- function() {
    on.exit(reset(commits(repository(), n = 1)[[1]], "hard"))
    cat("* checking 'tex' to 'docx' round trip ... ")

    l <- sapply(list.files("chapters"), function(chapter) {
        wd <- setwd(paste0("chapters/", chapter))
        to_docx()
        from_docx()
        setwd(wd)
        unstaged <- unlist(status(repository())$unstaged)
        if (file.path("chapters", chapter, "text.tex") %in% unstaged)
            return(file.path("chapters", chapter, "text.tex"))
        NULL
    })

    l <- l[!sapply(l, is.null)]
    if (length(l)) {
        cat("ERROR\n")
        lapply(l, function(filename) cat("   ", filename, "\n"))
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}

##' Check for open track changes in each chapter docx-file.
##'
##' @noRd
check_open_track_changes <- function()
{
    cat("* checking for open track changes ... ")

    l <- sapply(list.files("chapters"), function(chapter) {
        filename <- paste0("workspace/chapters/", chapter, "/", chapter, ".docx")
        if (!file.exists(filename))
            stop("Missing file:", filename)

        ## Unzip the content of the word file.
        on.exit(unlink(file.path(tempdir(), "document.xml")), add = TRUE)
        unzip(filename, "word/document.xml", junkpaths = TRUE, exdir = tempdir())

        ## Parse the content of the word file
        doc <- read_xml(file.path(tempdir(), "document.xml"))

        ## Check for insertions.
        if (length(xml_find_all(doc, xpath = "//w:ins")) > 0)
            return(filename)
        NULL
    })

    l <- l[!sapply(l, is.null)]
    if (length(l)) {
        cat("ERROR\n")
        lapply(l, function(filename) cat("   ", filename, "\n"))
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}

##' Check apply patch 'typeset.patch' to 'text.tex'
##'
##' Checking that applying patches doesn't generate warnings or
##' errors.
##' @noRd
check_apply_typeset_patch <- function() {
    cat("* checking apply typeset patch ... ")

    l <- sapply(list.files("chapters"), function(chapter) {
        wd <- setwd(paste0("chapters/", chapter))
        result <- tryCatch(apply_patch(), error = function(e) chapter)
        setwd(wd)
        result
    })

    l <- l[!sapply(l, is.null)]
    if (length(l)) {
        cat("ERROR\n")
        lapply(l, function(filename) cat("   ", filename, "\n"))
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}

##' Check reference format
##'
##' @noRd
check_reference_format <- function(x) {
    cat("* checking reference format ... ")

    ref <- references()
    ref <- ref[ref$cmd == "ref", ]
    ref_all <- ref$tex
    ref_fig <- ref$tex[ref$reftype == "fig"]
    ref_tab <- ref$tex[ref$reftype == "tab"]

    d <- setdiff(ref$tex, c(ref_fig, ref_tab))
    if (length(d)) {
        cat("ERROR\n    ")
        cat(d, sep = "\n    ")
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}

##' Check for missing figure reference files
##'
##' Check for figure references in the 'text.tex' file that does not
##' have a corresponding 'figure.tex' file.
##' @noRd
check_missing_figure_reference_files <- function()
{
    cat("* checking missing figure reference files ... ")

    chapters <- list.files("chapters", full.names = TRUE)
    ref <- unlist(lapply(chapters, function(chapter) {
        wd <- setwd(chapter)

        ## Get references for figure files
        ref <- references()
        ref <- ref[ref$cmd == "ref" & ref$reftype == "fig", ]

        ## Expected files from figure references: 'fig:chapter:id'
        if (nrow(ref)) {
            id <- sapply(strsplit(ref$marker, ":"), "[", 3)
            filename <- paste0(chapter, "/fig_", normalize_title(basename(chapter)), "_", id, ".tex")
            ref_fig_files <- filename
        } else {
            ref_fig_files <- character(0)
        }

        ## Observed tex files
        fig_files <- paste0(chapter, "/", list.files(pattern = "tex$"))

        setwd(wd)

        setdiff(ref_fig_files, fig_files)
    }))

    if (length(ref)) {
        cat("ERROR\n    ")
        cat(ref, sep = "\n    ")
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}

##' Check for missing table reference files
##'
##' Check for table references in the 'text.tex' file that does not
##' have a corresponding 'table.tex' file.
##' @noRd
check_missing_table_reference_files <- function()
{
    cat("* checking missing table reference files ... ")

    chapters <- list.files("chapters", full.names = TRUE)
    ref <- unlist(lapply(chapters, function(chapter) {
        wd <- setwd(chapter)

        ## Get references for table files
        ref <- references()
        ref <- ref[ref$cmd == "ref" & ref$reftype == "tab", ]

        ## Expected files from table references: 'tab:chapter:id'
        if (nrow(ref)) {
            id <- sapply(strsplit(ref$marker, ":"), "[", 3)
            filename <- paste0(chapter, "/tab_", id, ".tex")
            ref_tab_files <- filename
        } else {
            ref_tab_files <- character(0)
        }

        ## Observed tex files
        tab_files <- paste0(chapter, "/", list.files(pattern = "tex$"))

        setwd(wd)

        setdiff(ref_tab_files, tab_files)
    }))

    if (length(ref)) {
        cat("ERROR\n    ")
        cat(ref, sep = "\n    ")
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}

##' Check range character format
##'
##' Checking that the range character is '--' and not '-'
##' @noRd
check_range_character <- function()
{
    cat("* checking range character ... ")

    ## Identify for example '2016-2017' but exclude '3-19-11-N-311' or
    ## '{1-1}'.
    pattern <- "(?<!(-|\\d|{))\\d+-\\d+(?!(-|\\d|}))"

    ## List all tex files
    tex_files <- list.files("chapters", pattern = "[.]tex$",
                            recursive = TRUE, full.names = TRUE)

    ## Drop 'typeset.tex'
    tex_files <- tex_files[!(basename(tex_files) %in% "typeset.tex")]

    l <- sapply(tex_files, function(filename) {
        lines <- readLines(filename)

        ## Find the line for the reference section. Use the complete
        ## text if the file doesn't contain a reference section.
        i <- grep("^\\\\section[*][{]References[}]", lines)
        if (!length(i))
            i <- length(lines)
        stopifnot(identical(length(i), 1L))
        lines <- lines[seq_len(i)]

        lines <- grep(pattern, lines, perl = TRUE)
        length(lines) > 0
    })

    l <- tex_files[l]
    if (length(l)) {
        cat("ERROR\n")
        lapply(l, function(filename) {
            cat("   ", filename, "  line(s): ")
            lines <- grep(pattern, readLines(filename), perl = TRUE)
            lines <- paste(lines, collapse = ", ")
            cat(lines, "\n")
        })
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}

##' Check for thousand separator
##'
##' @noRd
check_thousand_separator <- function()
{
    cat("* checking thousand separator ... ")

    pattern <- "[0-9]\\s+[0-9]"

    ## List all tex files.
    tex_files <- list.files("chapters", pattern = "[.]tex$",
                            recursive = TRUE, full.names = TRUE)

    ## Drop 'typeset.tex' and 'fig_*.tex' files.
    tex_files <- tex_files[!(basename(tex_files) %in% "typeset.tex")]
    tex_files <- tex_files[!startsWith(basename(tex_files), "fig_")]

    l <- sapply(tex_files, function(filename) {
        lines <- grep(pattern, readLines(filename))
        length(lines) > 0
    })

    l <- tex_files[l]
    if (length(l)) {
        cat("ERROR\n")
        lapply(l, function(filename) {
            cat("   ", filename, "  line(s): ")
            lines <- grep(pattern, readLines(filename))
            lines <- paste(lines, collapse = ", ")
            cat(lines, "\n")
        })
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}

##' Utility function to check for a pattern in tex-files.
##'
##' @noRd
check_pattern <- function(pattern, description, perl = FALSE)
{
    cat(paste("*", description, "... "))

    ## List all tex files.
    tex_files <- list.files("chapters", pattern = "[.]tex$",
                            recursive = TRUE, full.names = TRUE)

    ## Drop 'typeset.tex'.
    tex_files <- tex_files[!(basename(tex_files) %in% "typeset.tex")]

    l <- sapply(tex_files, function(filename) {
        lines <- grep(pattern, readLines(filename), perl = perl)
        length(lines) > 0
    })

    l <- tex_files[l]
    if (length(l)) {
        cat("ERROR\n")
        lapply(l, function(filename) {
            cat("   ", filename, "  line(s): ")
            lines <- grep(pattern, readLines(filename), perl = perl)
            lines <- paste(lines, collapse = ", ")
            cat(lines, "\n")
        })
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}

##' Utility function to check for highlights in patch-files.
##'
##' @noRd
check_highlight <- function()
{
    cat("* checking for highlights in patches ...")

    ## List all patch files.
    patch_files <- list.files("chapters", pattern = "[.]patch$",
                              recursive = TRUE, full.names = TRUE)

    l <- sapply(patch_files, function(filename) {
        lines <- grep("[\\]hl[{]", readLines(filename))
        length(lines) > 0
    })

    l <- patch_files[l]
    if (length(l)) {
        cat("ERROR\n")
        lapply(l, function(filename) {
            cat("   ", filename, "\n")
        })
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}
