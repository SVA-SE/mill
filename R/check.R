##' Clear checks
##'
##' Write a new CHECKLIST file in the report root and in each
##' chapter. To ignore a check, add the check id of the check to the
##' CHECKLIST file. Checks can either be ignored on a global level by
##' adding the id to the root CHECKLIST or on a chapter level by
##' adding the id to a specific CHECKLIST chapter file. Comments are
##' allowed in a CHECKLIST file, any text from a \code{#} character to
##' the end of the line is taken to be a comment.
##' @export
clear_checks <- function() {
    if (in_chapter()) {
        ## Add a CHECKLIST file in the chapter folder.
        filename <- "CHECKLIST"
        unlink(filename)
        file.create(filename)
        git2r::add(repository(), filename)

        return(invisible(NULL))
    }

    if (in_report()) {
        ## Add a CHECKLIST file in the root folder.
        filename <- "CHECKLIST"
        unlink(filename)
        file.create(filename)
        git2r::add(repository(), filename)

        ## Make sure to add a CHECKLIST file in each chapter.
        lapply(list.files("chapters", full.names = TRUE), function(filename) {
            filename <- paste0(filename, "/CHECKLIST")
            unlink(filename)
            file.create(filename)
            git2r::add(repository(), filename)
        })

        return(invisible(NULL))
    }

    stop("Must be in the report root folder or in a chapter to clear checks.")
}

##' Read and parse a CHECKLIST file
##'
##' @param filename name of the CHECKLIST file.
##' @param ignore numeric vector with ids to append to the ones found
##'     in the CHECKLIST file.
##' @return a numeric vector or NULL.
##' @noRd
read_checklist <- function(filename, ignore = NULL) {
    if (!file.exists(filename))
        return(ignore)

    x <- readLines(filename)
    x <- sub("#.*", "", x)
    x <- trimws(x)
    x <- c(ignore, as.numeric(x))
    x <- x[!is.na(x)]
    if (length(x))
        return(sort(x))

    NULL
}

##' Get a list with chapter checks to skip
##' @noRd
checklist <- function() {
    ## Load report CHECKLIST to find checks to ignore on a report
    ## level.
    report_ignore <- NULL
    if (in_report())
        report_ignore <- read_checklist("CHECKLIST")

    ## Load chapter CHECKLIST to find checks to ignore
    CHECKLIST <- list.files(pattern = "^CHECKLIST$", recursive = TRUE)
    ignore <- lapply(CHECKLIST, read_checklist, ignore = report_ignore)

    if (in_chapter()) {
        names(ignore) <- basename(getwd())
    } else {
        names(ignore) <- basename(dirname(CHECKLIST))
    }

    ignore[!vapply(ignore, is.null, logical(1))]
}

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

    if (check_pandoc_is_installed())
        return(invisible(TRUE))
    if (check_patch_is_installed())
        return(invisible(TRUE))

    cat("* checking report ... \n    ")
    cat(capture.output(repository()), sep = "\n    ")

    if (check_expect_clean_repository())
        return(invisible(TRUE))

    cat("** To skip a test, add the check id to the chapter CHECKLIST **\n")
    ignore <- checklist()
    id <- 0
    result <- check_open_track_changes(id <- id + 1)
    result <- c(result, check_apply_typeset_patch(id <- id + 1))
    result <- c(result, check_reference_format(id <- id + 1))
    result <- c(result, check_figure_reference_files(id <- id + 1))
    result <- c(result, check_table_reference_files(id <- id + 1))

    ## Checking that the range character is '--' and not
    ## '-'. Identify, for example, '2016-2017' but exclude
    ## '3-19-11-N-311' or '{1-1}'.
    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "(?<!(-|\\d|{|}))\\d+-\\d+(?!(-|\\d|}|/|[.]\\d))",
                    "checking range character",
                    perl = TRUE))

    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "(?<=\\s)\\d+,\\d+(?!:)",
                    "checking for comma as decimal separator",
                    perl = TRUE,
                    patches = FALSE))

    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "[0-9]\\s+[0-9]",
                    "checking thousand separator"))

    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "[.]\\s*[.]",
                    "checking multiple dots"))

    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "[,]\\s*[,]",
                    "checking multiple commas"))

    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "\u00b4",
                    "checking for incorrect apostrophe character"))

    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "[/]\\s*[\\]numprint[{]100000[}]",
                    "checking for incorrect 'per 100000 inhabitants'"))

    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "[0-9]\\s+[\\][%]",
                    "checking for space between digit and '%'"))

    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "http[:]//",
                    "checking for 'http://'"))

    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "2018",
                    "checking for '2018'"))

    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "(?<![a-z\\{])figure(?![a-z])",
                    "checking for lowercase 'figure'",
                    perl = TRUE))

    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "(?<![a-z\\{])table(?![a-z])",
                    "checking for lowercase 'table'",
                    perl = TRUE))

    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "[fF]ig(?![u:_])",
                    "checking for shortform of figure",
                    perl = TRUE))

    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "(?<![a-z\\{])[tT]ab(?![l:_])",
                    "checking for shortform of table",
                    perl = TRUE))

    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "[\\][^{]*[{]\\s",
                    paste0("checking for whitespace at ",
                           "beginning of '\\command{ text}'")))

    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "[\\][^{]*[{][^}]*(?<=\\s)[}]",
                    "checking for whitespace at end of '\\commad{text }'",
                    perl = TRUE))

    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "[\\][^{]*[{].[}][.]",
                    paste0("checking for one character command ",
                           "followed by '.' e.g. '\\textit{S}.'")))

    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "[\\]hl[{]",
                    "checking for highlights"))

    result <- c(result,
                check_pattern(
                    id <- id + 1, ignore,
                    "\\scounty\\s[^o]",
                    paste0("checking usage of county e.g. ",
                           "'Uppsala county' (use 'county of')")))

    result <- c(result,
                check_incomplete_line(
                    id <- id + 1, ignore))

    invisible(any(result))
}

check_chapters <- function(id = NULL, ignore = NULL, patches = NULL) {
    if (in_chapter())
        return(".")
    list.files("chapters", full.names = TRUE)
}

##' Check that repository is clean
##'
##' To protect against overwriting un-committed changes.
##' @importFrom git2r diff
##' @importFrom git2r repository
##' @importFrom git2r status
##' @noRd
check_expect_clean_repository <- function() {
    cat("* checking that repository is clean ... ")

    ## Check if the working tree is clean
    s <- status(repository())
    if (length(s$unstaged) || length(s$staged)) {
        cat("ERROR\n")
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}

##' Check that pandoc is installed
##'
##' pandoc is required to check for example 'tex' to 'docx' round
##' trip.
##' @noRd
check_pandoc_is_installed <- function() {
    cat("* checking that 'pandoc' is installed ... ")

    output <- tryCatch(system("pandoc --version", intern = TRUE,
                              ignore.stderr = TRUE),
                       error = function(e) character(0))
    ver <- grep("^pandoc(\\.exe)*[[:space:]]+[.0-9]*", output)
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
check_patch_is_installed <- function() {
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

##' Check for open track changes in each chapter docx-file.
##'
##' @noRd
check_open_track_changes <- function(id) {
    cat(sprintf("[%02i] checking for open track changes ... ", id))

    l <- sapply(check_chapters(), function(chapter) {
        if (in_chapter()) {
            filename <- paste0("../../workspace/chapters/",
                               basename(getwd()), "/",
                               basename(getwd()), ".docx")
        } else {
            filename <- paste0("workspace/chapters/",
                               basename(chapter), "/",
                               basename(chapter), ".docx")
        }

        if (!file.exists(filename))
            stop("Missing file:", filename)

        ## Unzip the content of the word file.
        on.exit(unlink(file.path(tempdir(), "document.xml")), add = TRUE)
        unzip(filename, "word/document.xml",
              junkpaths = TRUE, exdir = tempdir())

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
check_apply_typeset_patch <- function(id) {
    cat(sprintf("[%02i] checking apply typeset patch ... ", id))

    l <- sapply(check_chapters(), function(chapter) {
        wd <- setwd(chapter)
        result <- tryCatch(apply_patch(verbose = FALSE),
                           error = function(e) chapter)
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
check_reference_format <- function(id, x) {
    cat(sprintf("[%02i] checking reference format ... ", id))

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
check_figure_reference_files <- function(id) {
    cat(sprintf("[%02i] checking missing figure reference files ... ", id))

    ref <- unlist(lapply(check_chapters(), function(chapter) {
        wd <- setwd(chapter)

        ## Get references for figure files
        ref <- references()
        ref <- ref[ref$cmd == "ref" & ref$reftype == "fig", ]

        ## Expected files from figure references: 'fig:chapter:id'
        if (nrow(ref)) {
            id <- sapply(strsplit(ref$marker, ":"), "[", 3)
            filename <- paste0(chapter, "/fig_",
                               id, ".tex")
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
check_table_reference_files <- function(id) {
    cat(sprintf("[%02i] checking missing table reference files ... ", id))

    ref <- unlist(lapply(check_chapters(), function(chapter) {
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

##' Utility function to check for a pattern in tex-files.
##'
##' @noRd
check_pattern <- function(id,
                          ignore,
                          pattern,
                          description,
                          perl = FALSE,
                          patches = TRUE) {
    cat(sprintf("[%02i] %s ... ", id, description))

    ## List files.
    if (isTRUE(patches)) {
        file_pattern <- "[.](tex|patch)$"
    } else {
        file_pattern <- "[.]tex$"
    }

    if (in_chapter()) {
        ignore <- as.numeric(ignore[[basename(getwd())]])
        if (id %in% ignore) {
            files <- character(0)
        } else {
            files <- list.files(pattern = file_pattern)
        }
    } else {
        files <- unlist(lapply(list.files("chapters"), function(chapter) {
            if (id %in% as.numeric(ignore[[chapter]]))
                return(character(0))
            list.files(paste0("chapters/", chapter),
                       pattern = file_pattern,
                       recursive = TRUE,
                       full.names = TRUE)
        }))
    }

    l <- vapply(files, function(filename) {
        lines <- grep(pattern, readLines(filename), perl = perl)
        length(lines) > 0
    }, logical(1), USE.NAMES = FALSE)

    l <- files[l]
    if (length(l)) {
        cat("ERROR\n")
        lapply(l, function(filename) {
            cat("     -", filename, "  line(s): ")
            lines <- grep(pattern, readLines(filename), perl = perl)
            lines <- paste(lines, collapse = ", ")
            cat(lines, "\n")
        })
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}

##' Check for incomplete final line in 'typeset.tex'
##'
##' @noRd
check_incomplete_line <- function(id, ignore) {
    cat(sprintf(
        "[%02i] checking for incomplete final line in tex-files ... ",
        id))

    if (in_chapter()) {
        ignore <- as.numeric(ignore[[basename(getwd())]])
        if (id %in% ignore) {
            files <- character(0)
        } else {
            files <- list.files(pattern = "[.]tex$")
        }
    } else {
        files <- unlist(lapply(list.files("chapters"), function(chapter) {
            if (id %in% as.numeric(ignore[[chapter]]))
                return(character(0))
            list.files(paste0("chapters/", chapter),
                       pattern = "[.]tex$",
                       recursive = TRUE,
                       full.names = TRUE)
        }))
    }

    l <- vapply(files, function(filename) {
        result <- FALSE
        tryCatch(readLines(filename),
                 warning = function(w) {
                     if (startsWith(w$message, "incomplete final line"))
                         result <<- TRUE
        })
        result
    }, logical(1), USE.NAMES = FALSE)

    l <- files[l]
    if (length(l)) {
        cat("ERROR\n")
        lapply(l, function(filename) {
            cat("     -", filename, "\n")
        })
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}
