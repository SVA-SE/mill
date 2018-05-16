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
    cat(capture.output(report), sep = "\n    ")

    if (check_expect_clean_repository(report))
        return(invisible(TRUE))

    result <- check_tex_to_docx_round_trip(report)
    result <- c(result, check_apply_typeset_patch(report))
    result <- c(result, check_reference_format(report))
    result <- c(result, check_missing_figure_reference_files(report))
    result <- c(result, check_missing_table_reference_files(report))

    invisible(any(result))
}

##' Check that repository is clean
##'
##' To protect against overwriting un-committed changes.
##' @importFrom git2r diff
##' @importFrom git2r repository
##' @keywords internal
check_expect_clean_repository <- function() {
    cat("* checking that repository is clean ... ")

    ## Check if the working tree is clean
    d <- diff(repository())
    if (length(d@files)) {
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
##' @keywords internal
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
##' @keywords internal
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
##' @keywords internal
check_tex_to_docx_round_trip <- function(x)
    UseMethod("check_tex_to_docx_round_trip")

##' @keywords internal
##' @importFrom git2r reset
##' @importFrom git2r commits
check_tex_to_docx_round_trip.report <- function(x) {
    on.exit(reset(commits(repository(), n = 1)[[1]], "hard"))
    cat("* checking 'tex' to 'docx' round trip ... ")

    l <- check_tex_to_docx_round_trip(chapters(x))
    l <- l[!sapply(l, is.null)]
    if (length(l)) {
        cat("ERROR\n")
        lapply(l, function(filename) cat("   ", filename, "\n"))
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}

##' @keywords internal
check_tex_to_docx_round_trip.chapters <- function(x) {
    sapply(x$section, function(y) check_tex_to_docx_round_trip(y))
}

##' @keywords internal
##' @importFrom git2r commits
##' @importFrom git2r reset
##' @importFrom git2r status
check_tex_to_docx_round_trip.chapter <- function(x) {
    to_docx(x)
    from_docx(x)
    unstaged <- unlist(status(repository())$unstaged)
    if (file.path(chapter_path(x), "text.tex") %in% unstaged)
        return(file.path(chapter_path(x), "text.tex"))
    NULL
}

##' Check apply patch 'typeset.patch' to 'text.tex'
##'
##' Checking that applying patches doesn't generate warnings or
##' errors.
##' @keywords internal
check_apply_typeset_patch <- function(x)
    UseMethod("check_apply_typeset_patch")

##' @keywords internal
check_apply_typeset_patch.report <- function(x) {
    cat("* checking apply typeset patch ... ")

    l <- check_apply_typeset_patch(chapters(x))
    l <- l[!sapply(l, is.null)]
    if (length(l)) {
        cat("ERROR\n")
        lapply(l, function(filename) cat("   ", filename, "\n"))
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}

##' @keywords internal
check_apply_typeset_patch.chapters <- function(x) {
    sapply(x$section, function(y) check_apply_typeset_patch(y))
}

##' @keywords internal
check_apply_typeset_patch.chapter <- function(x) {
    owd <- setwd(chapter_path(x))
    on.exit(setwd(owd))
    output <- tryCatch(system2("patch",
                               args = c("text.tex", "-i", "typeset.patch",
                                        "-o", "typeset.tex"),
                               stdout = TRUE, stderr = TRUE),
                       warning = function(w) w)
    if (identical(output,
                  "patching file typeset.tex (read from text.tex)"))
        return(NULL)
    return(file.path(chapter_path(x), "typeset.patch"))
}

##' Check reference format
##'
##' @param x the report or chapter object.
##' @keywords internal
check_reference_format <- function(x)
    UseMethod("check_reference_format")

check_reference_format.report <- function(x) {
    cat("* checking reference format ... ")

    ref <- references(x)
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
##' @param x the report or chapter object.
##' @keywords internal
check_missing_figure_reference_files <- function(x)
    UseMethod("check_missing_figure_reference_files")

check_missing_figure_reference_files.report <- function(x) {
    cat("* checking missing figure reference files ... ")

    ref <- check_missing_figure_reference_files(chapters(x))
    if (length(ref)) {
        cat("ERROR\n    ")
        cat(ref, sep = "\n    ")
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}

check_missing_figure_reference_files.chapters <- function(x) {
    unlist(lapply(x$section, function(y) {
        wd <- setwd(chapter_path(y))
        ref <- check_missing_figure_reference_files(y)
        setwd(wd)
        ref
    }))
}

check_missing_figure_reference_files.chapter <- function(x) {
    ## Get references for figure files
    ref <- references(x)
    ref <- ref[ref$cmd == "ref" & ref$reftype == "fig", ]

    ## Expected files from figure references: 'fig:chapter:id'
    if (nrow(ref)) {
        id <- sapply(strsplit(ref$marker, ":"), "[", 3)
        filename <- paste0("fig_", normalize_title(x$title), "_", id, ".tex")
        ref_fig_files <- file.path(chapter_path(x), filename)
    } else {
        ref_fig_files <- character(0)
    }

    ## Observed tex files
    fig_files <- list.files(path = chapter_path(x),
                            pattern = "tex$", full.names = TRUE)

    setdiff(ref_fig_files, fig_files)
}

##' Check for missing table reference files
##'
##' Check for table references in the 'text.tex' file that does not
##' have a corresponding 'table.tex' file.
##' @param x the report or chapter object.
##' @keywords internal
check_missing_table_reference_files <- function(x)
    UseMethod("check_missing_table_reference_files")

check_missing_table_reference_files.report <- function(x) {
    cat("* checking missing table reference files ... ")

    ref <- check_missing_table_reference_files(chapters(x))
    if (length(ref)) {
        cat("ERROR\n    ")
        cat(ref, sep = "\n    ")
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}

check_missing_table_reference_files.chapters <- function(x) {
    unlist(lapply(x$section, function(y) {
        wd <- setwd(chapter_path(y))
        ref <- check_missing_table_reference_files(y)
        setwd(wd)
        ref
    }))
}

check_missing_table_reference_files.chapter <- function(x) {
    ## Get references for table files
    ref <- references(x)
    ref <- ref[ref$cmd == "ref" & ref$reftype == "tab", ]

    ## Expected files from table references: 'tab:chapter:id'
    if (nrow(ref)) {
        id <- sapply(strsplit(ref$marker, ":"), "[", 3)
        filename <- paste0("tab_", normalize_title(x$title), "_", id, ".tex")
        ref_tab_files <- file.path(chapter_path(x), filename)
    } else {
        ref_tab_files <- character(0)
    }

    ## Observed tex files
    tab_files <- list.files(path = chapter_path(x),
                            pattern = "tex$", full.names = TRUE)

    setdiff(ref_tab_files, tab_files)
}
