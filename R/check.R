##' Check report
##'
##' @param path The path to the report.
##' @return invisible \code{FALSE} if OK, else invisible \code{TRUE}.
##' @export
check <- function(path = ".") {
    cat("* using 'relax' version", as.character(packageVersion("relax")), "\n")

    if (check_expect_pandoc_is_installed())
        return(invisible(TRUE))
    if (check_expect_readxl_is_installed())
        return(invisible(TRUE))

    cat("* loading report ... ")
    report <- tryCatch(load_report(path), error = function(e) NULL)
    if (is.null(report)) {
        cat("ERROR\n")
        return(invisible(TRUE))
    }
    cat("OK\n    ")
    cat(capture.output(report), sep = "\n    ")

    repo <- git2r::repository(report$path)
    if (check_expect_clean_repository(report, repo))
        return(invisible(TRUE))

    result <- check_tex_to_docx_round_trip(report, repo)
    result <- c(result, check_reference_format(report))
    result <- c(result, check_missing_figure_reference_files(report))
    result <- c(result, check_missing_table_reference_files(report))

    invisible(any(result))
}

##' Check that repository is clean
##'
##' To protect against overwriting un-committed changes.
##' @keywords internal
check_expect_clean_repository <- function(x, repo)
    UseMethod("check_expect_clean_repository")

##' @keywords internal
check_expect_clean_repository.report <- function(x, repo) {
    cat("* checking that repository is clean ... ")

    ## Check if the working tree is clean
    d <- git2r::diff(repo)
    if (length(d@files)) {
        cat("ERROR\n")
        return(TRUE)
    }

    cat("OK\n    ")
    cat(capture.output(git2r::repository()), sep = "\n    ")
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

##' Check that readxl is installed
##'
##' readxl is required to check consistency between 'report.yml' and
##' exportd 'Authors.xlsx'.
##' @keywords internal
check_expect_readxl_is_installed <- function() {
    cat("* checking that 'readxl' is installed ... ")

    if (!requireNamespace("readxl", quietly=TRUE)) {
        cat("ERROR\n")
        return(TRUE)
    }

    cat("OK\n    readxl", format(packageVersion("readxl")), "\n")
    FALSE
}

##' Check conversion between 'tex' and 'docx'
##'
##' Checking that converting to 'docx' from 'tex' and converting back
##' to 'tex' doesn't generate changes.
##' @keywords internal
check_tex_to_docx_round_trip <- function(x, repo)
    UseMethod("check_tex_to_docx_round_trip")

##' @keywords internal
check_tex_to_docx_round_trip.report <- function(x, repo) {
    on.exit(git2r::reset(git2r::commits(repo, n = 1)[[1]], "hard"))
    cat("* checking 'tex' to 'docx' round trip ... ")

    if (check_tex_to_docx_round_trip(x$chapters, repo)) {
        cat("ERROR\n")
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}

##' @keywords internal
check_tex_to_docx_round_trip.chapters <- function(x, repo) {
    any(sapply(x, function(y) check_tex_to_docx_round_trip(y, repo)))
}

##' @keywords internal
##' @importFrom git2r commits
##' @importFrom git2r reset
##' @importFrom git2r status
check_tex_to_docx_round_trip.chapter <- function(x, repo) {
    to_docx(x)
    from_docx(x)
    unstaged <- unlist(git2r::status(repo)$unstaged)
    if (file.path("chapters", x$title, "text.tex") %in% unstaged)
        return(TRUE)
    FALSE
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

    ref <- check_missing_figure_reference_files(x$chapters)
    if (length(ref)) {
        cat("ERROR\n    ")
        cat(sub(x$path, ".", ref), sep = "\n    ")
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}

check_missing_figure_reference_files.chapters <- function(x) {
    unlist(lapply(x, function(y) check_missing_figure_reference_files(y)))
}

check_missing_figure_reference_files.chapter <- function(x) {
    ## Get references for figure files
    ref <- references(x)
    ref <- ref[ref$cmd == "ref" & ref$reftype == "fig", ]

    ## Expected files from figure references: 'fig:chapter:id'
    if (nrow(ref)) {
        id <- sapply(strsplit(ref$marker, ":"), "[", 3)
        filename <- paste0("figure-", id, ".tex")
        ref_fig_files <- file.path(x$path, filename)
    } else {
        ref_fig_files <- character(0)
    }

    ## Observed figure files
    fig_files <- figure_files(x, "tex")

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

    ref <- check_missing_table_reference_files(x$chapters)
    if (length(ref)) {
        cat("ERROR\n    ")
        cat(sub(x$path, ".", ref), sep = "\n    ")
        return(TRUE)
    }

    cat("OK\n")
    FALSE
}

check_missing_table_reference_files.chapters <- function(x) {
    unlist(lapply(x, function(y) check_missing_table_reference_files(y)))
}

check_missing_table_reference_files.chapter <- function(x) {
    ## Get references for table files
    ref <- references(x)
    ref <- ref[ref$cmd == "ref" & ref$reftype == "tab", ]

    ## Expected files from table references: 'tab:chapter:id'
    if (nrow(ref)) {
        id <- sapply(strsplit(ref$marker, ":"), "[", 3)
        filename <- paste0("table-", id, ".tex")
        ref_tab_files <- file.path(x$path, filename)
    } else {
        ref_tab_files <- character(0)
    }

    ## Observed table files
    tab_files <- file.path(x$path, list.files(x$path, "^table-[^.]*[.]tex"))

    setdiff(ref_tab_files, tab_files)
}
