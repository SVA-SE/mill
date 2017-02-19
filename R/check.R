##' Check report
##'
##' @param path The path to the report.
##' @return invisible \code{FALSE} if OK, else invisible \code{TRUE}.
##' @export
check <- function(path = ".") {
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
    if (check_expect_pandoc_is_installed())
        return(invisible(TRUE))
    if (check_tex_to_docx_round_trip(report, repo))
        return(invisible(TRUE))
    if (check_reference_format(report))
        return(invisible(TRUE))
    if (check_missing_figure_reference_files(report))
        return(invisible(TRUE))
    if (check_missing_table_reference_files(report))
        return(invisible(TRUE))

    invisible(FALSE)
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

    ref_all <- references(x, "all")
    ref_fig <- references(x, "fig")
    ref_tab <- references(x, "tab")

    ref <- setdiff(ref_all, c(ref_fig, ref_tab))
    if (length(ref)) {
        cat("ERROR\n    ")
        cat(ref, sep = "\n    ")
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
    ## Expected figure files
    ref_fig <- references(x, "fig")
    ref_fig_files <- sub("\\\\ref[{]fig:[^:]+:([^}]+)[}]",
                         "figure-\\1.tex",
                         ref_fig)
    ref_fig_files <- file.path(x$path, ref_fig_files)

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
    ## Expected table files
    ref_tab <- references(x, "tab")
    ref_tab_files <- sub("\\\\ref[{]tab:[^:]+:([^}]+)[}]",
                         "table-\\1.tex",
                         ref_tab)
    ref_tab_files <- file.path(x$path, ref_tab_files)

    ## Observed table files
    tab_files <- file.path(x$path, list.files(x$path, "^table-[^.]*[.]tex"))

    setdiff(ref_tab_files, tab_files)
}