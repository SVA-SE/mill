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
