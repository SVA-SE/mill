##' Intialize the report folder structure
##'
##' @param path The path to the root folder of the project.
##' @param force The method fails if the folder structure already
##'     exists and force equals FALSE.
##' @export
init_report <- function(path = ".", force = FALSE) {
    path <- normalizePath(path, mustWork = TRUE)
    report <- load_report(path)

    init_clean(file.path(path, "chapters"), force)
    init_clean(file.path(path, ".git"), force)

    repo <- git2r::init(path)
    do_init(report, path, repo)
    git2r::commit(repo, "Initial repository")

    invisible(report)
}

##' Clean folder structure before init
##'
##' @param path The path to the folder to remove.
##' @param force The method fails if the folder exists and force is
##'     not TRUE.
##' @return invisible NULL
##' @keywords internal
init_clean <- function(path, force) {
    if (file.exists(path)) {
        if (identical(force, TRUE)) {
            unlink(path, recursive = TRUE, force = TRUE)
        } else {
            stop("The report exists. Use 'force = TRUE' to re-create.")
        }
    }
    invisible()
}

##' @importFrom git2r init
##' @keywords internal
do_init <- function(x, path, repo) UseMethod("do_init")

do_init.report <- function(x, path, repo) {
    git2r::add(repo, file.path(path, "report.yml"))
    path <- file.path(path, "chapters")
    dir.create(path)
    do_init(x$chapters, path, repo)
    invisible()
}

do_init.chapters <- function(x, path, repo) {
    lapply(x, function(y) do_init(y, path, repo))
    invisible()
}

do_init.chapter <- function(x, path, repo) {
    path <- file.path(path, x$title)
    dir.create(path)

    lines <- c("Hello world")

    filename <- file.path(path, "text.tex")
    writeLines(lines, con = filename)
    git2r::add(repo, filename)

    invisible()
}
