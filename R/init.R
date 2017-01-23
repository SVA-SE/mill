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
    init_clean(file.path(path, "report.org"), force)

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

    filename <- file.path(path, "report.org")
    writeLines(to_orgmode(x), con = filename)
    git2r::add(repo, filename)

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

    filename <- file.path(path, "text.tex")
    writeLines(lorem_ipsum(x$title), con = filename)
    git2r::add(repo, filename)

    invisible()
}

##' @keywords internal
lorem_ipsum <- function(title) {
    c(paste0("\\chapter*{", title, "}"),
      "",
      "\\section*{Heading 1}",
      "",
      lorem_ipsum_paragraph(),
      "",
      "\\subsection*{Heading 2}",
      "",
      lorem_ipsum_paragraph())
}

##' @keywords internal
lorem_ipsum_paragraph <- function() {
    c("Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do",
      "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut",
      "enim ad minim veniam, quis nostrud exercitation ullamco laboris",
      "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in",
      "reprehenderit in voluptate velit esse cillum dolore eu fugiat",
      "nulla pariatur. Excepteur sint occaecat cupidatat non proident,",
      "sunt in culpa qui officia deserunt mollit anim id est laborum.")
}

##' @keywords internal
to_orgmode <- function(x) UseMethod("to_orgmode")

to_orgmode.report <- function(x) {
    c("#+TODO: TODO(t!) UPDATE(u!) SUBMITTED(s!) CONVERTED(c!) AUTHOR-FEEDBACK(a!) TYPESET(y!) PROOF(p!) AUTHOR-OK(o!) REVIEW(r!) EDITOR(e!) | DONE(d@/!)",
      "#+STARTUP: indent",
      "#+STARTUP: hidestars",
      paste0("#+TITLE: ", x$report),
      "",
      "* Time",
      "#+BEGIN: clocktable :maxlevel 2 :scope file",
      "#+CAPTION: Clock summary at [2015-12-20 Sun 21:28]",
      "| Headline     | Time   |",
      "|--------------+--------|",
      "| *Total time* | *0:00* |",
      "#+END:",
      "",
      to_orgmode(x$chapters))
}

to_orgmode.chapters <- function(x) {
    c("* Chapters [%]", sapply(x, function(y) to_orgmode(y)))
}

to_orgmode.chapter <- function(x) {
    paste0("** TODO ", x$title)
}
