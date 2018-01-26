##' Intialize the report folder structure
##'
##' @param path The path to the root folder of the project.
##' @param import Import chapters from another report, where
##'     \code{import} is the path to the root folder of the other
##'     report.
##' @param force The method fails if the folder structure already
##'     exists and force equals FALSE.
##' @importFrom git2r commit
##' @importFrom git2r init
##' @export
init_report <- function(path = ".", import = NULL, force = FALSE) {
    report <- load_report(path)

    init_clean(file.path(report$path, ".git"), force)
    init_clean(file.path(report$path, ".gitignore"), force)
    init_clean(file.path(report$path, "assets"), force)
    init_clean(file.path(report$path, "chapters"), force)
    init_clean(file.path(report$path, "Makefile"), force)
    init_clean(file.path(report$path, "report.org"), force)

    repo <- init(report$path)
    do_init(report, repo, import)
    commit(repo, "Initial repository")

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

##' Initialize a report structure
##'
##' @param x The report object to initialize.
##' @param repo The git repository to add the report to.
##' @param import The root path to another report to import chapters
##'     from.
##' @return invisible NULL.
##' @importFrom git2r add
##' @keywords internal
do_init <- function(x, repo, import) UseMethod("do_init")

##' @keywords internal
##' @importFrom git2r add
do_init.report <- function(x, repo, import) {
    do_init(x$chapters, repo, import)

    create_Makefile(x)

    add(repo, file.path(x$path, "report.xlsx"))

    filename <- file.path(x$path, ".gitignore")
    writeLines(".Rproj.user", con = filename)
    add(repo, filename)

    filename <- file.path(x$path, "report.Rproj")
    writeLines(Rproj_file(), con = filename)
    add(repo, filename)

    filename <- file.path(x$path, "report.org")
    writeLines(to_orgmode(x), con = filename)
    add(repo, filename)

    ## Latex snippets
    dst <- file.path(x$path, "assets", "latex")
    src <- file.path(import, "assets", "latex")
    dir.create(dst, recursive = TRUE)
    files <- list.files(path = src)
    sapply(files, function(filename) {
        file.copy(file.path(src, filename), file.path(dst, filename))
        add(repo, file.path(dst, filename))
    })

    ## Cover
    dst <- file.path(x$path, "assets", "cover")
    src <- file.path(import, "assets", "cover")
    dir.create(dst, recursive = TRUE)
    files <- list.files(path = src)
    sapply(files, function(filename) {
        file.copy(file.path(src, filename), file.path(dst, filename))
        add(repo, file.path(dst, filename))
    })

    ## Front-matter
    dst <- file.path(x$path, "assets", "front-matter")
    src <- file.path(import, "assets", "front-matter")
    dir.create(dst, recursive = TRUE)
    files <- list.files(path = src)
    sapply(files, function(filename) {
        file.copy(file.path(src, filename), file.path(dst, filename))
        add(repo, file.path(dst, filename))
    })

    ## Back-matter
    dst <- file.path(x$path, "assets", "back-matter")
    src <- file.path(import, "assets", "back-matter")
    dir.create(dst, recursive = TRUE)
    files <- list.files(path = src)
    sapply(files, function(filename) {
        file.copy(file.path(src, filename), file.path(dst, filename))
        add(repo, file.path(dst, filename))
    })

    invisible()
}

##' @keywords internal
do_init.chapters <- function(x, repo, import) {
    lapply(x, function(y) do_init(y, repo, import))
    invisible()
}

##' @keywords internal
##' @importFrom git2r add
do_init.chapter <- function(x, repo, import) {
    dir.create(x$path, recursive = TRUE)

    filename <- file.path(x$path, ".gitignore")
    writeLines("auto", con = filename)
    writeLines("text.log", con = filename)
    add(repo, filename)

    filename <- file.path(x$path, "text.tex")
    if (!import_from(import, x$path, x$title, "text.tex"))
        writeLines(lorem_ipsum(x$title), con = filename)
    add(repo, filename)

    import_text_input(x, repo, import, figure_pattern())
    import_text_input(x, repo, import, "^table-[^.]+[.]tex$")
    import_text_input(x, repo, import, "^tab_[^.]+[.]tex$")
    import_text_input(x, repo, import, "^fig_[^.]+[.]tex$")
    import_text_input(x, repo, import, "^img_[^.]+[.]png2$")
    import_text_input(x, repo, import, "^typeset[.]patch$")

    invisible()
}

##' @keywords internal
##' @importFrom git2r add
import_text_input <- function(x, repo, import, pattern) {
    if (!is.null(import)) {
        files <- list.files(path = file.path(import, "chapters", x$title),
                            pattern = pattern)
        lapply(files, function(filename) {
            file.copy(file.path(import, "chapters", x$title, filename),
                      file.path(x$path, filename))
            add(repo, file.path(x$path, filename))
        })
    }
}

##' @keywords internal
Rproj_file <- function() {
    c("Version: 1.0",
      "",
      "RestoreWorkspace: No",
      "SaveWorkspace: No",
      "AlwaysSaveHistory: No",
      "",
      "EnableCodeIndexing: Yes",
      "UseSpacesForTab: Yes",
      "NumSpacesForTab: 2",
      "Encoding: UTF-8",
      "",
      "RnwWeave: Sweave",
      "LaTeX: pdfLaTeX")
}

##' Import a chapter from another report
##'
##' @param from The path to the root folder of the other report to
##'     import the chapter from.
##' @param to The path of the chapter to import to.
##' @param title The title of the chapter.
##' @param filename The filename of the file to import.
##' @return TRUE if the file to import existed, else FALSE.
##' @keywords internal
import_from <- function(from, to, title, filename) {
    if (!is.null(from)) {
        f_to <- file.path(to, filename)

        f_from <- file.path(from, "chapters", title, filename)
        if (file.exists(f_from)) {
            file.copy(f_from, f_to)
            return(TRUE)
        }

        ## Check if spaces have been replaced with underscores.
        title <- gsub("[[:space:]]", "_", title)
        f_from <- file.path(from, "chapters", title, filename)
        if (file.exists(f_from)) {
            file.copy(f_from, f_to)
            return(TRUE)
        }
    }

    return(FALSE)
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
