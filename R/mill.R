##' @keywords internal
authors <- function(x) {
    stopifnot(inherits(x, "chapter"))
    for (i in seq_len(length(x$section))) {
        if (inherits(x$section[[i]], "authors"))
            return(x$section[[i]])
    }

    stop("Unable to find 'Authors'")
}

##' @noRd
chapters <- function(x) {
    stopifnot(inherits(x, "report"))
    for (i in seq_len(length(x$contents))) {
        if (inherits(x$contents[[i]], "chapters"))
            return(x$contents[[i]])
    }

    stop("Unable to find 'Chapters'")
}

##' Load configuration for the report
##'
##' @param path The path to the root folder of the project.
##' @export
load_report <- function(path = ".") {
    path <- normalizePath(path, mustWork = TRUE)
    filename <- file.path(path, "README.org")
    org <- org_doc(readLines(filename))
    class(org) <- c("report", class(org))

    ii <- length(org$contents)
    for (i in seq_len(ii)) {
        if (inherits(org$contents[[i]], "org_headline")) {
            if (identical(grep("Chapters", org$contents[[i]]$headline), 1L)) {
                cl <- c("chapters", "org_headline")
                class(org$contents[[i]]) <- cl

                jj <- length(org$contents[[i]]$section)
                for (j in seq_len(jj)) {
                    stopifnot(inherits(org$contents[[i]]$section[[j]],
                                       "org_headline"))
                    cl <- c("chapter", "org_headline")
                    class(org$contents[[i]]$section[[j]]) <- cl

                    kk <- length(org$contents[[i]]$section[[j]]$section)
                    for (k in seq_len(kk)) {
                        d <- org$contents[[i]]$section[[j]]$section[[k]]
                        if (inherits(d, "org_drawer") && d$name == "AUTHORS") {
                            cl <- c("authors", "org_drawer")
                            class(org$contents[[i]]$section[[j]]$section[[k]]) <- cl

                            break;
                        }

                        if (identical(k, kk))
                            stop("Unable to find 'Authors'")
                    }
                }

                break
            }
        }

        if (identical(i, ii))
            stop("Unable to find 'Chapters'")
    }

    org
}

##' @method summary report
##' @export
summary.report <- function(object, ...) {
    cat("Report: ", object$report, "\n\n", sep = "")
    print(chapters(object))
}

##' @export
print.report <- function(x, ...) {
    cat("Report: ", x$report, "\n", sep = "")
    do.call("rbind", lapply(x, function(y) as.data.frame(y)))

    ## Authors
    authors <- lapply(x$chapters, function(chapter) {
        sapply(chapter$authors, function(author) {
            author$name
        })
    })
    authors <- unique(unlist(authors))
    cat("Authors: ", length(authors), "\n", sep = "")

    cat("Chapters: ", length(chapters(x)$section), "\n", sep = "")
}

##' @export
print.author <- function(x, ..., indent = "") {
    cat(indent,
        x$name, " [", x$organisation, "] <", x$email, ">\n", sep = "")
}

##' @export
print.chapters <- function(x, ...) {
    cat("Chapters:\n")
    lapply(x$section, print, indent = "  ")
    invisible()
}

##' @noRd
chapter_state <- function(x) {
    stopifnot(inherits(x, "chapter"))
    m <- regexpr("[^[:space:]]+", x$headline)
    m <- regmatches(x$headline, m)
    trimws(m)
}

##' @noRd
chapter_title <- function(x) {
    stopifnot(inherits(x, "chapter"))
    m <- regexpr("[^[]+[]]{2}$", x$headline)
    m <- regmatches(x$headline, m)
    trimws(sub("[]]{2}$", "", m))
}

##' @export
print.chapter <- function(x, ..., indent = "") {
    cat(indent, chapter_state(x), " ", chapter_title(x), "\n", sep = "")
    ## print(x$authors, indent = paste0(indent, "  "))
    ## cat("\n")
}

##' @export
print.authors <- function(x, ..., indent = "") {
    cat(indent, "Authors:\n", sep = "")
    lapply(x$items, print, indent = paste0(indent, "  "))
    invisible()
}

##' @export
as.data.frame.report <- function(x, ...) {
    as.data.frame(x$chapters)
}

as.data.frame.chapters <- function(x, ...) {
    do.call("rbind", lapply(x, function(y) as.data.frame(y)))
}

as.data.frame.chapter <- function(x, ...) {
    cbind(chapter = x$title, as.data.frame(x$authors))
}

as.data.frame.authors <- function(x) {
    cbind(role = "Author",
          do.call("rbind", lapply(x, function(y) as.data.frame(y))))
}

as.data.frame.contributor <- function(x) {
    data.frame(name = x$name, email = x$email, organisation = x$organisation)
}

##' @export
`[.report` <- function(x, i) {
    if (is.character(i))
        i <- grep(i, sapply(x$chapters, "[", "title"), ignore.case = TRUE)
    x$chapters[i]
}

##' @export
`[[.report` <- function(x, i) {
    if (is.character(i))
        i <- grep(i, sapply(x$chapters, "[", "title"), ignore.case = TRUE)
    stopifnot(identical(length(i), 1L))
    x$chapters[[i]]
}
