##' @export
authors <- function(x, ...) {
    UseMethod("authors")
}

##' @export
authors.report <- function(x, ...) {
    authors(chapters(x), ...)
}

##' @export
authors.chapters <- function(x, ...) {
    sort(unique(unlist(sapply(x$section, authors, ...))))
}

##' @export
authors.chapter <- function(x, ...) {
    for (i in seq_len(length(x$section))) {
        if (inherits(x$section[[i]], "authors"))
            return(authors(x$section[[i]], ...))
    }

    stop("Unable to find 'Authors'")
}

##' @export
authors.authors <- function(x, ...) {
    sapply(x$contents[[1]]$items, authors, ...)
}

##' @export
authors.author <- function(x, ...) {
    as.character(x$item)
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

                            ll <- length(org$contents[[i]]$section[[j]]$section[[k]]$contents[[1]]$items)
                            for (l in seq_len(ll)) {
                                cl <- c("author", "org_item")
                                class(org$contents[[i]]$section[[j]]$section[[k]]$contents[[1]]$items[[l]]) <- cl
                            }

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
    cat("Report: ", report_title(object), "\n\n", sep = "")
    print(chapters(object))
}

##' @export
print.report <- function(x, ...) {
    cat("Report: ", report_title(x), "\n", sep = "")
    cat("Authors: ", length(authors(x)), "\n", sep = "")
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
report_title <- function(x) {
    stopifnot(inherits(x, "report"))
    "FIXME"
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
    print(authors(x), indent = paste0(indent, "  "))
    cat("\n")
}

##' @export
print.authors <- function(x, ..., indent = "") {
    cat(indent, "Authors:\n", sep = "")
    lapply(x$items, print, indent = paste0(indent, "  "))
    invisible()
}

##' @export
print.author <- function(x, ..., indent = "") {
    cat(indent, x$item, "\n", sep = "")
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
