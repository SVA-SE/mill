##' List authors
##'
##' @param x the report object, for example, a chapter, to list autors
##'     in.
##' @return authors
##' @export
authors <- function(x) {
    UseMethod("authors")
}

##' @export
authors.report <- function(x) {
    authors(chapters(x))
}

##' @export
authors.chapters <- function(x) {
    sort(unique(unlist(sapply(x$section, authors))))
}

##' @export
authors.chapter <- function(x) {
    for (i in seq_len(length(x$section))) {
        if (inherits(x$section[[i]], "authors"))
            return(authors(x$section[[i]]))
    }

    stop("Unable to find 'Authors'")
}

##' @export
authors.authors <- function(x) {
    unlist(lapply(x$contents[[1]]$items, function(y) {
        y$item
    }))
}

##' Formatting authors
##'
##' This function gets a unique list of authors and sorts them
##' alphabetically. I assume this should actually be solved as a print
##' method and format method for an authors class object.
##'
##' We also have a lexigraphical sort problem in this function that is
##' solved with a hack that might have a better cross platform
##' solution.
##'
##' @title formatted authors
##' @param x A report object
##' @param format The format of the author list you want
##' @return A unique sorted list of authors
##' @author Thomas Rosendal
##' @importFrom utils tail
##' @export
##' @examples
##' \dontrun{
##' ## Update the authors list for front matter
##' writeLines(paste(formatted_authors(load_report()),
##'                  collapse = ",\n"),
##'            "assets/front-matter/authors.tex")
##' ## Or get the emails in a send list format
##' paste(formatted_authors(load_report(), format = "email"), collapse = ";")
##' }
formatted_authors <- function(x, format = c("name", "email")) {
    format <- match.arg(format)
    auths <- authors(x)
    names <- unlist(lapply(regmatches(auths, regexec('- (.*) \\(', auths)), "[", 2))
    names <- do.call("rbind", lapply(names, function(y){
        lastname <- tail(strsplit(y, " ")[[1]], 1)
        ## Sorting of non-ASCII characters is system dependant and we
        ## expect the text in the report to be UTF-8. Therefore we can
        ## fix the sorting by replacing the last three letter of the
        ## Swedish alphabet with sortable ASCII strings:
        lastname <- gsub(paste0("^", rawToChar(as.raw(c(0xc3, 0x85)))), "ZZZZZA", lastname)
        lastname <- gsub(paste0("^", rawToChar(as.raw(c(0xc3, 0x84)))), "ZZZZZB", lastname)
        lastname <- gsub(paste0("^", rawToChar(as.raw(c(0xc3, 0x96)))), "ZZZZZC", lastname)
        c(lastname, y)
    }))
    if(format == "name") {
        return(names[,2][order(names[,1])])
    }
    mails <- unlist(lapply(regmatches(auths, regexec('^- (.*)', auths)), "[", 2))
    return(mails[order(names[,1])])
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
    print(object, ..., main_only = FALSE)
}

##' @export
print.report <- function(x, ...) {
    cat("Report: ", report_title(x), "\n", sep = "")
    cat("Progress: [", report_progress(x), "%]\n", sep = "")
    cat("Authors: ", length(authors(x)), "\n", sep = "")
    print(chapters(x), ...)
}

##' @export
print.chapters <- function(x, ...) {
    cat("Chapters: ", length(x$section), "\n", sep = "")
    lapply(x$section, function(y) {print(y, ..., indent = "  ")})
    invisible()
}

##' @export
print.chapter <- function(x, ..., indent = "", main_only = TRUE) {
    cat(indent, chapter_state(x), " ", chapter_title(x), "\n", sep = "")
    indent <- paste0(indent, "  ")
    x <- authors(x)
    if (isTRUE(main_only)) {
        i <- 1
    } else {
        i <- seq_len(length(x))
    }
    lapply(x[i], function(y) {
        cat(indent, y, "\n", sep = "")
    })
    invisible()
}

##' @noRd
report_keyword <- function(x, key) {
    stopifnot(inherits(x, "report"))

    ii <- length(x$contents)
    for (i in seq_len(ii)) {
        if (inherits(x$contents[[i]], "org_headline")) {
            if (identical(grep("Org-mode configuration", x$contents[[i]]$headline), 1L)) {
                jj <- length(x$contents[[i]]$section)
                for (j in seq_len(jj)) {
                    if (inherits(x$contents[[i]]$section[[j]], "org_keyword")) {
                        if (identical(x$contents[[i]]$section[[j]]$key, key)) {
                            return(x$contents[[i]]$section[[j]]$value)
                        }
                    }
                }
            }
        }
    }

    stop("Unable to find keyword: ", key)
}

##' @noRd
report_title <- function(x) {
    report_keyword(x, "TITLE")
}

##' @noRd
report_progress <- function(x) {
    ## Determine valid todo states for a chapter
    todos <- report_keyword(x, "TODO")
    todos <- gsub("[(][^)]*[)]", "", todos)
    todos <- gsub("[|]", "", todos)
    todos <- trimws(unlist(strsplit(todos, " ")))
    todos <- todos[nchar(todos) > 0]

    completed <- sum(sapply(chapters(x)$section, function(y) {
        s <- chapter_state(y)
        stopifnot(s %in% todos)
        match(s, todos)
    }))

    as.integer(100 * completed / (length(todos) * length(chapters(x)$section)))
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

##' @noRd
chapter_path <- function(x) {
    stopifnot(inherits(x, "chapter"))
    file.path("chapters", chapter_title(x))
}

##' @export
`[.report` <- function(x, i) {
    if (is.character(i)) {
        i <- grep(i, sapply(chapters(x)$section, chapter_title),
                  ignore.case = TRUE)
    }
    chapters(x)$section[i]
}

##' @export
`[[.report` <- function(x, i) {
    if (is.character(i)) {
        i <- grep(i, sapply(chapters(x)$section, chapter_title),
                  ignore.case = TRUE)
    }
    stopifnot(identical(length(i), 1L))
    chapters(x)$section[[i]]
}
