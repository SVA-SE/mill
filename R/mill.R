##' Trim whitespace
##'
##' @param str character string to trim
##' @return trimmed character string
##' @keywords internal
trim <- function(str) {
    trimws(gsub("[*]", "", str))
}

##' Contributor
##'
##' Extract one contributor from one row of the project excel sheet.
##' @param row the row with the contributor.
##' @param title the title of the chapter.
##' @return a contributor object
##' @keywords internal
contributor <- function(row, title) {
    stopifnot(is.data.frame(row))
    stopifnot(all(c("Name", "Email", "Organisation", "Chapter") %in%
                  colnames(row)))
    stopifnot(nrow(row) == 1)

    structure(list(name = trim(row$Name[1]),
                   email = trim(row$Email[1]),
                   organisation = trim(row$Organisation[1]),
                   contact = length(grep(paste0(title, "*"), row$Chapter[1], fixed = TRUE)) > 0),
              class = "contributor")
}

##' @keywords internal
authors <- function(sheet, title) {
    stopifnot(is.data.frame(sheet))
    result <- lapply(grep(title, sheet$Chapter), function(i) {
        contributor(sheet[i, ], title = title)
    })
    class(result) <- "authors"
    result
}

##' @keywords internal
chapters <- function(sheet, path) {
    stopifnot(is.data.frame(sheet))
    stopifnot("Chapter" %in% colnames(sheet))
    titles <- sort(unique(trim(unlist(strsplit(sheet$Chapter, ",")))))
    result <- lapply(titles, function(title) {
        structure(list(title = title,
                       path = file.path(path, "chapters", title),
                       authors = authors(sheet, title)),
                  .Names = c("title", "path", "authors"),
                  class = "chapter")
    })
    class(result) <- "chapters"
    result
}

##' Load configuration for the report
##'
##' @param path The path to the root folder of the project.
##' @export
load_report <- function(path = ".") {
    path <- normalizePath(path, mustWork = TRUE)
    filename <- file.path(path, "README.org")
    org <- org_doc(filename)
    class(org) <- c("report", class(org))
    org
}

##' @method summary report
##' @export
summary.report <- function(object, ...) {
    cat("Report: ", object$report, "\n\n", sep = "")
    print(object$chapters)
}

##' @export
print.report <- function(x, ...) {
    cat("Report: ", x$report, "\n", sep = "")
    do.call("rbind", lapply(x, function(y) as.data.frame(y)))

    ## Contributors
    authors <- lapply(x$chapters, function(chapter) {
        sapply(chapter$authors, function(author) {
            author$name
        })
    })
    authors <- unique(unlist(authors))
    cat("Contributors: ", length(authors), "\n", sep = "")

    cat("Chapters: ", length(x$chapters), "\n", sep = "")
}

##' @export
print.author <- function(x, ..., indent = "") {
    cat(indent,
        x$name, " [", x$organisation, "] <", x$email, ">\n", sep = "")
}

##' @export
print.contributor <- function(x, ..., indent = "") {
    cat(indent,
        ifelse(x$contact, "*", " "),
        x$name, " (", x$organisation, ") <", x$email, ">\n", sep = "")
}

##' @export
print.chapters <- function(x, ...) {
    cat("Chapters:\n")
    lapply(x, print, indent = "  ")
    invisible()
}

##' @export
print.chapter <- function(x, ..., indent = "") {
    cat(indent, x$title, "\n", sep = "")
    print(x$authors, indent = paste0(indent, "  "))
    cat("\n")
}

##' @export
print.contacts <- function(x, ..., indent = "") {
    cat(indent, "Contacts:\n", sep = "")
    lapply(x, print, indent = paste0(indent, "  "))
    invisible()
}

##' @export
print.authors <- function(x, ..., indent = "") {
    cat(indent, "Authors:\n", sep = "")
    lapply(x, print, indent = paste0(indent, "  "))
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
