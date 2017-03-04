##' Trim whitespace
##'
##' @param str character string to trim
##' @return trimmed character string
##' @keywords internal
trim <- function(str) {
    sub("\\s*$", "", sub("^\\s*", "", str))
}

##' Contributors
##'
##' Extract the contributors from the project excel sheet.
##' @param sheet a data.frame defining the project.
##' @return a list with contributors
contributors <- function(sheet) {
    stopifnot(is.data.frame(sheet))
    stopifnot(all(c("Name", "Email", "Organisation") %in% colnames(sheet)))

    result <- lapply(seq_len(nrow(sheet)), function(i) {
        structure(list(name = trim(sheet$Name[i]),
                       email = trim(sheet$Email[i]),
                       organisation = trim(sheet$Organisation[i])),
                  .Names = c("name", "email", "organisation"),
                  class = "contributor")
    })

    class(result) <- "contributors"

    result
}

##' Load configuration for the report
##'
##' @param path The path to the root folder of the project.
##' @importFrom yaml yaml.load_file
##' @export
load_report <- function(path = ".") {
    path <- normalizePath(path, mustWork = TRUE)
    r <- yaml.load_file(file.path(path, "report.yml"))
    r$path <- path
    class(r) <- "report"

    class(r$contributors) <- "contributors"
    for (i in seq_len(length(r$contributors)))
        class(r$contributors[[i]]) <- "contributor"

    class(r$chapters) <- "chapters"
    for (i in seq_len(length(r$chapters))) {
        path <- file.path(r$path, "chapters", r$chapters[[i]]$title)
        r$chapters[[i]]$path <- path
        class(r$chapters[[i]]) <- "chapter"

        class(r$chapters[[i]]$contacts) <- "contacts"
        for (j in seq_len(length(r$chapters[[i]]$contacts)))
            class(r$chapters[[i]]$contacts[[j]]) <- "contributor"

        class(r$chapters[[i]]$authors) <- "authors"
        for (j in seq_len(length(r$chapters[[i]]$authors)))
            class(r$chapters[[i]]$authors[[j]]) <- "contributor"
    }

    r
}

##' @method summary report
##' @export
summary.report <- function(object, ...) {
    cat("Report: ", object$report, "\n\n", sep = "")
    print(object$contributors)
    cat("\n")
    print(object$chapters)
}

##' @export
print.report <- function(x, ...) {
    cat("Report: ", x$report, "\n", sep = "")
    cat("Contributors: ", length(x$contributors), "\n", sep = "")
    cat("Chapters: ", length(x$chapters), "\n", sep = "")
}

##' @export
print.contributors <- function(x, ..., indent = "") {
    cat("Contributors:\n")
    lapply(x, print, indent = "  ")
    invisible()
}

##' @export
print.contributor <- function(x, ..., indent = "") {
    cat(indent, x$name, " (", x$organisation, ") <", x$email, ">\n", sep = "")
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
    print(x$contacts, indent = paste0(indent, "  "))
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
    cbind(chapter = x$title,
          rbind(as.data.frame(x$authors), as.data.frame(x$contacts)))
}

as.data.frame.authors <- function(x) {
    cbind(role = "Author",
          do.call("rbind", lapply(x, function(y) as.data.frame(y))))
}

as.data.frame.contacts <- function(x) {
    cbind(role = "Contact",
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
