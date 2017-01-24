##' @export
to_json <- function(x) UseMethod("to_json")

to_json.default <- function(x) {
    key <- sub("^x[$]", "", deparse(substitute(x)))
    value <- x
    paste0("\"", key, "\":[\"", value, "\"]")
}

##' @export
to_json.report <- function(x) {
    x <- c(to_json(x$report), to_json(x$contributors), to_json(x$chapters))
    x <- paste0(x, collapse = ",")
    paste0("{", x, "}")
}

to_json.contributors <- function(x) {
    x <- paste0(sapply(x, function(y) to_json(y)), collapse = ",")
    paste0("\"contributors\":[", x, "]")
}

to_json.contributor <- function(x) {
    x <- c(to_json(x$name), to_json(x$email), to_json(x$organisation))
    x <- paste0(x, collapse = ",")
    paste0("{", x, "}")
}

to_json.chapters <- function(x) {
    x <- paste0(sapply(x, function(y) to_json(y)), collapse = ",")
    paste0("\"chapters\":[", x, "]")
}

to_json.chapter <- function(x) {
    x <- c(to_json(x$title), to_json(x$contacts), to_json(x$authors))
    x <- paste0(x, collapse = ",")
    paste0("{", x, "}")
}

to_json.contacts <- function(x) {
    x <- paste0(sapply(x, function(y) to_json(y)), collapse = ",")
    paste0("\"contacts\":[", x, "]")
}

to_json.authors <- function(x) {
    x <- paste0(sapply(x, function(y) to_json(y)), collapse = ",")
    paste0("\"authors\":[", x, "]")
}
