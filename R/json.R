##' @export
json <- function(x) UseMethod("json")

json.default <- function(x) {
    key <- sub("^x[$]", "", deparse(substitute(x)))
    value <- x
    paste0("\"", key, "\":[\"", value, "\"]")
}

##' @export
json.report <- function(x) {
    x <- c(json(x$title), json(x$contributors), json(x$chapters))
    x <- paste0(x, collapse = ",")
    paste0("{", x, "}")
}

json.contributors <- function(x) {
    x <- paste0(sapply(x, function(y) json(y)), collapse = ",")
    paste0("\"contributors\":[", x, "]")
}

json.contributor <- function(x) {
    x <- c(json(x$name), json(x$email), json(x$organisation))
    x <- paste0(x, collapse = ",")
    paste0("{", x, "}")
}

json.chapters <- function(x) {
    x <- paste0(sapply(x, function(y) json(y)), collapse = ",")
    paste0("\"chapters\":[", x, "]")
}

json.chapter <- function(x) {
    x <- c(json(x$title), json(x$contacts), json(x$authors))
    x <- paste0(x, collapse = ",")
    paste0("{", x, "}")
}

json.contacts <- function(x) {
    x <- paste0(sapply(x, function(y) json(y)), collapse = ",")
    paste0("\"contacts\":[", x, "]")
}

json.authors <- function(x) {
    x <- paste0(sapply(x, function(y) json(y)), collapse = ",")
    paste0("\"authors\":[", x, "]")
}
