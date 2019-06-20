##' @noRd
table_pattern <- function(fileext = c("all", "tex", "pdf")) {
    fileext <- switch(match.arg(fileext),
                      all = "((tex)|(pdf))$",
                      tex = "tex$",
                      pdf = "pdf$")

    paste0("^tab_[^.]+[.]", fileext)
}

##' @noRd
table_files <- function(fileext = "all") {
    if (in_chapter())
        return(list.files(pattern = table_pattern(fileext)))
    stop("Not implemented")
}
