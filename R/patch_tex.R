##' Add numprint to appropriate numbers
##'
##' This function 
##'
##' @param tex The tex character vector
##' @return tex character vector
##' @export
add_numprint <- function(tex) {
    pattern <- "[^]|[[:space:]]+([[:digit:]]{4, })"
    replacement <- " \\\numprint{\\1}"
    gsub(pattern, replacement, tex)[141]##[98:104]
}
