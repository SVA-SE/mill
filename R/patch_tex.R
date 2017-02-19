##' Interactive find and replace over a chapter
##'
##' This function does the interactive replacement for a chapter. An
##' example to add numprint{} might be:
##' 
##' @param pattern A regular expression with a single capture
##' @param replacement The replacement expression Referring to the
##'     first capture with "\\1".
##' @param x The tex character vector
##' @return tex character vector
##' @export
##'
##' @examples
##' r <- load_report()
##' pattern <- "([[:digit:]]{4, })"
##' replacement <- " \\\numprint{\\1}"
##' interactive_replace(r[["Campy"]], pattern, replacement) 
interactive_replace <- function(x, pattern, replacement) {
    UseMethod("interactive_replace")
}

##' Interactive find and replace over a chapter
##'
##' This function does the interactive replacement for a chapter. An
##' example to add numprint{} might be:
##' 
##' @param pattern A regular expression with a single capture
##' @param replacement The replacement expression Referring to the
##'     first capture with "\\1".
##' @param x The tex character vector
##' @return tex character vector
##' @export
##'
##' @examples
##' r <- load_report()
##' pattern <- "([[:digit:]]{4, })"
##' replacement <- " \\\numprint{\\1}"
##' interactive_replace(r[["Campy"]], pattern, replacement) 
interactive_replace.chapter <- function(x, pattern, replacement) {

    ## Get the text
    y <- readLines(text_files(x))

    ## Show lines of text and run interactive replace over the lines
    ## of text.
    do.call("c", lapply(y, function(z){
        cat(paste("\n", z))
        interactive_replace(z, pattern, replacement)
    }))
}

##' Interactive replacement
##'
##' Takes a character vector of length 1 and searches for the pattern
##' and replaces it with the replacement.
##'
##' @keywords internal
##' @param pattern A regular expression with a single capture
##' @param replacement The replacement expression Referring to the
##'     first capture with "\\1".
##' @param x string
##' @return string
interactive_replace.character <- function(x, pattern, replacement) {
    ## Check if there is a match
    match <- regexec(pattern, x)[[1]]
    if(match[1] == -1)
        return(x)

    ## Get various pieces if there is a match
    chunk <- substr(x,
               start = 1,
               stop = (match + attr(match, "match.length") - 1)
               )
    the_rest <- substr(x,
                       start = (match + attr(match, "match.length")),
                       stop = nchar(x)
                       )
    match_text <- substr(x,
                         start = match,
                         stop = (match + attr(match, "match.length") - 1))

    ## Ask the user a question
    cat(paste0("\n\nReplace ", blue(match_text), "?"))
    selection <- utils::menu(c("yes", "no"), title = "")

    ## If the user answers 'no' return the original text and continue
    if(selection == 2) {
        paste0(chunk,
               interactive_replace(the_rest,
                                   pattern,
                                   replacement)
               )
    } else {

        ## If the user answers 'yes' replace the text and continue
        paste0(sub(pattern,
                   replacement,
                   chunk
                   ), interactive_replace(the_rest,
                                          pattern,
                                          replacement)
               )
    }
}

##' Make text blue
##'
##' Make text blue for console output. Copied from the crayon package.
##'
##' @keywords internal
blue <- function(text) {
    paste0("\033[34m", text, "\033[39m")
}
