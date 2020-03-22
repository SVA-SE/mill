##' Spell checking of chapters
##'
##' @importFrom hunspell hunspell
##' @export
spell_checking <- function() {
    if (in_chapter()) {
        ignore <- character(0)
        if (file.exists("../../assets/WORDLIST")) ## Project-level
            ignore <- c(ignore, readLines("../../assets/WORDLIST"))
        if (file.exists("WORDLIST")) ## Chapter-level
            ignore <- c(ignore, readLines("WORDLIST"))
        ignore <- sort(unique(ignore))

        lines <- readLines("text.tex")
        bad_words <- hunspell(lines, format = "latex", dict = "en_GB",
                              ignore = ignore)
        bad_words <- unlist(bad_words)

        cat("\n**********\n*\n*", basename(getwd()), "\n*\n**********\n\n")
        cat(sort(unique(bad_words)), sep = "\n")
        cat("\n")
    } else if (in_report()) {
        bad_words <- lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            result <- spell_checking()
            setwd(wd)
            result
        })

        bad_words <- as.character(unlist(bad_words))
        bad_words <- as.data.frame(table(bad_words))
        bad_words <- bad_words[order(bad_words$Freq, decreasing = TRUE), ,
                               drop = FALSE]
        rownames(bad_words) <- NULL
    }

    invisible(bad_words)
}
