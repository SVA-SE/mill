##' Spell checking of chapters
##'
##' @importFrom hunspell hunspell
##' @export
spell_checking <- function()
{
    if (in_chapter()) {
        ignore <- character(0)
        if (file.exists("../../WORDLIST")) ## Project-level
            ignore <- c(ignore, readLines("../../WORDLIST"))
        if (file.exists("WORDLIST")) ## Chapter-level
            ignore <- c(ignore, readLines("WORDLIST"))
        ignore <- sort(unique(ignore))

        lines <- readLines("text.tex")
        bad_words <- hunspell(lines, format = "latex", dict = "en_GB", ignore = ignore)

        cat("\n**********\n*\n*", basename(getwd()), "\n*\n**********\n\n")
        cat(sort(unique(unlist(bad_words))), sep = "\n")
    } else if (in_report()) {
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            spell_checking()
            setwd(wd)
        })
    }

    invisible()
}
