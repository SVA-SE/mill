##' Spell checking of chapters
##'
##' @param harvest \code{TRUE} if you want to harvest the words and
##'     add them to the report-level [r] or chapter-level [c]
##'     WORDLIST. Harvesting can only be used in an interactive
##'     session.
##' @return a character vector with the bad words invisible.
##' @importFrom hunspell hunspell
##' @export
spell_checking <- function(harvest = FALSE) {
    if (in_chapter()) {
        ## Make sure to add a WORDLIST file in the report root and in
        ## each chapter.
        if (!file.exists("../../WORDLIST")) {
            file.create("../../WORDLIST")
            git2r::add(repository(), "WORDLIST")
        }

        if (!file.exists("WORDLIST")) {
            file.create("WORDLIST")
            git2r::add(repository(), "WORDLIST")
        }

        ignore <- character(0)
        if (file.exists("../../WORDLIST")) ## Project-level
            ignore <- c(ignore, readLines("../../WORDLIST"))
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

        if (interactive() && isTRUE(harvest)) {
            for (i in seq_len(length(bad_words))) {
                m <- sprintf("Add '%s' [(r)report|(c)hapter|(s)kip|(q)uit]? ",
                             bad_words[i])
                x <- readline(m)
                if (tolower(substr(x, 1, 1)) == "r") {
                    w <- sort(c(bad_words[i], readLines("../../WORDLIST")))
                    writeLines(w, "../../WORDLIST")
                } else if (tolower(substr(x, 1, 1)) == "c") {
                    w <- sort(c(bad_words[i], readLines("WORDLIST")))
                    writeLines(w, "WORDLIST")
                } else if (tolower(substr(x, 1, 1)) == "s") {
                    next
                } else if (tolower(substr(x, 1, 1)) == "q") {
                    break
                } else {
                    stop(sprintf("Unknown option: '%s'", x))
                }
            }
        }
    } else if (in_report()) {
        ## Make sure to add a WORDLIST file to the project.
        if (!file.exists("WORDLIST")) {
            file.create("WORDLIST")
            git2r::add(repository(), "WORDLIST")
        }

        bad_words <- lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            result <- spell_checking(harvest = harvest)
            setwd(wd)
            result
        })

        bad_words <- as.character(unlist(bad_words))
        bad_words <- as.data.frame(table(bad_words))
        bad_words <- bad_words[order(bad_words$Freq, decreasing = TRUE), ,
                               drop = FALSE]
        rownames(bad_words) <- NULL
    } else{
        stop("You must run spell-checking from ",
             "the report root or in a chapter.")
    }

    invisible(bad_words)
}
