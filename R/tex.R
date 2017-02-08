##' Clean a chapter 'text.tex' file
##'
##' Clean a chapter 'text.tex' file and save result to temporary file.
##' @param path The path to the chapter.
##' @return The filename to the temporary file.
##' @keywords internal
clean_tex <- function(path) {
    tex <- readLines(file.path(path, "text.tex"))

    ## Remove \begin{multicols}
    pattern <- "[\\]begin[{]multicols[}][{]2[}]"
    tex <- gsub(pattern, "", tex)

    ## Remove \end{multicols}
    pattern <- "[\\]end[{]multicols[}]"
    tex <- gsub(pattern, "", tex)

    ## Remove use of \numprint{1234}
    pattern <- "[\\]numprint[{]([0-9]*)[}]"
    tex <- gsub(pattern, "\\1", tex)

    ## Remove use of \label{sec:some-section}
    pattern <- "[\\]label[{]sec:[^}]*[}]"
    tex <- gsub(pattern, "", tex)

    ## Simplify
    ## e.g. \ref{fig:some-chapter:4}
    ## to \ref{fig:4}
    pattern <- "[\\]ref[{]fig:[^:]*:([^}]*)[}]"
    tex <- gsub(pattern, "\\\\ref{fig:\\1}", tex)

    ## Simplify
    ## e.g. \ref{tab:some-chapter:4}
    ## to \ref{tab:4}
    pattern <- "[\\]ref[{]tab:[^:]*:([^}]*)[}]"
    tex <- gsub(pattern, "\\\\ref{tab:\\1}", tex)

    ## Remove '\\\\'
    pattern <- "^[\\]{4}$"
    tex <- gsub(pattern, "", tex)

    ## Remove '\noindent'
    pattern <- "^[\\]noindent$"
    tex <- gsub(pattern, "", tex)

    ## Remove picture
    ## e.g. "\\includepicture{image1.png}{Photo: Alice}{0cm 0cm 0cm 0cm}"
    pattern <- "^[\\]includepicture[{][^}]*[}][{][^}]*[}][{][^}]*[}]$"
    tex <- gsub(pattern, "", tex)

    filename <- tempfile(pattern = "text-", tmpdir = path, fileext = ".tex")
    writeLines(tex, con = filename)
    filename
}
