##' Extract tex command and arguments
##'
##' Extract command and arguments from tex \command[optional
##' argument1]{argument2}{argument3}.
##' @param tex the character vector of length one to extract the
##'     command and arguments from. The first character must be the
##'     opening '\'.
##' @return list with one character vector 'cmd' for the command, one
##'     character vector 'o' with the optional arguments and one
##'     character vector 'm' with the mandatory arguments.
##' @noRd
tex_cmd <- function(tex) {
    stopifnot(is.character(tex), length(tex) == 1)

    ## Command
    cmd <- regmatches(tex, regexpr("^[\\][a-zA-Z]*[*]?", tex))
    if (!isTRUE(nchar(cmd) > 0))
        stop("Invalid tex command")
    tex <- substr(tex, nchar(cmd) + 1, nchar(tex))

    ## Optional arguments
    o <- regmatches(tex, regexpr("^[[][^]]*[]]", tex))
    if (isTRUE(nchar(o) > 0)) {
        o <- substr(o, 2, nchar(o) - 1)

        ## Move forward to mandatory arguments.
        tex <- substr(tex, nchar(o) + 3, nchar(tex))
    }

    ## Mandatory arguments
    stopifnot(substr(tex, 1, 1) == "{")
    m <- character(0)

    repeat {
        i <- 1
        depth <- 0
        len <- nchar(tex)

        ## Iterate over all characters in {} to extract the argument.
        repeat {
            if (substr(tex, i, i) == "}") {
                depth <- depth - 1
            } else if (substr(tex, i, i) == "{") {
                depth <- depth + 1
            }

            if (depth == 0) {
                i <- i - 1
                break
            }

            i <- i + 1
            if (i > len)
                stop("Unable to find a closing '}'")
        }

        m <- c(m, substr(tex, 2, i))

        ## Move forward and check if there exists another argument.
        tex <- substr(tex, i + 2, nchar(tex))
        if (substr(tex, 1, 1) != "{")
            break
    }

    list(cmd = cmd, m = m, o = o)
}

tex_cmd_nchar <- function(x) {
    nchar(x$cmd) +
        sum(vapply(x$o, function(o) nchar(o) + 2L, integer(1))) +
        sum(vapply(x$m, function(m) nchar(m) + 2L, integer(1)))
}

##' Extract arguments from a tex command
##'
##' Extract arguments from a tex command \command[optional
##' argument1]{argument2}{argument3}.
##' @param tex the character vector of length one to extract the
##'     argument from. The first character must be the opening '{'.
##' @return list with one character vector 'o' with the optional
##'     arguments and one character vector 'm' with the mandatory
##'     arguments.
##' @noRd
tex_arguments <- function(tex) {
    stopifnot(is.character(tex), length(tex) == 1)

    ## Optional arguments
    o <- regmatches(tex, regexpr("^[[][^]]*[]]", tex))
    if (isTRUE(nchar(o) > 0)) {
        o <- substr(o, 2, nchar(o) - 1)

        ## Move forward to mandatory arguments.
        tex <- substr(tex, nchar(o) + 3, nchar(tex))
    }

    ## Mandatory arguments
    stopifnot(substr(tex, 1, 1) == "{")
    m <- character(0)

    repeat {
        i <- 1
        depth <- 0
        len <- nchar(tex)

        ## Iterate over all characters in {} to extract the argument.
        repeat {
            if (substr(tex, i, i) == "}") {
                depth <- depth - 1
            } else if (substr(tex, i, i) == "{") {
                depth <- depth + 1
            }

            if (depth == 0) {
                i <- i - 1
                break
            }

            i <- i + 1
            if (i > len)
                stop("Unable to find a closing '}'")
        }

        m <- c(m, substr(tex, 2, i))

        ## Move forward and check if there exists another argument.
        tex <- substr(tex, i + 2, nchar(tex))
        if (substr(tex, 1, 1) != "{")
            break
    }

    list(m = m, o = o)
}

##' Collapse tex-lines to one line that contains '\n' between lines.
##' @noRd
tex_2_one_line <- function(tex) {
    paste0(tex, collapse = "\n")
}

##' Split tex that contains '\n' to multiple lines.
##' @noRd
tex_2_multi_line <- function(tex) {
    if (!is.null(tex)) {
        f <- textConnection(tex)
        tex <- readLines(f)
        close(f)
    }

    tex
}

##' Check if current working directory is in a chapter
##' @noRd
in_chapter <- function() {
    if (!file.exists("README.org")) {
        if (identical(basename(dirname(getwd())), "chapters"))
            return(file.exists("../../README.org"))
    }
    FALSE
}

##' Check if current working directory is in a report
##' @noRd
in_report <- function() {
    file.exists("README.org")
}

##' Import chapter docx file from workspace
##'
##' @return invisible NULL.
##' @export
import <- function() {
    if (in_chapter()) {
        chapter <- basename(getwd())
        cat(sprintf("Importing: %s\n", chapter))
        from <- paste0("../../workspace/chapters/", chapter)
        if (!dir.exists(from))
            stop("Invalid directory:", from)

        ## Import title.docx to text.docx
        from <- paste0(from, "/", chapter, ".docx")
        if (!file.exists(from))
            stop("Missing file:", from)
        to <- "text.docx"
        file.copy(from, to, overwrite = TRUE)
    } else if (in_report()) {
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            import()
            setwd(wd)
        })
    }

    invisible()
}

inject_tab_fig_infocus <- function(tex, infocus, chapter) {
    chapter <- normalize_title(chapter)

    ## Find the references
    tex_a <- c(tex, unlist(infocus))
    pattern <- "[\\]label[{][^}]*[}]|[\\]ref[{][^}]*[}]"
    m <- regmatches(tex_a, gregexpr(pattern, tex_a))
    m <- unlist(lapply(m, function(y) {
        regmatches(y, regexec(pattern, y))
    }))
    marker <- sub("[\\][^{]+[{]([^}]*)[}]", "\\1", m)
    reftype <- sapply(strsplit(marker, ":"), "[", 1)
    df <- data.frame(tex = m,
                     marker   = marker,
                     reftype  = reftype,
                     stringsAsFactors = FALSE)

    df <- df[df$reftype %in% c("fig", "tab"), ]
    if (nrow(df)) {
        df$filename <- paste0(gsub(":", "_", df$marker), ".tex")

        ## Are there any unreferenced figure files to append.
        files <- list.files(pattern = "^fig_[^.]+[.]tex$")
        if (length(files) > 0)
            files <- sub("^fig_", paste0("fig_", chapter, "_"), files)
        files <- unique(c(df$filename, files))
        files <- paste0("\\input{", files, "}")
        tex <- c(tex, files)
    }

    ## Append infocus
    if (length(infocus)[1] > 0) {
        infocus <- length(split_infocus(infocus))[1]
        infocus <- paste0("\\input{infocus_", chapter,
                          "_", seq_len(infocus), ".tex}")
        tex <- c(tex, infocus)
    }

    tex
}

##' Split figures
##'
##' Extract the tex for each figure that starts with an
##' includegraphics.
##' @param tex the tex for the figures section.
##' @return a list with the tex for each figure.
##' @noRd
split_figures <- function(tex) {
    tex <- tex_2_one_line(tex)

    ## Remove the first hypertarget.
    i <- regexpr("\\\\hypertarget[{]", tex)
    if (isTRUE(i == -1))
        stop("Unable to find 'hypertarget'")
    cmd <- tex_cmd(substr(tex, i, nchar(tex)))
    stopifnot(length(cmd$m) == 2)
    tex <- substr(tex, i + tex_cmd_nchar(cmd), nchar(tex))

    ## Pattern that starts a figure caption.
    pattern <- paste0("(Figure)?[[:space:]]*[{][[][}]",
                      "fig:[^{]+[{][]][}][:]?[[:space:]]*")

    ## Split tex into captions.
    captions <- list()
    repeat {
        i <- gregexpr(pattern, tex)[[1]]
        if (isTRUE(i == -1))
            stop("Unable to find a figure caption.")

        if (length(i) == 1) {
            to <- nchar(tex)
        } else {
            ## Determine if there is an 'includegraphics' between the
            ## first and second caption.
            j <- gregexpr("\\\\includegraphics[[]", tex)[[1]]
            j <- j[j > i[1] & j < i[2]]
            if (length(j)) {
                to <- j[1] - 1
            } else {
                to <- i[2] - 1
            }
        }

        caption <- substr(tex, 1, to)

        ## Check for a 'includegraphics' with the image file from the
        ## docx-file.
        docx <- NA_character_
        i <- regexpr("\\\\includegraphics[[]", caption)
        if (!isTRUE(i == -1)) {
            cmd <- tex_cmd(substr(caption, i, nchar(tex)))
            stopifnot(length(cmd$m) == 1)
            docx <- cmd$m[1]
            caption <- substr(caption, i + tex_cmd_nchar(cmd), nchar(caption))
        }

        caption <- trimws(caption)
        attr(caption, "docx") <- docx

        ## Check for empty lines.
        i <- gregexpr("\n{2,}", caption)[[1]]
        if (!isTRUE(i == -1))
            stop(sprintf("Invalid caption:\n%s", caption))

        captions[[length(captions) + 1]] <- caption
        tex <- substr(tex, to + 1, nchar(tex))
        if (nchar(tex) == 0)
            break
    }

    captions
}

##' @importFrom tools file_ext
##' @noRd
save_figure <- function(tex, chapter) {
    prefix <- normalize_title(chapter)

    ## The file that has been extracted from the docx-file. NA if the
    ## file was not found.
    docx_figure <- attr(tex, "docx")

    ## Determine the figure label.
    pattern <- paste0("^(Figure)?[[:space:]]*[{][[][}]",
                      "fig:[^{]+[{][]][}][:]?[[:space:]]*")
    label <- trimws(regmatches(tex, regexpr(pattern, tex)))
    is_figure <- startsWith(label, "Figure")
    label <- sub("^(Figure)?[[:space:]]*[{][[][}]fig:", "", label)
    label <- sub("[{][]][}][:]?$", "", label)

    ## Determine the caption.
    tex <- sub(pattern, "", tex)
    if (is_figure) {
        tex <- paste0("\\caption{", tex)
    } else {
        tex <- paste0("\\caption*{\\scriptsize{", tex, "}")
    }
    tex <- paste0(tex, paste0("\n\\label{fig:", prefix, ":", label, "}"))

    caption <- tex_2_multi_line(tex)
    i <- seq_len(length(caption))[-1]
    caption[i] <- paste0("  ", caption[i])
    caption <- c(caption, "}")

    ## Move and rename the docx_figure file.
    if (!is.na(docx_figure)) {
        to <- paste0("docx_fig_", label, ".", file_ext(docx_figure))
        cat(sprintf("  - Write file: %s (Only for info, not added to repo.)\n",
                    to))
        file.copy(docx_figure, to)
    }

    ## Create the tex-file for the figure.
    fig <- paste0("fig_", prefix, "_", label)
    lines <- c("\\begin{figure}[H]",
               paste0("  \\includegraphics[width=\\textwidth]{", fig, "}"),
               paste0("  ", caption),
               "\\end{figure}")
    lines <- style_numprint(lines)
    filename <- paste0("fig_", label, ".tex")
    cat(sprintf("  - Write file: %s\n", filename))
    writeLines(lines, filename)
    git2r::add(repository(), paste0("chapters/", chapter, "/", filename))

    invisible(NULL)
}

extract_figures <- function(tex, chapter) {
    if (!is.null(tex)) {
        figures <- split_figures(tex)
        lapply(figures, save_figure, chapter = chapter)
    }

    invisible(NULL)
}

##' Split infocus
##'
##' Extract the tex for each infocus that starts with a subsection.
##' We know that all infocus sections starts with a 'section' so drop
##' that and split on all subsections. Before returning, any infocus
##' subsections are bumped to sections.
##' @param tex the tex for the infocus sections.
##' @return a list with the tex for each subsection in the infocus.
##' @noRd
split_infocus <- function(tex) {
    ## First, remove the section.
    i <- grep("\\\\section", tex)
    stopifnot(length(i) == 1)
    tex <- tex[-seq_len(i)]

    ## Find position of subsections.
    infocus <- grep("\\\\subsection", tex)
    if (length(infocus) == 0)
        return(list())

    ## Make sure that we also include any hypertargets that might come
    ## one line before each subsection.
    infocus <- vapply(infocus, function(i) {
        if (startsWith(tex[i - 1], paste0("\\hypertarget{")))
            i <- i - 1L
        i
    }, integer(1))

    mapply(function(from, n) {
        to <- from + n - 1

        ## Remove empty lines.
        while (to > from &&
               ((nchar(tex[to]) == 0) || tex[to] == "\\\\\\\\")) {
            to <- to - 1
        }

        style_infocus(tex[seq(from = from, to = to)])
    }, infocus, diff(c(infocus, length(tex) + 1)), SIMPLIFY = FALSE)
}

##' Extract any infocus sections.
##'
##' @param tex character vector that contains zero- or more infocus
##'     sections.
##' @param chapter name of the chapter.
##' @return the number of infocus sections that were found.
##' @noRd
extract_infocus <- function(tex, chapter) {
    if (is.null(tex))
        return(0)

    infocus <- split_infocus(tex)
    for (i in seq_len(length(infocus))) {
        filename <- sprintf("infocus_%i.tex", i)
        cat(sprintf("  - Write file: %s\n", filename))
        writeLines(infocus[[i]], filename)
        git2r::add(repository(), paste0("chapters/", chapter, "/", filename))
    }

    length(infocus)
}

style_infocus <- function(tex) {
    tex <- tex_2_one_line(tex)

    ## Determine the infocus title.
    i <- regexpr("\\\\subsection[*][{]", tex)
    if (i == -1)
        stop("Unable to find 'subsection'")
    i <- i + attr(i, "match.length") - 1
    title <- tex_arguments(substr(tex, i, nchar(tex)))$m

    ## Split the tex to remove the title and then combine it again
    ## with the infocus title.
    tex_1 <- substr(tex, 1, i)
    tex_2 <- substr(tex, i + nchar(title) + 1, nchar(tex))
    infocus_title <- paste0("\\texorpdfstring{{\\color{svared}IN FOCUS:} ",
                            title, "}{", title, "}")
    tex <- paste0(tex_1, infocus_title, tex_2)

    ## Apply numprint and convert the tex to multi-lines again.
    tex <- tex_2_multi_line(style_numprint(tex))

    ## Finally, add some tex before and after the infocus.
    c("\\hspace*{-5mm}%",
      "\\begin{tikzpicture}",
      "\\node[anchor = west,",
      "fill = highlightgray,",
      "minimum width = \\textwidth,",
      "minimum height = 75mm,",
      "rounded corners=3mm,",
      "draw=svared,",
      "line width=0.1mm](text) at (0,0) {",
      "\\begin{minipage}{0.93\\textwidth}",
      "\\sf",
      tex,
      "\\end{minipage}",
      "};",
      "\\end{tikzpicture}")
}

style_fun <- function(tex, chapter) {
    tmp <- style_drop_section(tex, "Figures")
    extract_figures(tmp$drop, chapter)
    tex <- tmp$tex

    tex <- style_drop_section(tex, "Tables")$tex
    tex <- convert_docx_ref_to_ref(tex, chapter)
    tex <- make_labels_chapter_specific(tex, chapter)
    tex <- hypertargets_chapter_specific(tex, chapter)
    tex <- asterisk(tex, "add")
    tex <- style_chapter_title(tex)
    tex <- style_numprint(tex)

    tmp <- style_drop_section(tex, "In focus")
    extract_infocus(tmp$drop, chapter)
    tex <- tmp$tex
    tex <- add_line_between_references(tex)
    tex <- style_multicols(tex)
    tex <- inject_tab_fig_infocus(tex, tmp$drop, chapter)
    tex
}

##' Convert from docx to tex
##'
##' Use pandoc (http://pandoc.org/) to convert from 'docx' to
##' 'tex'. The chapter 'text.docx' is converted to 'text.tex'. Each
##' chapter 'text.tex' is added, but not commited, to the report git
##' repository.
##' @param repo the report git repository.
##' @param force if \code{TRUE}, run even if working tree is not
##'     clean. Default is \code{FALSE}.
##' @return invisible NULL.
##' @importFrom git2r add
##' @importFrom git2r repository
##' @export
from_docx <- function(repo = NULL, force = FALSE) {
    if (in_chapter()) {
        on.exit(unlink("./media", recursive = TRUE), add = TRUE)
        chapter <- basename(getwd())
        cat(sprintf("From docx: %s\n", chapter))

        ## First, delete all tex-files in order to detect tex-files
        ## that don't have origin in the docx-file.
        unlink(list.files(pattern = "[.]tex$"))

        ## Convert the docx to a temporary tex file.
        f_tex <- tempfile(fileext = ".tex")
        f_docx <- "text.docx"
        pandoc(paste("--top-level-division=chapter ",
                     shQuote(f_docx),
                     "--extract-media=. -o",
                     shQuote(f_tex)))

        ## Tweak incoming tex file
        tex <- readLines(f_tex)
        file.remove(f_tex)
        tex <- style_fun(tex, chapter)
        cat(sprintf("  - Write file: %s\n", "text.tex"))
        writeLines(tex, "text.tex")
        if (!is.null(repo))
            add(repo, paste0("chapters/", chapter, "/text.tex"))

        ## Convert tables
        lapply(docx_tables(f_docx), function(tbl) {
            prefix <- normalize_title(chapter)
            filename <- paste0(gsub(":", "_", format(tbl$label)), ".tex")
            cat(sprintf("  - Write file: %s\n", filename))
            writeLines(format(tbl, output = "tex", prefix = prefix), filename)
            if (!is.null(repo))
                add(repo, paste0("chapters/", chapter, "/", filename))
        })
    } else if (in_report()) {
        repo <- repository()
        if (!isTRUE(force)) {
            s <- status(repo)
            if (length(c(s$staged, s$unstaged))) {
                stop(paste("Cannot run 'from_docx' since working tree",
                           "is not clean. You can use 'force = TRUE'."))
            }
        }

        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            from_docx(repo = repo, force = force)
            setwd(wd)
        })
    }

    invisible()
}

normalize_title <- function(title) {
    gsub("[[:space:]]+", "-", tolower(title))
}

##' A section starts with something like:
##' \hypertarget{figures}{%
##' \section{Figures}\label{figures}}
##' @noRd
style_drop_section <- function(tex, section) {
    tex <- tex_2_one_line(tex)

    ## Find all 'hypertarget'.
    i <- gregexpr("\\\\hypertarget[{]", tex)[[1]]
    if (any(i == -1))
        return(tex_2_multi_line(tex))

    ## Now split all 'hypertarget' into smaller pieces to find the
    ## hypertarget with the section that we want to drop.
    h <- mapply(function(i, n) {
        args <- tex_arguments(substr(tex, i + n - 1, nchar(tex)))$m

        ## First, determine if this is a section, and then if it's the
        ## section to drop.
        j <- regexpr("\\\\section[*]?[{]", args[2])
        if (j == -1)
            return(NULL)

        j <- j + attr(j, "match.length") - 1
        title <- tex_arguments(substr(args[2], j, nchar(args[2])))$m
        drop <- identical(trimws(title), trimws(section))

        list(i = i, drop = drop)
    }, i, attr(i, "match.length"), SIMPLIFY = FALSE)

    ## Keep only hypertargets that are sections.
    h <- h[!vapply(h, is.null, logical(1))]

    ## Check if a section to drop was identified.
    drop <- NULL
    if (any(vapply(h, function(x) x$drop, logical(1)))) {
        ## Determine the index where to cut the tex.
        i <- min(unlist(sapply(h, function(x) {
            if (isTRUE(x$drop))
                return(x$i)
            integer(0)
        })))

        ## Determine if there are any tex after the cut to keep.
        j <- unlist(sapply(h, function(x) {
            if (!isTRUE(x$drop))
                return(x$i)
            integer(0)
        }))
        j <- j[j > i]

        if (length(j) > 0) {
            drop <- trimws(substr(tex, i, j - 1))
            tex <- paste0(trimws(substr(tex, 1, i - 1)),
                          "\n\n",
                          trimws(substr(tex, j, nchar(tex))))
        } else {
            drop <- trimws(substr(tex, i, nchar(tex)))
            tex <- trimws(substr(tex, 1, i - 1))
        }
    }

    list(tex  = tex_2_multi_line(tex),
         drop = tex_2_multi_line(drop))
}

##' Convert the docx references to tex ref
##'
##' @param tex The tex character vector
##' @param title The chapter title
##' @return tex character vector
##' @noRd
convert_docx_ref_to_ref <- function(tex, title) {
    title <- normalize_title(title)
    pattern <- "[{][[][}]([^:]*)[:]([^{]*)[{}[]][}]"
    replacement <- paste0("\\\\ref{\\1:", title, ":\\2}")
    tex <- gsub(pattern, replacement, tex)

    ## Replace space between 'something \ref' with 'something~\ref'.
    gsub("\\s+\\\\ref", "~\\\\ref", tex)
}

##' Make tex hypertargets chapter specific in the report
##'
##' @param tex The tex character vector
##' @param title The chapter title
##' @return tex character vector
##' @noRd
hypertargets_chapter_specific <- function(tex, title) {
    title <- normalize_title(title)
    pattern <- "[\\]hypertarget[{]([^}]*)[}]"
    replacement <- paste0("\\\\hypertarget{sec:", title, ":", "\\1}")
    gsub(pattern, replacement, tex)
}

##' Make tex labels chapter specific in the report
##'
##' @param tex The tex character vector
##' @param title The chapter title
##' @return tex character vector
##' @noRd
make_labels_chapter_specific <- function(tex, title) {
    title <- normalize_title(title)
    pattern <- "[\\]label[{]([^}]*)[}]"
    replacement <- paste0("\\\\label{sec:", title, ":", "\\1}")
    gsub(pattern, replacement, tex)
}

##' Add asterisk to sections
##'
##' @param tex The tex character vector
##' @return tex character vector
##' @noRd
asterisk <- function(tex, direction = c("add", "remove")) {
    replacement <- switch(match.arg(direction),
                          add = "\\1*\\3",
                          remove = "\\1\\3"
                          )
    patterns <- c("(\\\\chapter)([\\*]?)(\\{)",
                  "(\\\\section)([\\*]?)(\\{)",
                  "(\\\\subsection)([\\*]?)(\\{)",
                  "(\\\\subsubsection)([\\*]?)(\\{)",
                  "(\\\\paragraph)([\\*]?)(\\{)")
    for (i in 1:5) {
            tex <- gsub(patterns[i], replacement, tex)
        }
    return(tex)
}

##' Add an empty line between references
##'
##' @param tex The tex character vector
##' @return tex character vector
##' @noRd
add_line_between_references <- function(tex) {
    ## Find the line for the reference section.
    i <- grep("^\\\\section[*][{]References[}]", tex)
    if (!length(i))
        return(tex)

    ## We expect one reference section.
    stopifnot(identical(length(i), 1L))

    ## Skip the first empty line after the reference section line.
    i <- i + 2

    ## Determine the indices to empty lines after the reference
    ## section.
    j <- which(nchar(tex) == 0)
    i <- j[j > i]
    if (!length(i))
        return(tex)

    ## Add empty lines to tex.
    tex[i] <- "\\\\\\\\"

    tex
}

##' Handle multicols when converting to tex
##'
##' @param tex The tex character vector.
##' @return tex character vector.
##' @noRd
style_multicols <- function(tex) {
    tex <- tex_2_one_line(tex)

    ## Extract the arguments for the addcontentsline.
    i <- regexpr("\\\\addcontentsline[{]", tex)
    if (i == -1)
        stop("Unable to find 'addcontentsline'")
    i <- i + attr(i, "match.length") - 1
    args <- tex_arguments(substr(tex, i, nchar(tex)))$m
    stopifnot(length(args) == 3)
    args <- paste0("{", paste0(args, collapse = "}{"), "}")

    ## Split the tex into two parts: before and after
    ## 'addcontentsline'.
    tex_1 <- substr(tex, 1, i - 1)
    tex_2 <- substr(tex, i + nchar(args), nchar(tex))

    ## Combine the pieces and add the begin/end multicols.
    tex <- paste0(tex_1,
                  args,
                  "\n\\begin{multicols}{2}",
                  tex_2,
                  "\n\\end{multicols}")

    ## Convert the tex to multi-lines again.
    tex_2_multi_line(tex)
}

##' Inject numprint when converting to tex
##'
##' @param tex The tex character vector.
##' @return tex character vector.
##' @noRd
style_numprint <- function(tex) {
    ## Find the line for the reference section to make sure not to add
    ## \numprint{} to numbers in references. Use the complete text if
    ## the chapter doesn't contain a reference section.
    i <- grep("^\\\\section[*][{]References[}]", tex)
    if (!length(i))
        i <- length(tex)
    stopifnot(identical(length(i), 1L))
    i <- seq_len(i)

    ## Pattern to find 5 digits or more
    pattern <- "[[:digit:]]{5,}"

    ## Break apart text into pieces where some are matches to the
    ## pattern
    tex_mod <- regmatches(tex[i],
                          gregexpr(pattern, tex[i], perl = TRUE),
                          invert = NA)

    ## Which of these are that numbers
    positive <- lapply(tex_mod, function(x) {
        grep(pattern, x, perl = TRUE)
    })

    tex_mod <- mapply(function(x, y) {
        ## if there is no match on this line just return the line
        if (identical(y, integer(0))) return(x)

        ## Remove the matches if they are followed by a single hyphen
        ## but not two hyphens
        remove <- substr(x[y + 1], 1, 1) == "-" &
                  substr(x[y + 1], 1, 2) != "--"

        ## Check that a single hyphen does not preceed the numbers
        rev_x <- unlist(lapply(strsplit(x, NULL), function(x) {
            paste(rev(x), collapse = "")
        }))
        remove2 <- substr(rev_x[y - 1], 1, 1) == "-" &
                   substr(rev_x[y - 1], 1, 2) != "--"

        ## Check that a single '_'.
        remove3 <- substr(rev_x[y - 1], 1, 1) == "_"

        ## Remove the matches if they are inside '(' and ')'.
        remove4 <- endsWith(x[y - 1], "(") &
                   startsWith(x[y + 1], ")")

        remove <- remove | remove2 | remove3 | remove4
        y <- y[!remove]

        ## replace those we want to replace with numprint
        x[y] <- paste0("\\numprint{", x[y], "}")

        paste(x, collapse = "")
    }, tex_mod, positive)

    c(tex_mod,
      tex[-i])
}

##' Style chapter title and table of contents when converting to tex
##'
##' @param tex The tex character vector.
##' @return tex character vector.
##' @noRd
style_chapter_title <- function(tex) {
    tex <- tex_2_one_line(tex)

    ## Extract the arguments for the first hypertarget. It contains
    ## the chapter and label in the second argument.
    i <- regexpr("\\\\hypertarget[{]", tex)
    if (i == -1)
        stop("Unable to find 'hypertarget'")
    i <- i + attr(i, "match.length") - 1
    hypertarget <- tex_arguments(substr(tex, i, nchar(tex)))$m
    stopifnot(length(hypertarget) == 2)

    ## Extract the tex after the hypertarget. The '+4' is to include
    ## the '{' and '}' for each argument.
    i <- i + nchar(hypertarget[1]) + nchar(hypertarget[2]) + 4
    tex <- substr(tex, i, nchar(tex))

    ## Determine the title of the chapter from
    ## '\chapter*{title-of-chapter}'
    i <- regexpr("\\\\chapter[*][{]", hypertarget[2])
    if (i == -1)
        stop("Unable to find 'chapter'")
    i <- i + attr(i, "match.length") - 1
    title <- tex_arguments(substr(hypertarget[2], i, nchar(hypertarget[2])))$m
    stopifnot(length(title) == 1)

    ## Determine the label of the chapter from
    ## '\label{label-of-chapter}'
    i <- regexpr("\\\\label[{]", hypertarget[2])
    if (i == -1)
        stop("Unable to find 'label'")
    i <- i + attr(i, "match.length") - 1
    label <- tex_arguments(substr(hypertarget[2], i, nchar(hypertarget[2])))$m
    stopifnot(length(label) == 1)

    ## Determine 'texorpdfstring'.
    texorpdfstring <- paste0("\\texorpdfstring{", title, "}{", title, "}")

    ## Conbine all pieces.
    tex <- paste0("\\hypertarget{", hypertarget[1], "}{%\n",
                  "\\chapter*{", texorpdfstring, "}\\label{", label, "}}\n",
                  "\\addcontentsline{toc}{chapter}{", texorpdfstring, "}",
                  tex)

    ## Convert the tex to multi-lines again.
    tex_2_multi_line(tex)
}

##' Cleanup temporary files
##'
##' @return invisible NULL.
##' @export
cleanup <- function() {
    if (in_chapter()) {
        unlink("text.docx")
        unlink("typeset.tex")
        unlink("typeset.tex.rej")

        ## Table tex-files
        ref <- references()
        lapply(ref[ref$reftype == "tab", "marker"], function(marker) {
            marker <- paste0(gsub(":", "_", marker), ".tex")
            unlink(marker)
        })

    } else if (in_report()) {
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            cleanup()
            setwd(wd)
        })
    }

    invisible()
}
