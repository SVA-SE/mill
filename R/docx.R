##' Extract arguments from a tex command
##'
##' Extract arguments from a tex command
##' \command{argument1}{argument2}.
##' @param tex the character vector of length one to extract the
##'     argument from. The first character must be the opening '{'.
##' @return a character vector with the arguments.
##' @noRd
tex_arguments <- function(tex) {
    stopifnot(is.character(tex),
              length(tex) == 1,
              substr(tex, 1, 1) == "{")

    args <- character(0)

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

        args <- c(args, substr(tex, 2, i))

        ## Move forward and check if there exists another argument.
        tex <- substr(tex, i + 2, nchar(tex))
        if (substr(tex, 1, 1) != "{")
            break
    }

    args
}

##' Collapse tex-lines to one line that contains '\n' between lines.
##' @noRd
tex_2_one_line <- function(tex) {
    paste0(tex, collapse = "\n")
}

##' Split tex that contains '\n' to multiple lines.
##' @noRd
tex_2_multi_line <- function(tex) {
    f <- textConnection(tex)
    tex <- readLines(f)
    close(f)
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
        df$filename <- paste0("\\input{", gsub(":", "_", df$marker), ".tex}")
        tex <- c(tex, unique(df$filename))
    }

    ## Append infocus
    if (length(infocus)[1] > 0) {
        infocus <- length(split_infocus(infocus))[1]
        chapter <- normalize_title(chapter)
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
    figures <- grep("\\\\includegraphics", tex)
    if (length(figures) == 0)
        return(list())

    mapply(function(from, n) {
        to <- from + n - 1

        ## Remove empty lines.
        while (to > from &&
               ((nchar(tex[to]) == 0) || tex[to] == "\\\\\\\\")) {
            to <- to - 1
        }

        tex[seq(from = from, to = to)]
    }, figures, diff(c(figures, length(tex) + 1)), SIMPLIFY = FALSE)
}

save_figure <- function(tex, chapter) {
    prefix <- normalize_title(chapter)

    ## Determine the filename.
    pattern <- "^\\\\includegraphics[[][^]]+[]][{]"
    filename <- sub(pattern, "", sub("[}]$", "", tex[1]))
    stopifnot(file.exists(filename))

    ## Remove empty lines.
    tex <- tex[-1]
    while (length(tex) &&
           ((nchar(tex[1]) == 0) || tex[1] == "\\\\\\\\")) {
               tex <- tex[-1]
           }
    stopifnot(length(tex) > 0)

    ## Determine the label.
    pattern <- "^Figure[[:space:]]*[{][[][}]fig:[^{]+[{][]][}]:[[:space:]]*"
    label <- trimws(regmatches(tex[1], regexpr(pattern, tex[1])))
    label <- sub("^Figure[[:space:]]*[{][[][}]fig:", "", label)
    label <- sub("[{][]][}]:$", "", label)

    ## Determine the caption.
    caption <- tex
    caption[1] <- sub(pattern, "", caption[1])
    caption[1] <- paste0("\\caption{", caption[1])
    caption <- c(caption, paste0("\\label{fig:", prefix, ":", label, "}"))
    i <- seq_len(length(caption))[-1]
    caption[i] <- paste0("  ", caption[i])
    caption <- c(caption, "}")

    ## Move and rename the figure file.
    to <- paste0("docx_fig_", label, ".png")
    cat(sprintf("  - Write file: %s (Only for info, not added to repo.)\n",
                to))
    file.copy(filename, to)

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
        tex <- style_drop_section(tex, "Tables")$tex
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
    title <- tex_arguments(substr(tex, i, nchar(tex)))

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
      "line width=0.1mm](text) at (0,0){",
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

style_drop_section <- function(tex, section) {
    drop <- NULL
    i <- grep(paste0("^[\\]section[*]?[{]", section, "[}]"), tex)
    if (length(i) && i > 2) {
        section <- normalize_title(section)
        if (startsWith(tex[i - 1], paste0("\\hypertarget{"))) {
            i <- i - 2
        }

        ## Remove empty lines.
        while (i > 1 && ((nchar(tex[i]) == 0) || tex[i] == "\\\\\\\\")) {
            i <- i - 1
        }

        j <- seq(from = i + 1, to = length(tex))
        drop <- tex[j]
        tex <- tex[seq_len(i)]
    }

    list(tex = tex, drop = drop)
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
    args <- tex_arguments(substr(tex, i, nchar(tex)))
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
        rev_x <- lapply(strsplit(x, NULL), function(x) {
            paste(rev(x), collapse = "")
        })
        remove2 <- substr(rev_x[y - 1], 1, 1) == "-" &
                   substr(rev_x[y - 1], 1, 2) != "--"

        remove <- remove | remove2
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
style_chaper_title <- function(tex) {
    tex <- tex_2_one_line(tex)

    ## Extract the arguments for the first hypertarget. It contains
    ## the chapter and label in the second argument.
    i <- regexpr("\\\\hypertarget[{]", tex)
    if (i == -1)
        stop("Unable to find 'hypertarget'")
    i <- i + attr(i, "match.length") - 1
    hypertarget <- tex_arguments(substr(tex, i, nchar(tex)))
    stopifnot(length(hypertarget) == 2)

    ## Extract the tex after the hypertarget. The '+4' is to include
    ## the '{' and '}' for each argument.
    i <- i + nchar(hypertarget[[1]]) + nchar(hypertarget[[2]]) + 4
    tex <- substr(tex, i, nchar(tex))

    ## Determine the title of the chapter from
    ## '\chapter*{title-of-chapter}'
    i <- regexpr("\\\\chapter[*][{]", hypertarget[[2]])
    if (i == -1)
        stop("Unable to find 'chapter'")
    i <- i + attr(i, "match.length") - 1
    title <- tex_arguments(substr(hypertarget[[2]], i, nchar(hypertarget[[2]])))
    stopifnot(length(title) == 1)

    ## Determine the label of the chapter from
    ## '\label{label-of-chapter}'
    i <- regexpr("\\\\label[{]", hypertarget[[2]])
    if (i == -1)
        stop("Unable to find 'label'")
    i <- i + attr(i, "match.length") - 1
    label <- tex_arguments(substr(hypertarget[[2]], i, nchar(hypertarget[[2]])))
    stopifnot(length(label) == 1)

    ## Determine 'texorpdfstring'.
    texorpdfstring <- paste0("\\texorpdfstring{", title, "}{", title, "}")

    ## Conbine all pieces.
    tex <- paste0("\\hypertarget{", hypertarget[[1]], "}{%\n",
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
