##' Export files to workspace
##'
##' Authors make contributions to chapters in the workspace. The docx
##' files are exported to workspace/chapters/title.
##' @return invisible NULL.
##' @export
export <- function() {
    if (in_chapter()) {
        chapter <- basename(getwd())

        to <- paste0("../../workspace/chapters/", chapter)
        if (!dir.exists(to))
            dir.create(to, recursive = TRUE)

        ## Export text.docx to renamed title.docx
        from <- "text.docx"
        if (!file.exists(from))
            to_docx()
        file.copy(from, paste0(to, "/", chapter, ".docx"), overwrite = TRUE)

        ## Export data and preview files
        files <- c(figure_files("xlsx"), figure_files("pdf"), preview_files())
        lapply(files, function(from) {
            file.copy(from, file.path(to, basename(from)), overwrite = TRUE)
        })
    } else if (in_report()) {
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            export()
            setwd(wd)
        })
    }

    invisible()
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

inject_tab_and_fig <- function(tex) {
    ## Find the references
    pattern <- "[\\]label[{][^}]*[}]|[\\]ref[{][^}]*[}]"
    m <- regmatches(tex, gregexpr(pattern, tex))
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

##' @importFrom tools file_path_sans_ext
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
    to <- paste0("fig_", prefix, "_", label, ".png")
    file.copy(filename, to)
    git2r::add(repository(), paste0("chapters/", chapter, "/", to))

    ## Create the tex-file for the figure.
    lines <- c("\\begin{figure}[H]",
               paste0("  \\includegraphics{", file_path_sans_ext(to), "}"),
               paste0("  ", caption),
               "\\end{figure}")
    filename <- paste0("fig_", prefix, "_", label, ".tex")
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

style_fun <- function(tex, chapter) {
    tmp <- style_drop_section(tex, "Figures")
    extract_figures(tmp$drop, chapter)
    tex <- tmp$tex

    tex <- style_drop_section(tex, "Tables")$tex
    tex <- convert_docx_ref_to_ref(tex, chapter)
    tex <- make_labels_chapter_specific(tex, chapter)
    tex <- hypertargets_chapter_specific(tex, chapter)
    tex <- asterisk(tex, "add")
    tex <- style_toc(tex, output = "tex")
    tex <- style_numprint(tex, output = "tex")

    tmp <- style_drop_section(tex, "In focus")
    tex <- tmp$tex
    if (!is.null(tmp$drop)) {
        filename <- paste0("infocus_",
                           parse_infocus_title(tmp$drop),
                           ".tex")
        writeLines(tmp$drop, filename)
        git2r::add(repository(), filename)
    }

    tex <- add_line_between_references(tex)
    tex <- style_multicols(tex, output = "tex")
    tex <- inject_tab_and_fig(tex)
    tex
}

parse_infocus_title <- function(tex) {
    pattern <- "\\\\subsection\\*\\{"
    ln <- grep(pattern, tex)
    stopifnot(length(ln) == 1)
    title <- sub("}", "", sub(pattern, "", tex[ln]))
    title <- trimws(tolower(substr(start = 1, stop = 50, title)))
    gsub(" ", "-", title)
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
        writeLines(tex, "text.tex")
        if (!is.null(repo))
            add(repo, paste0("chapters/", chapter, "/text.tex"))

        ## Convert tables
        lapply(docx_tables(f_docx), function(tbl) {
            prefix <- normalize_title(chapter)
            filename <- paste0(gsub(":", "_", format(tbl$label)), ".tex")
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

##' Convert the tex ref to docx ref
##'
##' @param tex The tex character vector
##' @return tex character vector
##' @noRd
convert_ref_to_docx_ref <- function(tex) {
    pattern <- "\\\\ref[{]([^:]*)[:][^:]*[:]([^}]*)[}]"
    replacement <- "[\\1:\\2]"
    gsub(pattern, replacement, tex)
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

##' Convert style of empty line from tex to docx
##'
##' @param tex The tex character vector
##' @return tex character vector
##' @noRd
empty_line_from_tex_to_docx <- function(tex) {
    gsub("\\\\\\\\", "", tex)
}

##' Handle multicols when converting between tex and docx
##'
##' @param tex The tex character vector.
##' @param output The output format of the conversion.
##' @return tex character vector.
##' @noRd
style_multicols <- function(tex, output = c("docx", "tex")) {
    remove <- switch(match.arg(output),
                     docx = TRUE,
                     tex  = FALSE)

    if (isTRUE(remove)) {
        ## Check for '\begin{multicols}{2}'.
        i <- grep("^[\\]begin[{]multicols[}][{]2[}]$", tex)
        if (length(i)) {
            stopifnot(identical(length(i), 1L))
            tex <- tex[-i]
        }

        ## Check for '\end{multicols}'.
        i <- grep("^[\\][e][n][d][{]multicols[}]$", tex)
        if (length(i)) {
            stopifnot(identical(length(i), 1L))
            tex <- tex[-i]
        }

        return(tex)
    }

    ## Find the addcontentsline.
    i <- grep("^[\\]addcontentsline[{]toc[}][{]chapter[}][{]", tex)
    stopifnot(identical(length(i), 1L))

    ## Split the tex into two parts and inject the begin multicols
    ## between them.
    tex_a <- tex[seq_len(i)]
    tex_b <- character(0)
    if (i < length(tex))
        tex_b <- tex[seq(from = i + 1, to = length(tex), by = 1)]
    tex <- c(tex_a, "\\begin{multicols}{2}", tex_b)

    ## Add end multicols
    c(tex, "\\end{multicols}")
}

##' Style of numprint when converting between various formats
##'
##' @param tex The tex character vector.
##' @param output The output format of the conversion.
##' @return tex character vector.
##' @noRd
style_numprint <- function(tex, output = c("docx", "tex")) {
    remove <- switch(match.arg(output),
                     docx = TRUE,
                     tex  = FALSE)

    if (isTRUE(remove))
        return(gsub("[\\]numprint[{]([[:digit:]]+)[}]", "\\1", tex))

    ## Find the line for the reference section to make sure not to add
    ## \numprint{} to numbers in references. Use the complete text if
    ## the chapter doesn't contain a reference section.
    i <- grep("^\\\\section[*][{]References[}]", tex)
    if (!length(i))
        i <- length(tex)
    stopifnot(identical(length(i), 1L))
    i <- seq_len(i)

    c(gsub("([[:digit:]]{5,}(?!-))", "\\\\numprint{\\1}", tex[i], perl = TRUE),
      tex[-i])
}

##' Style table of contents when converting between various formats
##'
##' @param tex The tex character vector.
##' @param output The output format of the conversion.
##' @return tex character vector.
##' @noRd
style_toc <- function(tex, output = c("docx", "tex")) {
    remove <- switch(match.arg(output),
                     docx = TRUE,
                     tex  = FALSE)

    if (isTRUE(remove)) {
        i <- grep("^[\\]addcontentsline[{]toc[}][{]chapter[}][{]", tex)

        if (length(i)) {
            ## We expect one '\addcontentsline{toc}{chapter}'.
            stopifnot(identical(length(i), 1L))

            tex <- tex[-i]
        }

        return(tex)
    }

    ## Determine the name of the chapter from
    ## '\chapter*{name-of-chapter}'. Then create a toc using the
    ## chapter name.
    chapter <- paste(tex, collapse = " ")
    chapter <- sub("^.+[\\]chapter[*][{]", "", chapter)
    chapter <- unlist(strsplit(chapter, "}"))[1]
    toc <- paste0("\\addcontentsline{toc}{chapter}{", chapter, "}")

    ## Find the line for the chapter section. Since chapter section
    ## can run over multiple lines, look for the first label '\label{sec:'.
    i <- min(grep("\\\\label[{]sec[:]", tex))
    stopifnot(identical(length(i), 1L))

    ## Split the tex into two parts and inject the toc between them.
    tex_a <- tex[seq_len(i)]
    tex_b <- character(0)
    if (i < length(tex))
        tex_b <- tex[seq(from = i + 1, to = length(tex), by = 1)]

    c(tex_a, toc, tex_b)
}

##' Convert from tex to docx
##'
##' Use pandoc (http://pandoc.org/) to convert from 'tex' to
##' 'docx'. The chapter 'text.tex' is converted to 'text.docx'. Each
##' chapter 'text.docx' is added, but not commited, to the report git
##' repository.
##' @param repo the report git repository.
##' @return invisible NULL.
##' @export
to_docx <- function(repo = NULL) {
    if (in_chapter()) {
        ## Clean up changes made in from_docx_chapter()
        tex <- readLines("text.tex")
        tex <- asterisk(tex, "remove")
        tex <- convert_ref_to_docx_ref(tex)
        tex <- empty_line_from_tex_to_docx(tex)
        tex <- style_toc(tex, output = "docx")
        tex <- style_multicols(tex, output = "docx")
        tex <- style_numprint(tex, output = "docx")
        f_tex <- tempfile(fileext = ".tex")
        writeLines(tex, f_tex)
        f_docx <- "text.docx"
        unlink(f_docx)

        ## Convert to docx
        pandoc(paste("--top-level-division=chapter ",
                     shQuote(f_tex), "-o", shQuote(f_docx)))
        if (!is.null(repo))
            add(repo, paste0("chapters/", basename(getwd()), "/text.docx"))
    } else if (in_report()) {
        repo <- repository()
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            to_docx(repo = repo)
            setwd(wd)
        })
    }

    invisible()
}

##' Roundtrip tex to docx
##'
##' @return invisible NULL.
##' @importFrom git2r repository
##' @importFrom git2r reset
##' @importFrom git2r status
##' @export
roundtrip <- function() {
    if (in_chapter()) {
        ## Check if the working tree is clean
        repo <- repository("../..")
        s <- status(repo)
        if (length(c(s$staged, s$unstaged)))
            stop("Working tree is not clean")

        to_docx(repo = NULL)
        from_docx(repo = NULL)

        ## The roundtrip is clean if the tex-file is unchanged
        unstaged <- unlist(status(repo)$unstaged)
        tex_file <- paste0("chapters/", basename(getwd()), "/text.tex")
        if (!(tex_file %in% unstaged))
            reset(commits(repo, n = 1)[[1]], "hard")
    } else if (in_report()) {
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            roundtrip()
            setwd(wd)
        })
    }

    invisible()
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
