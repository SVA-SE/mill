##' Load all tables in a docx file
##'
##' @param filename name of the docx file.
##' @importFrom utils unzip
##' @importFrom xml2 xml_find_all
##' @importFrom xml2 xml_text
##' @importFrom xml2 read_xml
##' @export
##' @examples
##' ## Load tables from a bundled docx file.
##' tbl <- docx_tables(system.file("extdata/table.docx", package = "doctex"))
##'
##' ## View the layout of the tables
##' tbl
##'
##' ## View the column widths
##' sapply(tbl, col_widths)
docx_tables <- function(filename)
{
    on.exit(unlink(file.path(tempdir(), "document.xml")), add = TRUE)

    ## Unzip the content of the word file.
    unzip(filename, "word/document.xml", junkpaths = TRUE, exdir = tempdir())

    ## Parse the content of the word file
    doc <- read_xml(file.path(tempdir(), "document.xml"))

    ## Extract the tables
    x <- xml_find_all(doc, xpath = "//w:tbl")
    tbls <- lapply(seq_len(length(x)), function(i) {
        docx_table(x[[i]])
    })

    tbls
}

##' Create a docx table object
##'
##' A \code{docx_table} consists of a caption, a label, table data and
##' footnotes.
##' @param xml the xml node for the docx table.
##' @importFrom xml2 xml_find_first
##' @importFrom xml2 xml_name
##' @return a \code{docx_table} object.
##' @export
docx_table <- function(xml)
{
    stopifnot(identical(xml_name(xml), "tbl"))
    p <- xml_find_first(xml, "preceding-sibling::w:p[1]")
    structure(list(caption  = docx_caption(p),
                   label    = docx_label(p),
                   content  = xml,
                   footnote = docx_footnote(xml)),
              class = "docx_table")
}

##' Create a docx caption object
##'
##' @param xml the xml node for the docx caption.
##' @importFrom xml2 xml_name
##' @return a \code{docx_caption} object.
##' @export
docx_caption <- function(xml)
{
    structure(list(content = docx_paragraph(xml)), class = "docx_caption")
}

##' @importFrom xml2 xml_text
##' @export
format.docx_caption <- function(x, output = c("ascii", "tex"), ...)
{
    str <- trimws(format(x$content, output = output))

    if (match.arg(output) == "ascii")
        return(str)

    ## Determine the label for the caption.
    pattern <- "^(Table|Figure)[[:space:]]*[[](tab|fig):[^]]+[]]:[[:space:]]*"
    str <- sub(pattern, "", str)

    paste0("\\caption{", str, "}")
}

##' @export
print.docx_caption <- function(x, ...)
{
    cat(format(x, ...), "\n", sep = "")
    invisible(x)
}

##' Create a docx label object
##'
##' @param xml the xml node for the docx label.
##' @importFrom xml2 xml_name
##' @return a \code{docx_label} object.
##' @export
docx_label <- function(xml)
{
    structure(list(content = docx_paragraph(xml)), class = "docx_label")
}

##' @importFrom xml2 xml_text
##' @export
format.docx_label <- function(x, output = c("ascii", "tex"), prefix = "", ...)
{
    pattern <- "^(Table|Figure)[[:space:]]*[[](tab|fig):[^]]+[]]:[[:space:]]*"
    str <- trimws(format(x$content, output = "ascii"))
    lbl <- regmatches(str, regexpr(pattern, str))
    lbl <- sub("^Table[[:space:]]*[[]", "", sub("[]]:[[:space:]]*$", "", lbl))

    ## Check if the label should be prefixed e.g. 'tab:cattle' ->
    ## 'tab:prefix:cattle'.
    if (nchar(prefix))
        lbl <- sub("^tab:", paste0("tab:", prefix, ":"), lbl)

    if (match.arg(output) == "ascii")
        return(lbl)

    paste0("\\label{", lbl, "}")
}

##' @export
print.docx_label <- function(x, ...)
{
    cat(format(x, ...), "\n", sep = "")
    invisible(x)
}

##' @export
length.docx_label <- function(x)
{
    nchar(format(x$content, output = "ascii"))
}

##' Create a docx footnote object
##'
##' @param xml the xml node for the docx footnotes.
##' @importFrom xml2 xml_name
##' @return a \code{docx_footnote} object.
##' @export
docx_footnote <- function(xml)
{
    stopifnot(identical(xml_name(xml), "tbl"))
    content <- list()
    i <- 1
    repeat {
        xpath <- paste0("following-sibling::w:p[",
                        i,
                        "][./w:pPr/w:pStyle[@w:val='FootnoteText' or @w:val='Fotnotstext']]")
        footnote <- xml_find_first(xml, xpath)
        if (is.na(footnote))
            break
        content[[i]] <- docx_paragraph(footnote)
        i <- i + 1
    }

    structure(list(content = content), class = "docx_footnote")
}

##' @importFrom xml2 xml_text
##' @export
format.docx_footnote <- function(x, indentation = "", ...)
{
    lines <- paste0(indentation, "\\begin{tablenotes}")

    ## Write items.
    indentation <- paste0("  ", indentation)
    for (i in seq_len(length(x$content))) {
        lines <- c(lines,
                   paste0(indentation,
                          "\\item ",
                          format(x$content[[i]])))
    }

    indentation <- substr(indentation, 3, nchar(indentation))
    lines <- c(lines, paste0(indentation, "\\end{tablenotes}"))

    lines
}

##' @export
print.docx_footnote <- function(x, ...)
{
    cat(format(x, ...), "\n", sep = "")
    invisible(x)
}

##' @export
length.docx_footnote <- function(x)
{
    length(x$content)
}

##' Create a docx paragraph object
##'
##' @param xml the xml node for the docx paragraph.
##' @importFrom xml2 xml_name
##' @return a \code{docx_paragraph} object.
##' @export
docx_paragraph <- function(xml)
{
    stopifnot(identical(xml_name(xml), "p"))
    structure(list(content = xml), class = "docx_paragraph")
}

##' @export
format.docx_paragraph <- function(x, ...)
{
    lines <- lapply(xml_find_all(x$content, "w:r"), function(r) {
        line <- character(0)

        superscript <- xml_find_first(r, "w:rPr/w:vertAlign[@w:val='superscript']")
        if (!is.na(superscript))
            line <- paste0(line, "\\textsuperscript{")
        italic <- xml_find_first(r, "w:rPr/w:i")
        if (!is.na(italic))
            line <- paste0(line, "\\textit{")
        bold <- xml_find_first(r, "w:rPr/w:b")
        if (!is.na(bold))
            line <- paste0(line, "\\textbf{")
        line <- paste0(line, xml_text(r))
        if (!is.na(bold))
            line <- paste0(line, "}")
        if (!is.na(italic))
            line <- paste0(line, "}")
        if (!is.na(superscript))
            line <- paste0(line, "}")
        line
    })

    p <- paste0(unlist(lines), collapse = "")

    ## Escape '%'
    p <- gsub("%", "\\%", p, fixed = TRUE)

    ## Numprint
    p <- gsub("([[:digit:]]{5,}(?!-))", "\\\\numprint{\\1}", p, perl = TRUE)

    ## replace U+00a0 with ~ '%'
    p <- gsub("\u00a0", "~", p, fixed = TRUE)

    p
}

##' @export
print.docx_paragraph <- function(x, ...)
{
    cat(format(x, ...), "\n", sep = "")
    invisible(x)
}

##' Column widths
##'
##' Get the columns widths of a docx table.
##' @param tbl table to get the column widths from.
##' @return numeric vector with column widths.
##' @importFrom xml2 xml_attr
##' @export
col_widths <- function(tbl)
{
    as.numeric(xml_attr(xml_children(xml_child(tbl$content, "w:tblGrid")), "w"))
}

##' @importFrom xml2 xml_child
##' @importFrom xml2 xml_children
##' @importFrom xml2 xml_length
##' @importFrom xml2 xml_name
##' @export
dim.docx_table <- function(x)
{
    c(sum(xml_name(xml_children(x$content)) == "tr"),
      xml_length(xml_child(x$content, "w:tblGrid")))
}

##' @importFrom xml2 xml_find_all
##' @export
dim.docx_w_tr <- function(x)
{
    c(1, length(xml_find_all(x$content, "w:tc")))
}

##' Dimensions of a cell.
##'
##' Note that if the cell is part of a vertically merged region, then
##' the first dimension is \code{NA}.
##' @param x the cell to get the dimension from
##' @importFrom xml2 xml_attr
##' @importFrom xml2 xml_find_first
##' @export
dim.docx_w_tc <- function(x)
{
    ## Check if the cell is part of a vertically merged region.
    i <- xml_find_first(x$content, "w:tcPr/w:vMerge")
    if (is.na(i)) {
        i <- 1
    } else {
        ## The cell is part of a vertically merged region. Let the
        ## number of rows be undefined.
        i <- NA_integer_
    }

    ## Check if the cell is part of a horizontically merged region.
    j <- as.numeric(xml_attr(xml_find_first(x$content, "w:tcPr/w:gridSpan"), "val"))
    if (is.na(j))
        j <- 1

    c(i, j)
}

##' Determine if a cell is part of vertically merged region
##'
##' @param tbl the table.
##' @param i the index to the current row.
##' @param j the index to the current cell in the current row.
##' @noRd
is_vmerge_region <- function(tbl, i, j)
{
    is.na(nrow(tbl[i][j]))
}

##' Determine if a cell continues a vertically merged region.
##'
##' Find the cell below and make sure is starts at the same grid
##' column and that the cells have identical col-span.
##' @param tbl the table.
##' @param i the index to the current row.
##' @param j the index to the current cell in the current row.
##' @noRd
vmerge_continue <- function(tbl, i, j)
{
    ## Last row?
    if (i >= nrow(tbl))
        return(FALSE)

    ## Current and next row.
    row <- tbl[i]
    next_row <- tbl[i+1]

    ## Determine the grid column for where the current cell in the
    ## current row starts.
    grid_j <- 1 + sum(vapply(seq_len(j-1), function(jj) {
        ncol(row[jj])
    }, numeric(1)))

    ## Try to locate a cell below.
    ncol_below <- ncol(next_row)
    grid_j_below <- 1
    j_below <- 1
    repeat {
        if (j_below > ncol_below || grid_j_below > grid_j)
            return(FALSE)
        if (grid_j_below == grid_j)
            break
        grid_j_below <- grid_j_below + ncol(next_row[j_below])
        j_below <- j_below + 1
    }
    cell_below <- next_row[j_below]

    ## Check that the col-span of the current cell and the cell below
    ## is identical.
    if (!identical(ncol(row[j]), ncol(cell_below)))
        return(FALSE)

    ## Finally, check if the cell below continues a vertically merged
    ## region.
    merge <- xml_find_first(cell_below$content, "w:tcPr/w:vMerge")
    if (is.na(merge))
        return(FALSE)

    ## If the attribute value is equal to 'restart' then start a new
    ## vertically merged region in this table. If this attribute is
    ## 'continue' or omitted, continue the vertically merged region.
    val <- xml_attr(merge, "val")
    if (is.na(val) || val == "continue")
        return(TRUE)
    FALSE
}

format_docx_table_as_ascii <- function(tbl, output, ...)
{
    ## Line for the top and bottom borders.
    l <- paste0("+", paste0(rep("---", ncol(tbl)), collapse = "+"), "+")

    lines <- paste0(format(tbl$caption, output), "\n")
    for (i in seq_len(nrow(tbl))) {
        if (i == 1)
            lines <- c(lines, paste0(l, "\n"))

        row <- tbl[i]
        for (j in seq_len(ncol(row))) {
            ## Start each row with a vertical border '|' and a '+'.
            if (j == 1) {
                vb <- "|"
                hb <- "+"
            }

            ## Col-span of the current cell.
            nc <- ncol(row[j])

            ## Append a vertical border ('|') to the next cell on the
            ## right.
            vb <- paste0(vb, paste0(rep("   ", nc), collapse = " "), "|")

            ## Append a horizontal border ('-') to the next cell below
            ## if the cell is not part a vertically merged region that
            ## continues.
            if (is_vmerge_region(tbl, i, j) && vmerge_continue(tbl, i, j)) {
                b <- "   "
            } else {
                b <- "---"
            }
            hb <- paste0(hb, paste0(rep(b, nc), collapse = "+"), "+")
        }

        ## Last row?
        if (i == nrow(tbl))
            hb <- l

        lines <- c(lines, paste0(vb, "\n"))
        lines <- c(lines, paste0(hb, "\n"))
    }

    lines
}

is_midrule <- function(tbl, i)
{
    ## Check if every cell in the row contains a bottom border.
    xpath <- "w:tc/w:tcPr/w:tcBorders/w:bottom"
    b <- xml_find_all(tbl[i]$content, xpath)
    length(b) == ncol(tbl) && i < nrow(tbl)
}

format_docx_table_as_tex <- function(tbl,
                                     output,
                                     indentation = "",
                                     standalone = FALSE,
                                     threeparttable = FALSE,
                                     position = "[H]",
                                     addlinespace = 3,
                                     ...)
{
    lines <- character(0)

    if (isTRUE(standalone)) {
        lines <- c(lines, paste0(indentation, "\\documentclass{article}"))
        lines <- c(lines, paste0(indentation, "\\usepackage{booktabs}"))
        lines <- c(lines, paste0(indentation, "\\usepackage[margin=1in]{geometry}"))
        if (length(tbl$footnote) || isTRUE(threeparttable))
            lines <- c(lines, paste0(indentation, "\\usepackage{threeparttable}"))
        lines <- c(lines, paste0(indentation, "\\begin{document}"))
    }

    lines <- c(lines, paste0(indentation, "\\begin{table}", position))

    if (length(tbl$footnote) || isTRUE(threeparttable)) {
        indentation <- paste0("  ", indentation)
        lines <- c(lines, paste0(indentation, "\\begin{threeparttable}"))
    }

    indentation <- paste0("  ", indentation)
    lines <- c(lines, paste0(indentation, format(tbl$caption, output, ...)))

    lines <- c(lines, paste0(indentation, "\\begin{tabular}{"))
    indentation <- paste0("    ", indentation)
    for (i in seq_len(ncol(tbl))) {
        align <- ifelse (i == 1, "l", "r")
        if (i == ncol(tbl))
            align <- paste0(align, "}")
        lines <- c(lines, paste0(indentation, align))
    }
    lines <- c(lines, "")

    indentation <- substr(indentation, 3, nchar(indentation))
    lines <- c(lines, paste0(indentation,"\\toprule"), "")

    ## Keep track of the line number for the last '\midrule'.
    midrule <- NA

    for (i in seq_len(nrow(tbl))) {
        row <- tbl[i]

        for (j in seq_len(ncol(row))) {
            cell <- row[j]
            value <- docx_paragraph(xml_find_first(cell$content, "w:p"))
            value <- format(value)

            if (ncol(cell) > 1) {
                value <- paste0("\\multicolumn{", ncol(cell),
                                "}{l}{", value, "}")
            }

            if (j == ncol(row)) {
                value <- paste0(value, " \\\\")
            } else {
                value <- paste0(value, " &")
            }

            lines <- c(lines, paste0(indentation, value))
        }

        lines <- c(lines, "")

        if (is_midrule(tbl, i)) {
            lines <- c(lines, paste0(indentation, "\\midrule"), "")
            midrule <- i
        } else if (((i - midrule) %% addlinespace) == 0 && i < nrow(tbl)) {
            lines <- c(lines, paste0(indentation, "\\addlinespace"))
            lines <- c(lines, "")
        }

        if (i == nrow(tbl))
            lines <- c(lines, paste0(indentation, "\\bottomrule"), "")
    }

    indentation <- substr(indentation, 3, nchar(indentation))
    lines <- c(lines, paste0(indentation, "\\end{tabular}"))

    if (length(tbl$footnote) || isTRUE(threeparttable)) {
        if (length(tbl$footnote))
            lines <- c(lines, format(tbl$footnote, indentation))

        if (length(tbl$label))
            lines <- c(lines, paste0(indentation, format(tbl$label, output, ...)))

        indentation <- substr(indentation, 3, nchar(indentation))
        lines <- c(lines, paste0(indentation, "\\end{threeparttable}"))
    } else if (length(tbl$label)) {
        lines <- c(lines, paste0(indentation, format(tbl$label, output, ...)))
    }

    indentation <- substr(indentation, 3, nchar(indentation))
    lines <- c(lines, paste0(indentation, "\\end{table}"))

    if (isTRUE(standalone)) {
        indentation <- substr(indentation, 3, nchar(indentation))
        lines <- c(lines, paste0(indentation, "\\end{document}"))
    }

    lines
}

##' @export
format.docx_table <- function(x, output = c("ascii", "tex"), ...)
{
    f <- switch(match.arg(output),
                ascii = format_docx_table_as_ascii,
                tex   = format_docx_table_as_tex)

    f(tbl = x, output = output, ...)
}

##' @export
print.docx_table <- function(x, ...)
{
    cat(format(x, ...), "\n", sep = "\n")
    invisible(x)
}

##' @export
`[.docx_table` <- function(x, i, j, drop = TRUE)
{
    r <- xml_find_all(x$content, "w:tr")

    rows <- lapply(seq_len(length(r)), function(r_i) {
        structure(list(content = r[[r_i]]), class = "docx_w_tr")
    })[i]

    if (isTRUE(drop) && length(i) == 1)
        rows <- rows[[1]]

    rows
}

##' @export
`[.docx_w_tr` <- function(x, i, j, drop = TRUE)
{
    tc <- xml_find_all(x$content, "w:tc")

    cols <- lapply(seq_len(length(tc)), function(tc_i) {
        structure(list(content = tc[[tc_i]]), class = "docx_w_tc")
    })[i]

    if (isTRUE(drop) && length(i) == 1)
        cols <- cols[[1]]

    cols
}

##' @export
format.docx_w_tr <- function(x, ...)
{
    l1 <- paste0("+", paste0(rep("---", ncol(x)), collapse = "+"), "+\n")
    l2 <- paste0("|", paste0(rep("   ", ncol(x)), collapse = "|"), "|\n")

    c(l1, l2, l1)
}

##' @export
print.docx_w_tr <- function(x, ...)
{
    cat(format(x, ...), "\n", sep = "")
    invisible(x)
}

##' @importFrom xml2 xml_text
##' @export
format.docx_w_tc <- function(x, ...)
{
    xml_text(x$content)
}

##' @export
print.docx_w_tc <- function(x, ...)
{
    cat(format(x, ...), "\n", sep = "")
    invisible(x)
}
