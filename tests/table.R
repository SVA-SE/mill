library(mill)

stopifnot(identical(mill:::merge_font_styles("\\textit{A  }\\textit{B}"),
                    "\\textit{A  B}"))

stopifnot(identical(mill:::merge_font_styles("\\textit{A}  \\textit{B}"),
                    "\\textit{A  B}"))

stopifnot(identical(mill:::merge_font_styles("\\textit{A. }\\textit{woodi}"),
                    "\\textit{A. woodi}"))

stopifnot(identical(mill:::merge_font_styles("\\textbf{1}\\textbf{06}"),
                    "\\textbf{106}"))

str <- paste0("\\textit{Mycoplasma }\\textit{gallisepticum}\\textit{ ",
              "/ Mycoplasma }\\textit{synoviae}")
stopifnot(identical(
    mill:::merge_font_styles(str),
    "\\textit{Mycoplasma gallisepticum / Mycoplasma synoviae}"))

str <- paste0("\\textit{Mycoplasma }\\textit{gallisepticum}\\textit{/}",
              "\\textit{synoviae} some more text.")
stopifnot(identical(
    mill:::merge_font_styles(str),
    "\\textit{Mycoplasma gallisepticum/synoviae} some more text."))

stopifnot(identical(
    format(docx_tables("test-file-001.docx")[[1]], output = "tex"),
    c("\\begin{table}[H]",
      "  \\begin{threeparttable}",
      "    \\caption{This is a simple table.}",
      "    \\begin{tabular}{",
      "        l",
      "        r}",
      "",
      "      \\toprule",
      "",
      "      \\textbf{Total} &",
      "      \\textbf{1}\\textsuperscript{\\textbf{A}} \\\\",
      "",
      "      \\bottomrule",
      "",
      "    \\end{tabular}",
      "    \\begin{tablenotes}",
      "      \\item \\textsuperscript{A}Footnote.",
      "    \\end{tablenotes}",
      "    \\label{tab:test}",
      "  \\end{threeparttable}",
      "\\end{table}")))
