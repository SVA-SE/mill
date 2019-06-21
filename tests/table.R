library("mill")

stopifnot(identical(mill:::merge_font_styles("\\textit{A  }\\textit{B}"),
                    "\\textit{A  B}"))

stopifnot(identical(mill:::merge_font_styles("\\textit{A}  \\textit{B}"),
                    "\\textit{A  B}"))
