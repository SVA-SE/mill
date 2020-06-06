library(mill)

tex <- c(
    paste0("\\hypertarget{sec:animal-databases:animal-registers-",
           "and-data-sources-used-in-surveillance}{%"),
    "\\chapter*{Animal registers and data sources used in",
    paste0("surveillance}\\label{sec:animal-databases:animal-",
           "registers-and-data-sources-used-in-surveillance}}"),
    "",
    "\\hypertarget{sec:animal-databases:the-central-register-of-holdings}{%",
    "\\section*{The Central Register of",
    "Holdings}\\label{sec:animal-databases:the-central-register-of-holdings}}",
    "",
    "The Swedish Board of Agriculture is responsible for maintaining the")

tex <- mill:::tex_2_one_line(tex)

stopifnot(identical(mill:::tex_arguments(substr(tex, 13, nchar(tex)))$m,
                    c(paste0("sec:animal-databases:animal-registers-",
                             "and-data-sources-used-in-surveillance"),
                      paste0("%\n\\chapter*{Animal registers and data ",
                             "sources used in\nsurveillance}\\label{",
                             "sec:animal-databases:animal-registers-and-",
                             "data-sources-used-in-surveillance}"))))

tex <- "\\includegraphics[width=3.6181in,height=2.32592in]{./media/image1.png}"
args <- mill:::tex_arguments(substr(tex, 17, nchar(tex)))
stopifnot(identical(args$o, "width=3.6181in,height=2.32592in"))
stopifnot(identical(args$m, "./media/image1.png"))

tex <- "\\includegraphics[width=3.6181in,height=2.32592in]{./media/image1.png}"
cmd <- mill:::tex_cmd(tex)
stopifnot(identical(cmd$cmd, "\\includegraphics"))
stopifnot(identical(cmd$o, "width=3.6181in,height=2.32592in"))
stopifnot(identical(cmd$m, "./media/image1.png"))
stopifnot(identical(mill:::tex_cmd_nchar(cmd), nchar(tex)))

tex <- "\\section*{some section}"
cmd <- mill:::tex_cmd(tex)
stopifnot(identical(cmd$cmd, "\\section*"))
stopifnot(identical(cmd$o, character(0)))
stopifnot(identical(cmd$m, "some section"))
stopifnot(identical(mill:::tex_cmd_nchar(cmd), nchar(tex)))
