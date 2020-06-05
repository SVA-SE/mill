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
