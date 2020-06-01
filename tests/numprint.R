ob <- mill:::style_numprint("250000")
ex <- "\\numprint{250000}"
stopifnot(identical(ob, ex))

## A 6 digit range
ob <- mill:::style_numprint("250000--300000")
ex <- "\\numprint{250000}--\\numprint{300000}"
stopifnot(identical(ob, ex))

## A 5 digit range
ob <- mill:::style_numprint("25000--30000")
ex <- "\\numprint{25000}--\\numprint{30000}"
stopifnot(identical(ob, ex))

## A legislation number
ob <- mill:::style_numprint("34653-3")
ex <- "34653-3"
stopifnot(identical(ob, ex))

## An iso number
ob <- mill:::style_numprint("ISO-10272")
ex <- "ISO-10272"
stopifnot(identical(ob, ex))

## A report number
ob <- mill:::style_numprint("dnr 6.2.18-14271/2018")
ex <- "dnr 6.2.18-14271/2018"
stopifnot(identical(ob, ex))

## Test numprint in a figure tex-file.
tex <- c(
"\\begin{figure}[H]",
paste0("  \\includegraphics[width=\\textwidth]",
       "{fig_verotoxinogenic-escherichia-coli_timeseries}"),
"  \\caption{Incidence (per 100000 inhabitants) of",
"    notified human STEC cases in Sweden, 1997--2019. Prior to 2005, only",
"    O157 was required to be reported. In 2005, all serogroups of STEC",
"    including PCR findings became subject for notification. In 2005, 2016",
"    and 2018, the number of cases increased due to one or more domestic",
"    outbreaks.",
"    \\label{fig:verotoxinogenic-escherichia-coli:timeseries}",
"  }",
"\\end{figure}")

tex_exp <- c(
"\\begin{figure}[H]",
paste0("  \\includegraphics[width=\\textwidth]",
       "{fig_verotoxinogenic-escherichia-coli_timeseries}"),
"  \\caption{Incidence (per \\numprint{100000} inhabitants) of",
"    notified human STEC cases in Sweden, 1997--2019. Prior to 2005, only",
"    O157 was required to be reported. In 2005, all serogroups of STEC",
"    including PCR findings became subject for notification. In 2005, 2016",
"    and 2018, the number of cases increased due to one or more domestic",
"    outbreaks.",
"    \\label{fig:verotoxinogenic-escherichia-coli:timeseries}",
"  }",
"\\end{figure}")

stopifnot(identical(mill:::style_numprint(tex), tex_exp))
