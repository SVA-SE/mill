library(mill)

tex <- c(
paste0("\\hypertarget{sec:cryptosporidiosis:national-increase-in-the-",
       "number-of-reported-cases-of-cryptosporidiosis-in-humans-oct-dec",
       "-2019}{%"),
"\\subsection*{National \textit{increase} in the number of reported cases of",
"cryptosporidiosis in humans Oct-Dec",
paste0("2019}\\label{sec:cryptosporidiosis:national-increase-in-the-number-of",
       "-reported-cases-of-cryptosporidiosis-in-humans-oct-dec-2019}}"),
"",
"There was a national increase in the number of reported cases of",
"cryptosporidiosis in the autumn of 2019. 58 percent (n=450/771) of",
"annual domestic cases were reported from 1 October-31 December. Reported",
"cases peaked during three weeks in November (Figure",
paste0("\\ref{fig:cryptosporidiosis:timeseries-months}). ",
       "An outbreak investigation was initiated"),
"and 300 isolates from cases were typed as part of that investigation.",
"Five foodborne outbreaks were identified during this period, all",
"attributed to infection with \\emph{C. parvum} (n=285). Dominating",
"subtypes were IIaA22G1c (n=122) and IIdA24G1 (n= 65). Both these",
"subtypes have been detected both earlier in 2019 and in the previous",
"years. During the outbreak period, 122 cases in ten different regions",
"were caused by subtype IIdA22G1c. The county of Stockholm had the most",
"cases (n=58) followed by Västra Götaland (n=16) and Halland (n=16)",
"counties. The median age of the cases was 39 years (2--83 years) with no",
"significant gender difference (52\\% women, 48\\% men). Using surveys in",
"collaboration with other authorities, unpasteurised juice with spinach",
"was identified as the source of infection with subtype IIdA22G1c.",
"",
"During the same period, cryptosporidiosis in 65 cases in twelve",
"different regions were caused by \\emph{C. parvum} subtype IIdA24G1. The",
"county of Västra Götaland had the most cases (n=13) followed by",
"Stockholm (n=12), Östergötland (n=12) and Jönköping (n=11) counties.",
"More women (62\\%) than men (38\\%) were infected and the median age was",
"40 years (11--79 years). No source of infection was identified for",
"subtype IIdA24G1. Other common outbreak subtypes were IIdA20G1e (n=23)",
"and IIdA21G1* (n= 20). These subtypes were also found in cases that had",
"visited different Christmas buffets in December where fresh kale from",
"four kale producers in the southern parts of Sweden was identified as",
"the probable source of infection.")

tex <- mill:::tex_2_one_line(tex)
i <- regexpr("\\\\subsection[*][{]", tex)
if (i == -1)
    stop("Unable to find 'subsection'")
i <- i + attr(i, "match.length") - 1

stopifnot(identical(mill:::tex_argument(substr(tex, i, nchar(tex))),
                    paste0("National \textit{increase} in the number ",
                           "of reported cases of\ncryptosporidiosis in ",
                           "humans Oct-Dec\n2019")))
