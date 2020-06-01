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
