ob <- mill:::style_numprint("250000", output = "tex")
ex <-"\\numprint{250000}"
stopifnot(identical(ob, ex))

ob <- mill:::style_numprint("250000--300000", output = "tex")
ex <-"\\numprint{250000}--\\numprint{300000}"
stopifnot(identical(ob, ex))

ob <- mill:::style_numprint("25000--30000", output = "tex")
ex <-"\\numprint{25000}--\\numprint{30000}"
stopifnot(identical(ob, ex))

ob <- mill:::style_numprint("250000-300000", output = "tex")
ex <-"\\numprint{250000}-\\numprint{300000}"
stopifnot(identical(ob, ex))

ob <- mill:::style_numprint("25000-30000", output = "tex")
ex <-"\\numprint{25000}-\\numprint{30000}"
stopifnot(identical(ob, ex))
