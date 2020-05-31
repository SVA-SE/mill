wd <- tempdir()
file.create(file.path(wd, "README.org"))
file.copy("chapters",
          wd,
          recursive = TRUE)
wd <- file.path(wd, "chapters", "test1")
setwd(wd)

mill::apply_patch()

## Ensure that the two new files were created
ob <- list.files()
ex <- c("fig_test1_timeseries.tex",
        "fig_timeseries.tex",
        "text.tex",
        "typeset.tex")
stopifnot(identical(ob, ex))

## Because no changes were made there should be no patches created:
mill::create_patch()
ob <- list.files()
stopifnot(identical(ob, ex))

## If we make a change to typeset and fig_test1_timeseries.tex then we
## expect patches to be written.
writeLines(c(readLines("fig_test1_timeseries.tex"),
             "bar"), con = "fig_test1_timeseries.tex")

writeLines(c(readLines("typeset.tex"),
             "bar"), con = "typeset.tex")

## Now create the patches.
mill::create_patch()
ob <- list.files()
ex <- c("fig_test1_timeseries.tex",
        "fig_timeseries.patch",
        "fig_timeseries.tex",
        "text.tex",
        "typeset.patch",
        "typeset.tex")
stopifnot(identical(ob, ex))
