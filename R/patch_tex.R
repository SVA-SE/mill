do_apply_patch <- function(from, patchfile, to, verbose, force) {
    if (!file.exists(patchfile)) {
        file.copy(from = from, to = to, overwrite = TRUE)
        return(NULL)
    }

    if (isTRUE(verbose))
        cat(sprintf("  - %s\n", patchfile))

    output <- tryCatch(system2("patch",
                               args = c(from,
                                        "--binary",
                                        "-i", patchfile,
                                        "-o", to),
                               stdout = TRUE, stderr = TRUE),
                       warning = function(w) w)

    t1 <- identical(output,
                    paste0("patching file ", to, " (read from ", from, ")"))
    if (!t1 & !force) {
        stop(paste("Unable to apply patch: ", basename(getwd())))
    }

    if (!t1 & force) {
        warning(paste("Unable to apply patch: ", basename(getwd())))
    }

    NULL
}

apply_patch_files <- function(chapter, prefix, verbose, force) {
    files <- list.files(pattern = paste0("^", prefix, "_[^.]+[.]tex"))
    pattern <- paste0("^", prefix, "_", normalize_title(chapter), "_")
    files <- files[!grepl(pattern = pattern, x = files)]

    lapply(files, function(from) {
        patch <- paste0(file_path_sans_ext(from), ".patch")
        to <- sub(paste0("^", prefix),
                  paste0(prefix, "_", normalize_title(chapter)),
                  from)
        do_apply_patch(from, patch, to, verbose, force)
    })

    NULL
}

##' Apply patch
##'
##' @param verbose give information about the process.
##' @param force return NULL from function even if patch doesn't apply cleanly
##' @return invisible(NULL)
##' @export
apply_patch <- function(verbose = TRUE,
                        force = FALSE) {
    if (in_chapter()) {
        chapter <- basename(getwd())
        if (isTRUE(verbose))
            cat(sprintf("Apply patches: %s\n", chapter))

        do_apply_patch("text.tex",
                       "typeset.patch",
                       "typeset.tex",
                       verbose,
                       force)
        apply_patch_files(chapter, "tab", verbose, force)
        apply_patch_files(chapter, "fig", verbose, force)
        apply_patch_files(chapter, "infocus", verbose, force)
    } else if (in_report()) {
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            apply_patch()
            setwd(wd)
        })
    }

    invisible(NULL)
}

do_create_patch <- function(from, to, patchfile) {
    if (!file.exists(to))
        stop(paste0("Missing file '", to, "'"))

    if (!file.exists(from))
        stop(paste0("Missing file '", from, "'"))

    system2("diff",
            args = c("-c", "--label=text",
                     "--label=typeset",
                     from, to),
            stdout = patchfile)

    ## Drop empty patch
    if (!file.size(patchfile))
        unlink(patchfile)

    NULL
}

create_patch_files <- function(chapter, prefix) {
    files <- list.files(pattern = paste0("^", prefix, "_[^.]+[.]tex"))
    pattern <- paste0("^", prefix, "_", normalize_title(chapter), "_")
    i <- grepl(pattern = pattern, x = files)
    files <- files[!grepl(pattern = pattern, x = files)]

    lapply(files, function(from) {
        patch <- paste0(file_path_sans_ext(from), ".patch")
        to <- sub(paste0("^", prefix),
                  paste0(prefix, "_", normalize_title(chapter)),
                  from)
        do_create_patch(from, to, patch)
    })

    NULL
}

##' Create patch
##'
##' @return invisible(NULL)
##' @export
create_patch <- function() {
    if (in_chapter()) {
        do_create_patch("text.tex", "typeset.tex", "typeset.patch")

        ## Table diff
        chapter <- basename(getwd())
        create_patch_files(chapter, "tab")

        ## Figure diff
        chapter <- basename(getwd())
        create_patch_files(chapter, "fig")

        ## Fix to handle named infocus files
        create_patch_files(chapter, "infocus")

    } else if (in_report()) {
        lapply(list.files("chapters"), function(chapter) {
            wd <- setwd(paste0("chapters/", chapter))
            on.exit(setwd(wd), add = TRUE)
            create_patch()
        })
    }

    invisible(NULL)
}
