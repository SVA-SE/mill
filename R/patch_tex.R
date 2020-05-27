do_apply_patch <- function(from, patchfile, to) {
    if (!file.exists(patchfile)) {
        file.copy(from = from, to = to, overwrite = TRUE)
        return(NULL)
    }

    output <- tryCatch(system2("patch",
                               args = c(from,
                                        "-i", patchfile,
                                        "-o", to),
                               stdout = TRUE, stderr = TRUE),
                       warning = function(w) w)

    if (!identical(output,
                   paste0("patching file ", to, " (read from ", from, ")"))) {
        stop(paste("Unable to apply patch: ", basename(getwd())))
    }

    NULL
}

apply_patch_files <- function(chapter, prefix) {
    files <- list.files(pattern = paste0("^", prefix, "_[^.]+[.]tex"))
    pattern <- paste0("^", prefix, "_", normalize_title(chapter), "_")
    files <- files[!grepl(pattern = pattern, x = files)]

    lapply(files, function(from) {
        patch <- paste0(file_path_sans_ext(from), ".patch")
        to <- sub(paste0("^", prefix),
                  paste0(prefix, "_", normalize_title(chapter)),
                  from)
        do_apply_patch(from, patch, to)
    })

    NULL
}

##' Apply patch
##'
##' @return invisible(NULL)
##' @export
apply_patch <- function() {
    if (in_chapter()) {
        do_apply_patch("text.tex", "typeset.patch", "typeset.tex")

        ## Patching
        chapter <- basename(getwd())
        apply_patch_files(chapter, "tab")
        apply_patch_files(chapter, "fig")
        apply_patch_files(chapter, "infocus")
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
                     from, to, ">", patchfile))

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
