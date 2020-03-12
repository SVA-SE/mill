##' @title work_status
##'
##' @importFrom git2r odb_blobs
##' @importFrom git2r repository
##' @param path path to repo
##' @return a data.frame
##' @export
work_status <- function(path = ".") {
    report <- load_report(path)
    repo <- repository()
    blobs <- odb_blobs(repo)
    workspace <- unlist(lapply(strsplit(blobs$path, "/"), "[", 1))
    blobs <- blobs[workspace %in% c("workspace"), ]
    chapters_list <- data.frame(name = list.files("chapters"),
                                stringsAsFactors = FALSE)
    blobs$chap_match <- match(basename(blobs$path), chapters_list$name)
    chapters_list$commits <- as.numeric(table(blobs$chap_match))
    blobs$week <- factor(format(blobs$when, "%w"), levels = 1:52)
    chapters_list$sparkline <- tapply(blobs$week,
                                      blobs$chap_match,
                                      table)
    chapters_list
}

##' @title sparkline
##'
##' @param path path to repo
##' @param ylim ylim of graphs
##' @param xlim xlim of graphs
##' @importFrom graphics par
##' @return plots
##' @export
sparkline <- function(path = ".",
                      ylim = NULL,
                      xlim = NULL) {
    df <- work_status(path)
    nrows <- 8
    ncols <- nrow(df) %/% nrows
    if((nrow(df) %% nrows) != 0) ncols <- ncols + 1
    par(mfrow = c(nrows, ncols),
        mar = c(1,1,3,1))
    for(i in seq_len(nrow(df))) {
        plot_object <- df[i, "sparkline"][[1]]
        plot(x = names(plot_object),
             y = plot_object,
             main = df$name[i],
             type = "l",
             ylab = "file changes",
             xlab = "week",
             ylim = ylim,
             xlim = xlim)
    }
}
