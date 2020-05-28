##' A ggplot2 theme for plotting in the report
##'
##' @return A theme for ggplot2
##' @importFrom ggplot2 element_blank
##' @importFrom ggplot2 element_line
##' @importFrom ggplot2 element_rect
##' @importFrom ggplot2 element_text
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 theme_bw
##' @importFrom grid unit
##' @importFrom RColorBrewer brewer.pal
##' @export
sva_theme <- function() {
    ## Generate the colors for the chart
    palette <- brewer.pal("Greys", n = 9)
    color.background <- palette[1]
    color.grid.major <- palette[3]
    color.axis.text <- palette[6]
    color.axis.title <- palette[7]
    color.title <- palette[9]

    ## Begin construction of chart
    theme_bw(base_size = 9) +

    ## Set the entire chart region to a light gray color
    theme(panel.background = element_rect(fill = color.background,
                                          color = color.background)) +
    theme(plot.background = element_rect(fill = color.background,
                                         color = color.background)) +
    theme(panel.border = element_rect(color=color.background)) +

    ## Format the grid
    theme(panel.grid.major = element_line(color = color.grid.major,
                                          size = .25)) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.ticks = element_blank()) +

    ## Format the legend, but hide by default
    theme(legend.position = "none") +
    theme(legend.background = element_rect(fill = "transparent")) +
    theme(legend.text = element_text(family = "Helvetica-Narrow",
                                     face = "plain",
                                     size = 12,
                                     color = color.axis.title)) +

    ## Set title and axis labels, and format these and tick marks
    theme(plot.title = element_text(family = "Helvetica-Narrow",
                                    face = "plain",
                                    size = 10,
                                    color = color.title,
                                    vjust = 1.25)) +
    theme(axis.text.x = element_text(family = "Helvetica-Narrow",
                                     face = "plain",
                                     size = 12,
                                     color = color.axis.text,
                                     angle = 45)) +
    theme(axis.text.y = element_text(family = "Helvetica-Narrow",
                                     face = "plain",
                                     size = 12,
                                     color = color.axis.text)) +
    theme(axis.title.x = element_text(family = "Helvetica-Narrow",
                                      face = "plain",
                                      size = 14,
                                      color = color.axis.title,
                                      vjust = 0)) +
    theme(axis.title.y = element_text(family = "Helvetica-Narrow",
                                      face = "plain",
                                      size = 14,
                                      color = color.axis.title,
                                      vjust = 1.25)) +

    ## Set the outline of the legend plot characters to be none
    theme(legend.key = element_blank()) +

    ## Plot margins
    theme(plot.margin = unit(c(0.1, 0.35, 0.25, 0.35), "cm"))
}
