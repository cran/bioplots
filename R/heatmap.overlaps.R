## roxygen2::roxygenise()
#' Plot heatmat
#' 
#' Plot heatmat to visualize multiple Venn diagram.
#' 
#' @import RColorBrewer
#' @import gplots
#' @import reshape2
#' @import ggplot2
#' 
#' @export
#' 
#' @param x A matrix object or a list object with two-levels.
#' @param legend A legend for colors.
#' @param xlab A label for the x axis.
#' @param ylab A label for the y axis.
#' @param col A character vector of colors. Default is Spectral color
#'            palette in RColorBrewer package.
#' @param lim A range of values.
#' @param x.axis.angle A angle for labels in x axis. Default is 90 degrees.
#' 
#' @return ggplot class object
#' 
#' @examples
#' mat <- matrix(rnbinom(135, size = 10, prob = 0.2), nrow = 15, ncol = 9)
#' colnames(mat) <- paste0("Treat", 1:9)
#' rownames(mat) <- c("a", "b", "c", "d", "ab", "ac", "ad", "bc", "bd", "cd",
#'                    "abc", "abd", "acd", "bcd", "abcd")
#' head(mat)
#' 
#' heatmap.overlaps(mat)
#' 
#' 
heatmap.overlaps <- function(x, legend = NA, xlab = NA, ylab = NA, col = NA, lim = NA, x.axis.angle = 90 ) {
    
    if (is.na(xlab)) xlab <- "condition"
    if (is.na(ylab)) ylab <- "pattern"
    if (is.na(legend)) legend <- "overlaps"
    if (is.na(col))  col <-  rev(brewer.pal(9, "Spectral"))
    
    obj <- NULL
    
    x.class <- class(x)
    if (x.class == "list") {
        mat <- .heatmap.overlaps.list(x)
        obj <- .heatmap.overlaps.matrix(mat, legend, xlab, ylab, col, lim, x.axis.angle)
    }
    if (x.class == "matrix") {
        obj <- .heatmap.overlaps.matrix(x, legend, xlab, ylab, col, lim, x.axis.angle)
    }
    
    obj
}




.heatmap.overlaps.list <- function(x) {
    nm <- NULL
    for (x.i in x) {
        if (is.null(nm)) {
            nm <- names(x.i)
        } else {
            if (length(setdiff(nm, names(x.i))) > 0) {
                stop("Names of the second level lists are not equal.")
            }
        }
    }
    
    mat <- matrix(0, ncol = length(x), nrow = 2 ^ length(nm) - 1)
    colnames(mat) <- names(x)
    rownames(mat) <- NULL
    
    for (x.i in names(x)) {
        .venndat <- venn(x[[x.i]], show.plot = FALSE)
        x.i.combn <- names(attr(.venndat, "intersections"))
        if (is.null(rownames(mat))) rownames(mat) <- x.i.combn
        for (ptn in names(attr(.venndat, "intersections"))) {
            mat[ptn, x.i] <- length(attr(.venndat, "intersections")[[ptn]])
        }
    }
    
    mat
}



.heatmap.overlaps.matrix <- function(x, legend = NA, xlab = NA, ylab = NA, col = NA, lim = NA, x.axis.angle = 90 ) {
    x.melted <- melt(x)
    colnames(x.melted) <- c("pattern", "condition", "value")
    
    condition <- pattern <- value <- NULL
    rm(condition, pattern, value)
    
    gmat <- ggplot(x.melted, aes(x = condition, y = pattern, fill = value))
    gmat <- gmat + geom_tile()
    gmat <- gmat + theme_bw()
    gmat <- gmat + theme(plot.background = element_blank(),
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         axis.line = element_blank(),
                         axis.ticks = element_blank(),
                         strip.background = element_rect(fill = "white", colour = "white"),
                         axis.text.x = element_text(angle = x.axis.angle, vjust = 0.5, hjust = 0.5))
    gmat <- gmat + scale_fill_gradientn(legend, colours = rev(brewer.pal(9, "Spectral")), na.value = "white")
    gmat <- gmat + xlab(xlab) + ylab(ylab)
    gmat <- gmat + coord_fixed(ratio = 1)
    gmat <- gmat + coord_fixed(ratio = 1)
    gmat
}






