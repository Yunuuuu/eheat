#' Fit ggplot2 in a viewport
#' @inheritParams ggfit
#' @return Draw ggplot object in `vp`.
#' @examples
#' p <- ggplot(data.frame(x = 0:10, y = 0:10), aes(x, y)) +
#'     geom_point()
#' outerBox <- viewport(width = unit(125, "mm"), height = unit(150, "mm"))
#' innerBox <- viewport(
#'     x = unit(0.5, "npc"), y = unit(0.6, "npc"),
#'     width = unit(60, "mm"), height = unit(70, "mm"), angle = -30
#' )
#'
#' grid.newpage()
#' pushViewport(outerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA))
#'
#' pushViewport(innerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA, lwd = 2))
#' ggfit_plot(p)
#' @export
ggfit_plot <- function(gg, vp = NULL,
                       sides = c("b", "t", "l", "r"),
                       elements = c("axis", "lab", "guide"),
                       gt = NULL) {
    if (is.null(gt)) {
        stopifnot(ggplot2::is.ggplot(gg))
        gt <- ggplot2::ggplotGrob(gg)
    } else {
        stopifnot(gtable::is.gtable(gt))
    }
    if (!is.null(vp)) stopifnot(inherits(vp, "viewport"))
    stopifnot(all(sides %in% c("b", "t", "l", "r")))
    if (!is.character(elements) && !is.list(elements)) {
        stop("elements must be a character or a list of character")
    }
    .ggfit_plot(gt, vp, sides, elements)
}

.ggfit_plot <- function(gt, vp = NULL,
                        sides = c("b", "t", "l", "r"),
                        elements = c("axis", "lab", "guide")) {
    if (is.null(vp)) vp <- grid::viewport()
    patterns <- c("panel")
    if (is.character(elements)) elements <- list(elements)
    elements <- rep_len(elements, length(sides))
    for (i in seq_along(sides)) {
        s <- .subset(sides, i)
        side_element <- .subset2(elements, i)
        patterns <- c(patterns, ggpatterns(s, side_element))
    }
    draw_grob(vp, gtable::gtable_filter(gt, paste(patterns, collapse = "|")))
}

ggpatterns <- function(side, element) {
    if (any(element == "lab")) {
        lab <- paste("lab", side, sep = "-")
    } else {
        lab <- NULL
    }
    if (any(element == "axis")) {
        axis <- paste("axis", side, sep = "-")
    } else {
        axis <- NULL
    }
    if (any(element == "guide")) {
        guide <- paste("guide-box", switch(side,
            l = "left",
            r = "right",
            b = "bottom",
            t = "top"
        ), sep = "-")
    } else {
        guide <- NULL
    }
    c(lab, axis, guide)
}
