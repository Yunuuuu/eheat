#' Fit ggplot2 panel in a viewport
#' @param gg A [ggplot2][ggplot2::ggplot] object.
#' @param vp A [viewport][grid::viewport] object.
#' @param sides Which side to draw besides the panel elements. If `NULL`, will
#' draw panel only.
#' @param elements Ggplot elements to draw, can be a list of a character to
#' specify elements for each side separately. Valid elements are "axis", "lab",
#' "guide". Other elements will be ignored.
#' @param gt A [gtable][ggplot2::ggplotGrob] object.
#' @return Draw ggplot object by fitting exactly the panel to a viewport.
#' @examples
#' p <- ggplot(data.frame(x = 0:10, y = 0:10), aes(x, y)) +
#'     geom_point()
#' grid.newpage()
#' outerBox <- viewport(width = unit(125, "mm"), height = unit(150, "mm"))
#' pushViewport(outerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA))
#' innerBox <- viewport(
#'     x = unit(0.5, "npc"), y = unit(0.6, "npc"),
#'     width = unit(60, "mm"), height = unit(70, "mm"), angle = -30
#' )
#' pushViewport(innerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA, lwd = 2))
#' ggfit_panel(p)
#' @export
ggfit_panel <- function(gg, vp = NULL,
                        sides = c("b", "t", "l", "r"),
                        elements = c("axis", "lab", "guide"),
                        gt = NULL) {
    # gt <- egg::set_panel_size(
    #     gg, width = unit(1, "npc"), height = unit(1, "npc")
    # )
    if (is.null(gt)) {
        stopifnot(ggplot2::is.ggplot(gg))
        gt <- ggplot2::ggplotGrob(gg)
    } else {
        stopifnot(gtable::is.gtable(gt))
    }
    stopifnot(all(sides %in% c("b", "t", "l", "r")))
    if (!is.character(elements) && !is.list(elements)) {
        stop("elements must be a character or a list of character")
    }
    fit_panel(gt, vp, sides, elements)
}

# source by Sandy Muspratt from: https://stackoverflow.com/questions/29535760/fit-ggplot-exactly-to-viewport-size
fit_panel <- function(gt, vp = NULL,
                      sides = c("b", "t", "l", "r"),
                      elements = c("axis", "lab", "guide")) {
    if (is.null(vp)) vp <- grid::viewport()
    grid::pushViewport(vp)
    grid::grid.draw(gtable::gtable_filter(gt, "panel"))
    grid::popViewport()
    if (!length(sides)) return(NULL) # styler: off
    if (is.character(elements)) elements <- list(elements)
    elements <- rep_len(elements, length(sides))
    for (i in seq_along(sides)) {
        s <- .subset(sides, i)
        side_element <- .subset2(elements, i)
        if (any(side_element == "lab")) {
            lab <- paste("lab", s, sep = "-")
        } else {
            lab <- NULL
        }
        if (any(side_element == "axis")) {
            axis <- paste("axis", s, sep = "-")
        } else {
            axis <- NULL
        }
        if (any(side_element == "guide")) {
            guide <- paste("guide-box", switch(s,
                l = "left",
                r = "right",
                b = "bottom",
                t = "top"
            ), sep = "-")
        } else {
            guide <- NULL
        }
        pattern <- paste(c(lab, axis, guide), collapse = "|")
        if (!length(pattern)) next
        gt_elements <- gtable::gtable_filter(gt, pattern)
        if (!length(gt_elements)) next
        w <- switch(s,
            l = ,
            r = grid::convertX(gtable::gtable_width(gt_elements), "mm"),
            b = ,
            t = grid::unit(1, "npc")
        )
        h <- switch(s,
            l = ,
            r = grid::unit(1, "npc"),
            b = ,
            t = grid::convertY(gtable::gtable_height(gt_elements), "mm")
        )
        x <- switch(s,
            l = grid::unit(0, "npc") - .5 * w,
            r = grid::unit(1, "npc") + .5 * w,
            b = ,
            t = grid::unit(0.5, "npc")
        )
        y <- switch(s,
            l = ,
            r = grid::unit(0.5, "npc"),
            b = grid::unit(0, "npc") - .5 * h,
            t = grid::unit(1, "npc") + .5 * h
        )
        grid::pushViewport(grid::viewport(
            x = x,
            y = y,
            width = w,
            height = h
        ))
        grid::grid.draw(gt_elements)
        grid::popViewport()
    }
}
