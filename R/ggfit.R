#' Fit ggplot2 panel or plot in a viewport
#'
#' @param gg A [ggplot2][ggplot2::ggplot] object.
#' @param align_with Which area of the object should be fit to the viewport?
#' Must be a string of "panel", "plot", "full".
#' @param clip One of `"on"`, `"inherit"`, or `"off"`, indicating whether to
#' clip to the extent out of the viewport, inherit the clipping region from
#' `vp`, or turn clipping off altogether. A logical value of `TRUE` corresponds
#' to `"on"` and `FALSE` corresponds to `"off"`. A `NULL` means `"inherit"`.
#' @param margins Which margin to draw besides the plot in the `vp` viewport.
#' This allows for a more precise adjustment of the `clip` feature. Allowed
#' values are `r rd_elements(MARGINS)`, When set to `NULL`, it means clip =
#' `"off"`. 
#' @param elements Ggplot elements to draw, can be a list of a character to
#' specify elements for each side separately. Valid elements are
#' `r rd_elements(GG_ELEMENTS)`.
#' @param vp A [viewport][grid::viewport] object.
#' @param gt A [gtable][ggplot2::ggplotGrob] object.
#' @return Fit ggplot object in the viewport.
#' - align_with = `"panel"`: Draw ggplot object by fitting exactly the `panel`
#'   to `vp`.
#' - align_with = `"plot"`: Draw ggplot object by fitting `panel`, `axis`, and
#'   `lab` in `vp`.
#' - align_with = `"full"`: Draw full ggplot object in `vp`.
#' @examples
#' p <- ggplot(data.frame(x = 0:10, y = 0:10), aes(x, y)) +
#'     geom_point()
#' outerBox <- viewport(width = unit(125, "mm"), height = unit(150, "mm"))
#' innerBox <- viewport(
#'     x = unit(0.5, "npc"), y = unit(0.6, "npc"),
#'     width = unit(60, "mm"), height = unit(70, "mm"), angle = -30
#' )
#'
#' # ggfit-panel: clip = "on" ------------
#' grid.newpage()
#' pushViewport(outerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA))
#'
#' pushViewport(innerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA, lwd = 2))
#' ggfit(p, "panel", clip = "on")
#'
#' # ggfit-panel: clip = "off" ------------
#' grid.newpage()
#' pushViewport(outerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA))
#'
#' pushViewport(innerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA, lwd = 2))
#' ggfit(p, "panel", clip = "off")
#'
#' # ggfit-plot -------------
#' grid.newpage()
#' pushViewport(outerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA))
#'
#' pushViewport(innerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA, lwd = 2))
#' ggfit(p, "plot")
#'
#' # ggfit-full -------------
#' grid.newpage()
#' pushViewport(outerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA))
#'
#' pushViewport(innerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA, lwd = 2))
#' ggfit(p, "full")
#' @export
ggfit <- function(gg, align_with = "full", clip = NULL, vp = NULL, gt = NULL) {
    align_with <- match.arg(align_with, c("panel", "plot", "full"))
    if (is.null(gt)) {
        stopifnot(ggplot2::is.ggplot(gg))
        gt <- ggplot2::ggplotGrob(gg)
    } else {
        stopifnot(gtable::is.gtable(gt))
    }
    if (is.null(vp)) {
        # we need a valid viewport to decide the `clip` argument
        vp <- grid::viewport()
    } else if (!inherits(vp, "viewport")) {
        stop("vp must be a viewport")
    }
    if (is.character(clip)) {
        clip <- match.arg(clip, c("on", "off", "inherit"))
    } else if (is.logical(clip)) {
        if (length(clip) != 1L) {
            stop("clip must be a single bool value")
        }
        if (is.na(clip)) {
            stop("clip cannot be missing")
        }
    } else if (!is.null(clip)) {
        stop("clip must be a string or a boolean value")
    }
    if (is.null(vp)) {
        if (is.null(clip) || identical(clip, "inherit")) clip <- vp$clip
    }
    if (isTRUE(clip) || identical(clip, "on")) {
        margins <- NULL
    } else {
        margins <- MARGINS
    }
    .ggfit(gt, align_with = align_with, margins = margins, vp = vp)
}

#' @examples
#'
#' # ggfit2-panel: margins = NULL ------------
#' grid.newpage()
#' pushViewport(outerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA))
#'
#' pushViewport(innerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA, lwd = 2))
#' ggfit2(p, "panel", margins = NULL)
#'
#' # ggfit2-panel: margins = "b" ------------
#' grid.newpage()
#' pushViewport(outerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA))
#'
#' pushViewport(innerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA, lwd = 2))
#' ggfit2(p, "panel", margins = "b")
#'
#' # ggfit2-panel: margins = "l" ------------
#' grid.newpage()
#' pushViewport(outerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA))
#'
#' pushViewport(innerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA, lwd = 2))
#' ggfit2(p, "panel", margins = "l")
#'
#' # ggfit2-plot -------------
#' grid.newpage()
#' pushViewport(outerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA))
#'
#' pushViewport(innerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA, lwd = 2))
#' ggfit2(p, "plot")
#'
#' # ggfit2-full -------------
#' grid.newpage()
#' pushViewport(outerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA))
#'
#' pushViewport(innerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA, lwd = 2))
#' ggfit2(p, "full")
#' @export
#' @rdname ggfit
ggfit2 <- function(gg, align_with = "full",
                   margins = c("b", "t", "l", "r"),
                   elements = c(
                       "axis", "lab", "guide",
                       "subtitle", "title", "caption"
                   ),
                   vp = NULL, gt = NULL) {
    if (is.null(gt)) {
        stopifnot(ggplot2::is.ggplot(gg))
        gt <- ggplot2::ggplotGrob(gg)
    } else {
        stopifnot(gtable::is.gtable(gt))
    }
    align_with <- match.arg(align_with, c("panel", "plot", "full"))
    if (!is.null(margins)) {
        margins <- unique(as.character(margins))
        if (!all(margins %in% MARGINS)) {
            stop(sprintf(
                "invalid margins provided, only %s are supported",
                oxford_comma(MARGINS)
            ))
        }
    }
    if (!is.character(elements) && !is.list(elements)) {
        stop("elements must be a character or a list of character")
    }
    if (!is.null(vp) && !inherits(vp, "viewport")) {
        stop("vp must be a viewport")
    }
    .ggfit(gt, align_with, margins, elements, vp)
}

.ggfit <- function(gt, align_with, margins = MARGINS,
                   elements = GG_ELEMENTS, vp = NULL) {
    if (is.null(vp)) vp <- grid::viewport()
    grid::pushViewport(vp)
    on.exit(grid::popViewport())
    if (align_with == "full") {
        grid::grid.draw(gt)
        return(invisible(NULL))
    } else if (align_with == "plot") {
        pattern <- "panel"
        for (s in MARGINS) {
            pattern <- c(pattern, ggpatterns(s, c("axis", "lab")))
        }
        pattern <- paste(pattern, collapse = "|")
    } else {
        pattern <- "panel"
    }
    center <- gtable::gtable_filter(gt, pattern)

    # draw center plot -----------------------------
    grid::grid.draw(center)

    # add margins around the center plot -----------
    if (!length(margins)) return(invisible(NULL)) # styler: off
    center_layout <- gtable::gtable_filter(gt, pattern, trim = FALSE)$layout
    if (is.character(elements)) elements <- list(elements)
    elements <- rep_len(elements, length(margins))
    for (i in seq_along(margins)) {
        m <- .subset2(margins, i)
        border <- gt_border(center_layout, m)
        margin <- gt_margin(gt, border, m)
        if (!length(margin)) next # If no margin
        # Only draw elements user specified, and omit invalid elements
        e <- intersect(.subset2(elements, i), GG_ELEMENTS)
        margin <- gtable::gtable_filter(margin, paste(e, collapse = "|"))
        if (!length(margin)) next # If no margin

        w <- switch(m,
            l = ,
            r = grid::convertX(gtable::gtable_width(margin), "mm"),
            b = ,
            t = grid::unit(1, "npc")
        )
        h <- switch(m,
            l = ,
            r = grid::unit(1, "npc"),
            b = ,
            t = grid::convertY(gtable::gtable_height(margin), "mm")
        )
        x <- switch(m,
            l = grid::unit(0, "npc") - .5 * w,
            r = grid::unit(1, "npc") + .5 * w,
            b = ,
            t = grid::unit(0.5, "npc")
        )
        y <- switch(m,
            l = ,
            r = grid::unit(0.5, "npc"),
            b = grid::unit(0, "npc") - .5 * h,
            t = grid::unit(1, "npc") + .5 * h
        )
        grid_draw(grid::viewport(x, y, width = w, height = h), margin)
    }
}

ggpatterns <- function(margin, element) {
    if (any(element == "lab")) {
        lab <- paste("lab", margin, sep = "-")
    } else {
        lab <- NULL
    }
    if (any(element == "axis")) {
        axis <- paste("axis", margin, sep = "-")
    } else {
        axis <- NULL
    }
    if (any(element == "guide")) {
        guide <- paste("guide-box", switch(margin,
            l = "left",
            r = "right",
            b = "bottom",
            t = "top"
        ), sep = "-")
    } else {
        guide <- NULL
    }
    c(lab, axis, guide, intersect(element, c("subtitle", "title", "caption")))
}

MARGINS <- c("t", "l", "b", "r")
GG_ELEMENTS <- c("axis", "lab", "guide", "subtitle", "title", "caption")
