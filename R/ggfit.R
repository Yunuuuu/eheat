#' Fit ggplot2 panel or plot in a viewport
#'
#' @param gg A [ggplot2][ggplot2::ggplot] object.
#' @param align_with Which area of the object should be fit to the viewport?
#' Must be a string of "panel", "plot", "full".
#' @param clip One of `"on"`, `"inherit"`, or `"off"`, indicating whether to
#' clip to the extent out of the viewport, inherit the clipping region from
#' `vp`, or turn clipping off altogether. A logical value of `TRUE` corresponds
#' to `"on"` and `FALSE` corresponds to `"off"`. Default: `"inherit"`.
#' @param margins Which margin to draw besides the plot in the `vp` viewport.
#' This allows for a more precise adjustment of the `clip` feature. Allowed
#' values are `r rd_elements(MARGINS)`, When set to `NULL`, it means clip =
#' `"off"`.
#' @param elements Ggplot elements to draw, can be a list of a character to
#' specify elements for each side separately. Valid elements are
#' `r rd_elements(GG_ELEMENTS)`. Invalid elements will be just omitted. IF
#' `NULL`, all valid ggplot elements will be used.
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
    if (is.null(gt)) {
        assert_s3_class(gg, "ggplot")
        gt <- ggplot2::ggplotGrob(gg)
    } else {
        assert_s3_class(gt, "gtable")
    }
    align_with <- match.arg(align_with, c("panel", "plot", "full"))
    assert_clip(clip)
    assert_s3_class(vp, "viewport", null_ok = TRUE)
    # we need a valid viewport to decide the `clip` argument
    vp <- vp %||% grid::viewport(clip = grid::current.viewport()$clip)
    margins <- setup_margins(clip, vp)
    .ggfit(gt, align_with, margins, vp = vp)
}

setup_margins <- function(clip, vp) {
    if (is.null(clip) || identical(clip, "inherit")) clip <- vp$clip
    if (isTRUE(clip) || identical(clip, "on")) {
        NULL
    } else {
        MARGINS
    }
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
                   elements = NULL,
                   vp = NULL, gt = NULL) {
    if (is.null(gt)) {
        assert_s3_class(gg, "ggplot")
        gt <- ggplot2::ggplotGrob(gg)
    } else {
        assert_s3_class(gt, "gtable")
    }
    align_with <- match.arg(align_with, c("panel", "plot", "full"))
    assert_margins(margins)
    margins <- unique(margins)
    elements <- setup_elements(elements, align_with, margins)
    assert_s3_class(vp, "viewport", null_ok = TRUE)
    .ggfit(gt, align_with, margins, elements, vp)
}

.ggfit <- function(gt, align_with, margins = MARGINS,
                   elements = GG_ELEMENTS, vp = NULL) {
    vp <- vp %||% grid::viewport(clip = grid::current.viewport()$clip)
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
    # used by internal function
    # for user input: we always use setup_elements to ensure the input is well
    if (is.character(elements)) {
        elements <- rep_len(list(elements), length(margins))
        names(elements) <- margins
    }
    for (m in margins) {
        border <- gt_border(center_layout, m)
        margin <- gt_margin(gt, border, m)
        if (!length(margin)) next # If no margin
        # Only draw elements user specified, and omit invalid elements
        if (is.null(e <- .subset2(elements, m))) {
            e <- GG_ELEMENTS
        } else {
            e <- intersect(e, GG_ELEMENTS)
        }
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

setup_elements <- function(elements, align_with, margins,
                           arg = rlang::caller_arg(elements),
                           call = rlang::caller_env()) {
    if (align_with == "full") return(elements) # styler: off
    if (length(margins) == 0L) return(elements) # styler: off
    if (is.character(elements)) {
        elements <- rep_len(list(elements), length(margins))
        names(elements) <- margins
    } else if (is.list(elements)) {
        if (rlang::is_named(elements)) {
            missing <- setdiff(rlang::names2(elements), margins)
            if (length(missing)) {
                cli::cli_warn("Unused element{?s}: {missing}", call = call)
            }
        } else if (length(elements) == length(margins)) {
            names(elements) <- margins
        } else {
            cli::cli_abort(sprintf(
                "unnamed {.arg {arg}} must have the same length of %s (%d)",
                style_arg("margins"), style_val(length(margins))
            ), call = call)
        }
    } else if (is.null(elements)) {
        elements <- rep_len(list(GG_ELEMENTS), length(margins))
        names(elements) <- margins
    } else {
        cli::cli_abort(
            "{.arg {arg}} must be a character or a list of character",
            call = call
        )
    }
    elements
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
            t = "top",
            i = "inside"
        ), sep = "-")
    } else {
        guide <- NULL
    }
    c(lab, axis, guide, intersect(element, c("subtitle", "title", "caption")))
}

MARGINS <- c("t", "l", "b", "r")
GG_ELEMENTS <- c("axis", "lab", "guide", "subtitle", "title", "caption")
