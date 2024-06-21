#' Fit ggplot2 panel or plot in a viewport
#'
#' @param gg A [ggplot2][ggplot2::ggplot] object.
#' @param align A string indicates how to align the `viewport`. Must be "panel"
#' or "plot".
#' @param vp A [viewport][grid::viewport] object.
#' @param sides Which side to draw besides the panel elements. If `NULL`, will
#' draw panel only.
#' @param elements Ggplot elements to draw, can be a list of a character to
#' specify elements for each side separately. Valid elements are "axis", "lab",
#' "guide". Other elements will be ignored.
#' @param background A boolean value indicates whether to draw the background.
#' @param gt A [gtable][ggplot2::ggplotGrob] object.
#' @return Draw ggplot object.
#' - align = `"panel"`: Draw ggplot object by fitting exactly the panel to `vp`.
#' - align = `"plot"`: Draw ggplot object in `vp`.
#' @examples
#' p <- ggplot(data.frame(x = 0:10, y = 0:10), aes(x, y)) +
#'     geom_point()
#' outerBox <- viewport(width = unit(125, "mm"), height = unit(150, "mm"))
#' innerBox <- viewport(
#'     x = unit(0.5, "npc"), y = unit(0.6, "npc"),
#'     width = unit(60, "mm"), height = unit(70, "mm"), angle = -30
#' )
#'
#' # ggfit_panel ------------
#' grid.newpage()
#' pushViewport(outerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA))
#'
#' pushViewport(innerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA, lwd = 2))
#' ggfit(p, "panel")
#'
#' # ggfit_plot -------------
#' grid.newpage()
#' pushViewport(outerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA))
#'
#' pushViewport(innerBox)
#' grid.rect(gp = gpar(col = "red", fill = NA, lwd = 2))
#' ggfit(p, "plot")
#' @seealso
#' - [ggfit_panel]
#' - [ggfit_plot]
#' @export
ggfit <- function(gg, align = "panel", vp = NULL,
                  sides = c("b", "t", "l", "r"),
                  elements = c("axis", "lab", "guide"),
                  background = FALSE,
                  gt = NULL) {
    align <- match.arg(align, c("panel", "plot"))
    if (align == "panel") {
        ggfit_panel(gg, vp, sides, elements, gt = gt)
    } else {
        ggfit_plot(gg, vp, sides, elements, background, gt = gt)
    }
}
