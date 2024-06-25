#' Make legends object
#'
#' @description Only object with [make_legends] methods can be put in
#' `legends_margin`. Only object with [draw][draw-method] methods can be put in
#' `legends_panel`.
#' @param x A [ggplot2][ggplot2::ggplot] object.
#' @param ... Not used currently.
#' @param margins Guides of which margin to draw besides the plot in the `vp`
#' viewport. Allowed values are `r rd_elements(c(MARGINS, "i"))`. `"i"` means
#' the inside legends (`ggplot2::theme(legend.position = "inside")`). Default:
#' `NULL`, which indicates `c("t", "l", "b", "r")`.
#' @return A list of [Legends][ComplexHeatmap::Legends-class] object.
#' @seealso [ComplexHeatmap::Legend]
#' @examples
#' gg <- ggplot(mtcars) +
#'     geom_bar(
#'         aes(mpg, disp, fill = factor(mpg)),
#'         stat = "identity"
#'     )
#' draw(make_legends(gg)[[1L]])
#' @export
make_legends <- function(x, ...) {
    UseMethod("make_legends")
}

#' @export
#' @rdname make_legends
make_legends.Legends <- function(x, ...) list(x)

#' @export
#' @rdname make_legends
make_legends.grob <- function(x, ...) {
    attr(x, "width") <- grid::grobWidth(x)
    attr(x, "height") <- grid::grobHeight(x)
    list(methods::new(
        "Legends",
        grob = x,
        type = "grob_legend",
        name = "eheat",
        n = 1L, multiple = 1L,
        # how to extract information directly from ggplot2?
        direction = "vertical"
    ))
}

#' @export
#' @rdname make_legends
make_legends.gtable <- function(x, ...) {
    attr(x, "width") <- gtable::gtable_width(x)
    attr(x, "height") <- gtable::gtable_height(x)
    list(methods::new(
        "Legends",
        grob = x,
        type = "gtable_legend",
        name = "eheat",
        n = 1L, multiple = 1L,
        # how to extract information directly from ggplot2?
        direction = "vertical"
    ))
}

#' @export
#' @rdname make_legends
make_legends.ggplot <- function(x, ..., margins = NULL) {
    guides <- get_guides(x, margins = margins)
    lapply(guides, function(g) {
        attr(g, "width") <- gtable::gtable_width(g)
        attr(g, "height") <- gtable::gtable_height(g)
        methods::new(
            "Legends",
            grob = g,
            type = "ggplot_legend",
            name = "eheat",
            n = 1L, multiple = 1L,
            # how to extract information directly from ggplot2?
            direction = "vertical"
        )
    })
}

#' @export
#' @rdname make_legends
make_legends.list <- function(x, ..., margins = NULL) {
    if (length(x) == 0L) return(list()) # styler: off
    unlist(lapply(x, make_legends, margins = margins), FALSE, FALSE)
}

#' Get guide legends as a list of `gtable` object
#' 
#' @param x See method signatures
#' @param ... Not used currently.
#' @inheritParams make_legends
#' @return A list of [gtable][gtable::gtable] objects.
#' @examples 
#' gg <- ggplot(mtcars) +
#'     geom_bar(
#'         aes(mpg, disp, fill = factor(mpg)),
#'         stat = "identity"
#'     )
#' get_guides(gg)
#' @export
#' @rdname get_guides
get_guides <- function(x, ...) UseMethod("get_guides")

#' @export
#' @rdname get_guides
get_guides.default <- function(x, ...) {
    cli::cli_abort("{.arg x} must be a {.cls ggplot} or a {.cls gtable} object")
}

#' @export
#' @rdname get_guides
get_guides.ggplot <- function(x, ...) {
    get_guides(ggplot2::ggplotGrob(x), ...)
}

#' @export
#' @rdname get_guides
get_guides.gtable <- function(x, margins = NULL, ...) {
    margins <- margins %||% MARGINS
    patterns <- gt_ggpatterns(margins, "guide")
    guides <- gtable::gtable_filter(x, paste(patterns, collapse = "|"))$grobs
    # trim zeroGrobs
    guides[!vapply(guides, function(x) {
        grid::is.grob(x) && inherits(x, "zeroGrob")
    }, logical(1L))]
    # if we omit `legend-box-background`, the position will be missed too
    # one `guide-box` can have multiple guides
    # outs <- lapply(guides, function(g) {
    #     gs <- gtable::gtable_filter(g, "guides")
    #     if (!length(gs)) return(NULL) # styler: off
    #     gs <- .subset2(gs, "grobs")
    #     gs[!vapply(gs, function(x) {
    #         grid::is.grob(x) && inherits(x, "zeroGrob")
    #     }, logical(1L))]
    # })
    # out <- unlist(outs, recursive = FALSE, use.names = FALSE)
    # if (is.null(out)) list() else out
}
