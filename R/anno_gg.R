anno_gg <- function() {

}

insert_gg <- function(gg, ..., arg = rlang::caller_arg(gg), call = rlang::caller_env()) {
    assert_s3_class(gg, "ggplot", arg = arg, call = call)
    new_anno(
        matrix = matrix,
        draw_fn = function(index, k, n) {
            vp <- flip_viewport(which,
                xscale = c(0.5, n + 0.5),
                yscale = yscale
            )
            matrix <- matrix[index, , drop = FALSE]
            rlang::inject(draw_fn(matrix, !!!dots, which = which, vp = vp))
        },
        yscale = yscale,
        subset_rule = subset_rule, subsettable = subsettable,
        which = which, width = width, height = height,
        show_name = show_name, name = "anno_fn"
    )
}

guide_from_gg <- function(gg, direction = NULL) {
    gt <- ggplot2::ggplot_gtable(gg)
    guide <- gtable::gtable_filter(gt, "guide")
    grob <- grid::grobTree(guide)
    attr(object@grob, "width") <- sum(guide$widths)
    attr(object@grob, "height") <- sum(guide$heights)
    methods::new(
        "Legends",
        grob = grob,
        type = "gg_legend",
        name = "gg",
        n = 1L, multiple = 1L,
        direction = match.arg(direction, c("vertical", "horizontal"))
    )
}

# ComplexHeatmap::Legend()
# legend <- function() {
#   object <- new("Legends")
#   object@grob <- legend_body
#   object@type <- "single_legend_no_title"
#   object@n <- 1
#   object@multiple <- 1
#   object@direction <- "vertical"
#   return(object)
#   object <- new("Legends")
#   object@grob <- legend_body
#   object@type <- "single_legend_no_title"
#   object@n <- 1
#   object@multiple <- 1
#   object@direction <- "vertical"
#   return(object)
#   object <- new("Legends")
#   object@grob <- gf
#   object@type <- "single_legend"
#   object@name <- name
#   object@n <- 1
#   object@multiple <- 1
#   object@direction <- "vertical"
# }
