gt_subset <- function(gt, index, trim = TRUE) {
    gt$layout <- gt$layout[index, , drop = FALSE]
    gt$grobs <- .subset(gt$grobs, index)
    if (trim) gt <- gtable::gtable_trim(gt)
    gt
}

gt_border <- function(layout, margin) {
    switch(margin,
        t = min(.subset2(layout, "t")),
        l = min(.subset2(layout, "l")),
        b = max(.subset2(layout, "b")),
        r = max(.subset2(layout, "r"))
    )
}

gt_match_margin <- function(gt, border, margin) {
    layout <- .subset2(gt, "layout")
    if (margin == "t") {
        .subset2(layout, "b") < border
    } else if (margin == "l") {
        .subset2(layout, "r") < border
    } else if (margin == "b") {
        .subset2(layout, "t") > border
    } else {
        .subset2(layout, "l") > border
    }
}

gt_margin <- function(gt, border, margin, trim = TRUE) {
    matches <- gt_match_margin(gt, border, margin)
    gt_subset(gt, matches, trim = trim)
}

# including both center (matched by pattern) and margins
gt_area <- function(gt, pattern, margins = NULL, fixed = FALSE, trim = TRUE) {
    layout <- .subset2(gt, "layout")
    matches <- grepl(
        pattern, .subset2(layout, "name"),
        fixed = fixed, perl = !fixed
    )
    center_layout <- .subset2(gt_subset(gt, matches, trim = FALSE), "layout")
    for (m in margins) {
        border <- gt_border(center_layout, m)
        matches <- matches | gt_match_margin(gt, border, m)
    }
    gt_subset(gt, matches, trim = trim)
}

gt_ggpatterns <- function(margins, elements) {
    if (any(elements == "guide")) {
        guide <- paste("guide-box", parse_margins(margins), sep = "-")
    } else {
        guide <- NULL
    }
    margins <- setdiff(margins, "i") # no inside elements for `lab` and `axis`
    if (length(margins) && any(elements == "lab")) {
        lab <- paste("lab", margins, sep = "-")
    } else {
        lab <- NULL
    }
    if (length(margins) && any(elements == "axis")) {
        axis <- paste("axis", margins, sep = "-")
    } else {
        axis <- NULL
    }
    c(lab, axis, guide, intersect(elements, c("subtitle", "title", "caption")))
}

parse_margins <- function(margins) {
    margin_to_direction <- c(
        l = "left",
        r = "right",
        b = "bottom",
        t = "top",
        i = "inside"
    )
    margin_to_direction[margins]
}

MARGINS <- c("t", "l", "b", "r")
GG_ELEMENTS <- c("axis", "lab", "guide", "subtitle", "title", "caption")

gt_bind <- function(fn, ...) {
    dots <- list(...)
    dots <- dots[lengths(dots) > 0L]
    if (length(dots) > 1) {
        do.call(fn, dots)
    } else if (length(dots) == 1L) {
        dots[[1L]]
    } else {
        gtable::gtable()
    }
}

gt_unify_panel <- function(gt, width, height) {
    panels <- grep("panel", gt$layout$name)
    gt_set_size(gt, width, height, panels)
}

gt_set_size <- function(gt, width, height, index = NULL) {
    index_w <- unique(gt$layout$l[index])
    index_h <- unique(gt$layout$t[index])
    nw <- length(index_w)
    nh <- length(index_h)
    gt$widths[index_w] <- rep(width, nw)
    gt$heights[index_h] <- rep(height, nh)
    gt
}

# gtable_split_by_panel <- function(gt) {
#     gt <- gt_subset(
#         gt, !grepl("background", .subset2(gt$layout, "name"))
#     )
#     matches <- grepl("panel", .subset2(gt$layout, "name"))
#     panels <- gt_subset(gt, matches)
#     if (length(panels) == 0L) {
#         cli::cli_abort("No {.field panels} found")
#     } else if (length(panels) == 1L) {
#         return(gt)
#     }
#     p_layout <- panels$layout
#     p_nms <- .subset2(p_layout, "name")
#     others <- gt_subset(gt, !matches)
#     o_layout <- others$layout
#     lapply(p_nms, function(nm) {
#         current_p_layout <- p_layout[p_layout$name == nm, , drop = FALSE]
#         other_p_layout <- p_layout[
#             p_layout$name %in% setdiff(p_nms, nm), ,
#             drop = FALSE
#         ]
#         #
#         if (all(current_p_layout$t <= other_p_layout$t)) {
#             index <- o_layout$b >= current_p_layout$t
#         } else {

#         }
#     })
# }
