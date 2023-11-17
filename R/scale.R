scale_label_identity <- function(..., guide = "none") {
    sc <- ggplot2::discrete_scale("label", "identity", force, ...,
        guide = guide, super = ggplot2::ScaleDiscreteIdentity
    )
    sc
}
