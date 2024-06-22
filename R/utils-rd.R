rd_elements <- function(x, quote = TRUE, code_style = TRUE) {
    if (quote) x <- paste0('"', x, '"')
    if (code_style) x <- paste0("`", x, "`")
    oxford_comma(x)
}
