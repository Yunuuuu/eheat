check_gp <- function(gp, arg = rlang::caller_arg(gp),
                     call = rlang::caller_env()) {
    assert_s3_class(gp, "gpar", arg = arg, call = call)
}

assert_clip <- function(clip, arg = rlang::caller_arg(clip),
                        call = rlang::caller_env()) {
    if (is_scalar(clip)) {
        if (is.character(clip) &&
            !any(clip == c("on", "off", "inherit"))) {
            stop_input_type(
                clip,
                "a string of \"on\", \"off\", or \"inherit\"",
                arg = arg, call = call
            )
        } else if (is.logical(clip)) {
            if (is.na(clip)) {
                cli::cli_abort("{.arg {arg}} cannot be missing", call = call)
            }
        }
    } else if (!is.null(clip)) {
        stop_input_type(
            clip,
            "a string or bool",
            null_ok = TRUE,
            show_length = TRUE,
            arg = arg, call = call
        )
    }
}

assert_margins <- function(margins, arg = rlang::caller_arg(margins),
                           call = rlang::caller_env()) {
    if (is.character(margins)) {
        missing <- setdiff(margins, MARGINS)
        if (length(missing)) {
            cli::cli_abort(
                "invalid values ({.val {missing}}) in {.arg {arg}}",
                call = call
            )
        }
    } else if (!is.null(margins)) {
        stop_input_type(
            margins,
            "a character",
            null_ok = TRUE,
            arg = arg, call = call
        )
    }
}
