.onAttach <- function(...) {
    attached <- eheat_attach()
    rlang::inform(eheat_attach_message(attached),
        class = "packageStartupMessage"
    )
}
