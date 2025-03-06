# ▄ •▄  ▄▄▄· ▪  • ▌ ▄ ·.  ▄▄▄·  ▐ ▄  ▄▄▄· 
# █▌▄▌▪▐█ ▀█ ██ ·██ ▐███▪▐█ ▀█ •█▌▐█▐█ ▀█ 
# ▐▀▀▄·▄█▀▀█ ▐█·▐█ ▌▐▌▐█·▄█▀▀█ ▐█▐▐▌▄█▀▀█ 
# ▐█.█▌▐█ ▪▐▌▐█▌██ ██▌▐█▌▐█ ▪▐▌██▐█▌▐█ ▪▐▌
# ·▀  ▀ ▀  ▀ ▀▀▀▀▀  █▪▀▀▀ ▀  ▀ ▀▀ █▪ ▀  ▀ 

# zzz.R
# ::kaimana::
# 2025 SDG rtemis.org

# kaimana internal environment
live <- new.env()

# vars
kaimana_version <- packageVersion("kaimana")
.availableCores <- unname(future::availableCores())

# References
# Unicode emojis: https://www.unicode.org/emoji/charts/full-emoji-list.html

# .onLoad <- function(libname, pkgname) {
#   # S7
#   S7::methods_register()
# }
.onAttach <- function(libname, pkgname) {
  .startup <- plain(paste0(
    "  .:", bold(pkgname), " v.", kaimana_version, " \U1F30A", " ", sessionInfo()[[2]]
  ))
  if (interactive()) {
    packageStartupMessage(paste0(
      # rtlogo,
      .startup,
      "\n  ", red(bold("PSA:"), "Do not throw data at LLMs. Chat responsibly!")
    ))
  } else {
    packageStartupMessage(
      .startup
    )
  }
}

#' \pkg{kaimana}: Interface to Large Language Models
#'
#' @description
#' Advanced Machine Learning made easy, efficient, reproducible
#'
#' @section Online Documentation and Vignettes:
#' <https://kaimana.rtemis.org>

#'
#' @name kaimana-package
"_PACKAGE"
NULL
