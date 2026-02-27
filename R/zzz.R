# ▄ •▄  ▄▄▄· ▪  • ▌ ▄ ·.  ▄▄▄·  ▐ ▄  ▄▄▄·
# █▌▄▌▪▐█ ▀█ ██ ·██ ▐███▪▐█ ▀█ •█▌▐█▐█ ▀█
# ▐▀▀▄·▄█▀▀█ ▐█·▐█ ▌▐▌▐█·▄█▀▀█ ▐█▐▐▌▄█▀▀█
# ▐█.█▌▐█ ▪▐▌▐█▌██ ██▌▐█▌▐█ ▪▐▌██▐█▌▐█ ▪▐▌
# ·▀  ▀ ▀  ▀ ▀▀▀▀▀  █▪▀▀▀ ▀  ▀ ▀▀ █▪ ▀  ▀

# zzz.R
# ::kaimana::
# 2025- EDG rtemis.org

# vars
kaimana_version <- packageVersion("kaimana")
.availableCores <- unname(future::availableCores())

# References
# Unicode emojis: https://www.unicode.org/emoji/charts/full-emoji-list.html

.onLoad <- function(libname, pkgname) {
  # S7
  S7::methods_register()
} # /onLoad

.onAttach <- function(libname, pkgname) {
  .startup <- plain(paste0(
    "  .:",
    bold(pkgname),
    " v.",
    kaimana_version,
    " \U1F30A",
    " ",
    sessionInfo()[[2]]
  ))
  if (interactive()) {
    packageStartupMessage(paste0(
      pkglogo(
        pkg = pkgname,
        args = list(
          color_left = rtemis_colors[["blue"]],
          color_right = rtemis_colors[["light_green"]],
          output_type = "ansi"
        )
      ),
      "\n",
      .startup,
      "\n  ",
      red("PSA: ", bold = TRUE),
      "Do not throw prompts at language models. Generate responsibly!"
    ))
  } else {
    packageStartupMessage(
      .startup
    )
  }
} # /onAttach
