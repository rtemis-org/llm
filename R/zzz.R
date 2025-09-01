# ▄ •▄  ▄▄▄· ▪  • ▌ ▄ ·.  ▄▄▄·  ▐ ▄  ▄▄▄·
# █▌▄▌▪▐█ ▀█ ██ ·██ ▐███▪▐█ ▀█ •█▌▐█▐█ ▀█
# ▐▀▀▄·▄█▀▀█ ▐█·▐█ ▌▐▌▐█·▄█▀▀█ ▐█▐▐▌▄█▀▀█
# ▐█.█▌▐█ ▪▐▌▐█▌██ ██▌▐█▌▐█ ▪▐▌██▐█▌▐█ ▪▐▌
# ·▀  ▀ ▀  ▀ ▀▀▀▀▀  █▪▀▀▀ ▀  ▀ ▀▀ █▪ ▀  ▀

# zzz.R
# ::kaimana::
# 2025 EDG rtemis.org

# vars
kaimana_version <- packageVersion("kaimana")
.availableCores <- unname(future::availableCores())

# rtemis internal functions
printls <- getFromNamespace("printls", "rtemis")
printdf <- getFromNamespace("printdf", "rtemis")

# References
# Unicode emojis: https://www.unicode.org/emoji/charts/full-emoji-list.html

.onLoad <- function(libname, pkgname) {
  # S7
  S7::methods_register()
}

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
      kmnlogo,
      "\n",
      .startup,
      "\n  ",
      red(bold("PSA:"), "Do not throw prompts at LLMs. Query responsibly!")
    ))
  } else {
    packageStartupMessage(
      .startup
    )
  }
}
