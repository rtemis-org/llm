# ▄ •▄  ▄▄▄· ▪  • ▌ ▄ ·.  ▄▄▄·  ▐ ▄  ▄▄▄·
# █▌▄▌▪▐█ ▀█ ██ ·██ ▐███▪▐█ ▀█ •█▌▐█▐█ ▀█
# ▐▀▀▄·▄█▀▀█ ▐█·▐█ ▌▐▌▐█·▄█▀▀█ ▐█▐▐▌▄█▀▀█
# ▐█.█▌▐█ ▪▐▌▐█▌██ ██▌▐█▌▐█ ▪▐▌██▐█▌▐█ ▪▐▌
# ·▀  ▀ ▀  ▀ ▀▀▀▀▀  █▪▀▀▀ ▀  ▀ ▀▀ █▪ ▀  ▀

# zzz.R
# ::rtemis.llm::
# 2025- EDG rtemis.org

# vars
rtemis.llm_version <- packageVersion("rtemis.llm")

# References
# Unicode emojis: https://www.unicode.org/emoji/charts/full-emoji-list.html

.onLoad <- function(libname, pkgname) {
  # S7
  S7::methods_register()
  # Capture trusted hashes of built-in tool functions at load time
  .warm_tool_hash_cache()
} # /onLoad

.onAttach <- function(libname, pkgname) {
  .startup <- plain(paste0(
    "  .:",
    bold(pkgname),
    " v.",
    rtemis.llm_version,
    " \U1F30A",
    " ",
    sessionInfo()[[2]]
  ))
  if (interactive()) {
    packageStartupMessage(paste0(
      .startup,
      "\n  ",
      fmt("PSA: ", col = rtemis_colors[["red"]], bold = TRUE),
      "Do not throw prompts at language models. Generate responsibly!"
    ))
  } else {
    packageStartupMessage(
      .startup
    )
  }
} # /onAttach
