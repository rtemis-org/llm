# ▄ •▄  ▄▄▄· ▪  • ▌ ▄ ·.  ▄▄▄·  ▐ ▄  ▄▄▄·
# █▌▄▌▪▐█ ▀█ ██ ·██ ▐███▪▐█ ▀█ •█▌▐█▐█ ▀█
# ▐▀▀▄·▄█▀▀█ ▐█·▐█ ▌▐▌▐█·▄█▀▀█ ▐█▐▐▌▄█▀▀█
# ▐█.█▌▐█ ▪▐▌▐█▌██ ██▌▐█▌▐█ ▪▐▌██▐█▌▐█ ▪▐▌
# ·▀  ▀ ▀  ▀ ▀▀▀▀▀  █▪▀▀▀ ▀  ▀ ▀▀ █▪ ▀  ▀

# zzz.R
# ::rtemis.llm::
# 2025- EDG rtemis.org

rtemis.llm_version <- packageVersion("rtemis.llm")

.onLoad <- function(libname, pkgname) {
  # S7
  if ("S7_on_load" %in% getNamespaceExports("S7")) {
    S7::S7_on_load()
  } else {
    S7::methods_register()
  }
  # Capture trusted hashes of built-in tool functions at load time
  .warm_tool_hash_cache()
} # /onLoad

.onAttach <- function(libname, pkgname) {
  .startup <- plain(paste0(
    "  .:",
    bold(pkgname),
    " v.",
    rtemis.llm_version,
    " \Ub7 ",
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

.onUnload <- function(libpath) {
  if ("S7_on_unload" %in% getNamespaceExports("S7")) {
    S7::S7_on_unload()
  }
}

if ("S7_on_build" %in% getNamespaceExports("S7")) {
  S7::S7_on_build()
}
