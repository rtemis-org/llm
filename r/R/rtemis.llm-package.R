# rtemis.llm-package.R
# ::rtemis.llm::
# 2025- EDG rtemis.org

#' \pkg{rtemis.llm}: Agentic AI for the rtemis ecosystem
#'
#' @description
#' rtemis.llm provides functionality to interface with Large Language Models, part of the
#' [rtemis](https://www.rtemis.org) ecosystem. It provides an `Agent` class with support for
#' reasoning, structured output, memory management, and tool use. Allows creation of custom
#' LLM-based workflows and agentic AI systems using a functional user-facing frontend and an S7
#' backend. Includes `llmapply()` for quick batch LLM inference. Supports Ollama, OpenAI, and
#' Anthropic endpoints.
#'
#' @section Online Documentation and Vignettes:
#' <https://www.rtemis.org>
#'
#' @name rtemis.llm-package
#' @import rtemis.core data.table S7
#' @importFrom utils sessionInfo
#' @importFrom stats setNames
#' @importFrom utils View
"_PACKAGE"
NULL
