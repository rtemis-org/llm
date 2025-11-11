# Tool.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% ToolParameter Class ----
#' @title ToolParameter
#'
#' @description
#' Class for a single tool parameter schema
#'
#' @field name Character: The name of the parameter.
#' @field type Character: The type of the parameter.
#' @field description Character: The description of the parameter.
#' @field required Logical: Whether the parameter is required.
#'
#' @author EDG
#' @noRd
ToolParameter <- new_class(
  "ToolParameter",
  properties = list(
    name = class_character,
    type = class_character,
    description = class_character,
    required = class_logical
  )
) # /kaimana::ToolParameter


# %% tool_param() ----
#' tool_param
#'
#' Define a tool parameter schema
#'
#' @param name Character: The name of the parameter.
#' @param type Character: The type of the parameter.
#' @param description Character: The description of the parameter.
#' @param required Logical: Whether the parameter is required.
#'
#' @return ToolParameter object
#' @author EDG
#' @export
tool_param <- function(
  name,
  type,
  description,
  required = FALSE
) {
  ToolParameter(
    name = name,
    type = type,
    description = description,
    required = required
  )
} # /kaimana::tool_param


# %% Tool Class ----
#' @title Tool
#'
#' @description
#' Class for a tool that can be used by an agent
#'
#' @field name Character: The name of the tool.
#' @field function_name Character: The name of the function to call.
#' @field description Character: The description of the tool.
#' @field parameters List of ToolParameter: The parameters of the tool.
#'
#' @author EDG
#' @noRd
Tool <- new_class(
  "Tool",
  properties = list(
    name = class_character,
    function_name = class_character,
    description = class_character,
    parameters = class_list
  ),
  validator = function(self) {
    for (param in self@parameters) {
      if (!S7_inherits(param, ToolParameter)) {
        cli::cli_abort(
          "All elements of 'parameters' must be ToolParameter objects."
        )
      }
    }
  }
) # /kaimana::Tool


# %% create_tool() ----
#' create_tool
#'
#' Define a tool for an agent
#'
#' @param name Character: The name of the tool, e.g. "Wikipedia Search".
#' @param function_name Character: The name of the function to call, e.g. "query_wikipedia".
#' @param description Character: The description of the tool.
#' @param parameters List of `ToolParameter`: The parameters of the tool, each  defined using
#' [tool_param].
#'
#' @return Tool object
#'
#' @author EDG
#' @export
create_tool <- function(
  name,
  function_name,
  description,
  parameters = list()
) {
  Tool(
    name = name,
    function_name = function_name,
    description = description,
    parameters = parameters
  )
} # /kaimana::tool


# %% as_list.Tool ----
#' Convert Tool object to named R list
#'
#' Prepare `Tool` definition for use in `httr2` API call
#'
#' @param x `Tool` object.
#'
#' @return list
#'
#' @author EDG
#' @noRd

method(as_list, Tool) <- function(x) {
  list(
    type = "function",
    `function` = list(
      name = x@name,
      description = x@description,
      parameters = list(
        type = "object",
        required = lapply(
          Filter(function(p) p@required, x@parameters),
          function(p) p@name
        ),
        properties = structure(
          lapply(
            x@parameters,
            function(p) {
              list(
                type = p@type,
                description = p@description
              )
            }
          ),
          names = sapply(x@parameters, function(p) p@name)
        ) # /properties
      ) # /parameters
    ) # /function
  )
} # /kaimana::as_list.Tool
