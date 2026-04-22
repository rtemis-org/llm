# %% Schema constants and helpers ----
#' JSON Schema Field Types
.SCHEMA_FIELD_TYPES <- c(
  "string",
  "number",
  "integer",
  "boolean",
  "array",
  "object"
)


# %% Field ----
#' @title Field Class
#'
#' @description
#' Internal S7 class for one JSON Schema property.
#'
#' @field name Optional Character: Field name.
#' @field type Character \{"string", "number", "integer", "boolean", "array", "object"\}: JSON
#' Schema type.
#' @field description Optional Character: Field description.
#' @field required Logical: Whether the field is required by its parent schema.
#'
#' @author EDG
#' @keywords internal
#' @noRd
Field <- S7::new_class(
  "Field",
  properties = list(
    name = S7::class_character,
    type = S7::class_character,
    description = optional(S7::class_character),
    required = S7::class_logical
  ),
  validator = function(self) {
    check_optional_scalar_character(self@name, "name")
    if (!nzchar(self@name)) {
      cli::cli_abort("Field name cannot be empty.")
    }
    check_optional_scalar_character(self@description, "description")
    check_character(self@type, allow_null = FALSE, arg_name = "type")
    if (length(self@type) != 1L) {
      cli::cli_abort("{.var type} must be a single JSON Schema type.")
    }
    check_enum(self@type, .SCHEMA_FIELD_TYPES, arg_name = "type")
    check_scalar_logical(self@required, "required")
    NULL
  }
)


# %% repr.Field ----
method(repr, Field) <- function(x, pad = 0L, output_type = NULL) {
  repr_ls(
    setNames(
      list(
        list(
          type = x@type,
          description = x@description,
          required = x@required
        )
      ),
      x@name
    ),
    pad = pad,
    output_type = output_type
  )
}


# %% print.Field ----
method(print, Field) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
  invisible(x)
}


# %% Schema ----
#' @title Schema Class
#'
#' @description
#' Internal S7 class for a JSON Schema object used for structured LLM output.
#'
#' @field name Optional Character: Schema name.
#' @field type Character {"object"}: JSON Schema type.
#' @field description Optional Character: Schema description.
#' @field fields List: Field objects defining object properties.
#'
#' @author EDG
#' @keywords internal
#' @noRd
Schema <- S7::new_class(
  "Schema",
  properties = list(
    name = optional(S7::class_character),
    type = class_character,
    description = optional(S7::class_character),
    fields = class_list
  ),
  constructor = function(
    name = NULL,
    description = NULL,
    fields
  ) {
    new_object(
      S7_object(),
      name = name,
      type = "object",
      description = description,
      fields = fields
    )
  },
  validator = function(self) {
    # name and description are optional scalar characters
    check_optional_scalar_character(self@name, "name")
    check_optional_scalar_character(self@description, "description")
    # at least one field is required
    if (length(self@fields) == 0L) {
      cli::cli_abort(
        "{.cls Schema} must have at least one field. Add a {.cls Field}."
      )
    }
    # all fields must be Field objects
    if (!all(sapply(self@fields, function(x) S7_inherits(x, Field)))) {
      cli::cli_abort(
        "All {.field fields} must be {.cls Field} objects. Use {.fun field}."
      )
    }
    # field names must be unique
    field_names <- vapply(
      self@fields,
      function(x) {
        x@name
      },
      character(1L),
      USE.NAMES = FALSE
    )
    if (anyDuplicated(field_names)) {
      cli::cli_abort(
        "Schema field names must be unique. Rename duplicate fields."
      )
    }
    NULL
  }
)


# %% repr.Schema ----
method(repr, Schema) <- function(x, pad = 0L, output_type = NULL) {
  out <- setNames(
    list(
      list(
        description = x@description,
        fields = sapply(x@fields, function(f) {
          setNames(
            list(
              list(
                type = f@type,
                description = f@description,
                required = f@required
              )
            ),
            f@name
          )
        })
      )
    ),
    x@name
  )
  repr_ls(out, pad = pad, output_type = output_type)
}


# %% print.Schema ----
method(print, Schema) <- function(x, output_type = NULL, ...) {
  cat(repr(x, output_type = output_type), "\n")
  invisible(x)
}


# %% as_list.Field ----
#' Convert Field to a JSON Schema property list
#'
#' @param x Field: Field object.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(as_list, Field) <- function(x) {
  out <- list(type = x@type)
  # JSON Schema descriptions are optional, so omit NULL descriptions.
  if (!is.null(x@description)) {
    out[["description"]] <- x@description
  }
  out
}


# %% to_json.Field ----
#' Convert Field to JSON
#'
#' @param x Field: Field object.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(to_json, Field) <- function(x) {
  jsonlite::toJSON(as_list(x), auto_unbox = TRUE)
}


# %% as_list.Schema ----
#' Convert Schema to a JSON Schema list
#'
#' @param x Schema: Schema object.
#'
#' @return Named list.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(as_list, Schema) <- function(x) {
  properties <- structure(
    lapply(x@fields, as_list),
    names = sapply(x@fields, function(field) field@name, USE.NAMES = FALSE)
  )
  required <- sapply(
    Filter(function(field) field@required, x@fields),
    function(field) field@name,
    USE.NAMES = FALSE
  )
  out <- list(
    type = x@type,
    properties = properties,
    required = I(as.character(required))
  )
  if (!is.null(x@description)) {
    out[["description"]] <- x@description
  }
  out
}


# %% to_json.Schema ----
#' Convert Schema to JSON
#'
#' @param x Schema: Schema object.
#'
#' @return Character.
#'
#' @author EDG
#' @keywords internal
#' @noRd
method(to_json, Schema) <- function(x) {
  jsonlite::toJSON(as_list(x), auto_unbox = TRUE)
}


# %% field() ----
#' Define a schema field
#'
#' @param name Optional Character: The name of the field.
#' @param description Optional Character: A brief description of the field.
#' @param type Character \{"string", "number", "integer", "boolean", "array", "object"\}: The field
#'   type.
#' @param required Logical: Whether the field is required.
#'
#' @return Field object
#'
#' @author EDG
#' @export
#'
#' @examples
#' # `type`` defaults to "string", `required` defaults to TRUE
#' field("lab_name", "Name of the lab test")
#' field("normal_range_low", "Lower bound of normal range", type = "number")
field <- function(
  name,
  description = name,
  type = c("string", "number", "integer", "boolean", "array", "object"),
  required = TRUE
) {
  type <- match.arg(type)
  Field(
    name = name,
    type = type,
    description = description,
    required = required
  )
}


# %% schema() ----
#' Define output schema for LLM responses
#'
#' @param name Optional Character: The name of the schema.
#' @param ... Field objects defining the schema fields.
#' @param description Optional Character: A brief description of the schema.
#'
#' @return Schema object, named list, or JSON string.
#'
#' @author EDG
#' @export
#'
#' @examples
#' schema(
#'   "LabSchema",
#'   field("Lab name"),
#'   field("normal range low", type = "number"),
#'   field("normal range high", type = "number")
#' )
schema <- function(
  name = NULL,
  ...,
  description = NULL
) {
  Schema(
    name = name,
    description = description,
    fields = list(...)
  )
}
