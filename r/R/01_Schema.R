# %% Schema constants and helpers ----
# Allowed JSON Schema field types.
.SCHEMA_FIELD_TYPES <- c(
  "string",
  "number",
  "integer",
  "boolean",
  "array",
  "object"
)

# Field types a fixed value set (`enum`) can be declared for. "boolean" is
# already a two-value type, and "array"/"object" enumerate a structure rather
# than a scalar, which the `Field` class does not model.
.SCHEMA_ENUM_TYPES <- c("string", "number", "integer")

# Element types an array can be declared to hold by name alone. An array of
# objects is declared by passing a Schema instead.
.SCHEMA_ITEM_TYPES <- c("string", "number", "integer", "boolean")


# %% .item_schema() ----
#' Convert a Field's `items` to a JSON Schema item definition
#'
#' Three ways to say what an array holds, in increasing detail: a type name, a
#' [field] carrying a description or `enum` for the elements, or a [schema] for
#' an array of objects.
#'
#' @param items Character, Field or Schema: Element declaration.
#'
#' @return Named list, or NULL if `items` is not one of those.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.item_schema <- function(items) {
  if (is.character(items)) {
    if (length(items) != 1L || !items %in% .SCHEMA_ITEM_TYPES) {
      return(NULL)
    }
    return(list(type = items))
  }
  if (S7_inherits(items, Field) || S7_inherits(items, Schema)) {
    # An item schema is positional, so a Field's name has nowhere to go and is
    # dropped by as_list() already.
    return(as_list(items))
  }
  NULL
}


# %% .enum_values() ----
#' Coerce a Field's `enum` to its declared type
#'
#' `Field@enum` is stored as character so that one property declaration covers
#' every enumerable type, but the emitted JSON Schema must carry values of the
#' declared type: `"1"` would never match a field declared `"integer"`.
#'
#' @param enum Character: Permitted values.
#' @param type Character \{"string", "number", "integer"\}: Field type.
#'
#' @return Vector of `enum` coerced to `type`, or NULL if any value cannot be
#' represented in it.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.enum_values <- function(enum, type) {
  if (type == "string") {
    return(as.character(enum))
  }
  num <- suppressWarnings(as.numeric(enum))
  if (anyNA(num) || !all(is.finite(num))) {
    return(NULL)
  }
  if (type == "number") {
    return(num)
  }
  # "integer": reject fractional values and anything outside R's integer range,
  # which as.integer() would otherwise silently truncate or turn into NA.
  if (any(num != trunc(num))) {
    return(NULL)
  }
  int <- suppressWarnings(as.integer(num))
  if (anyNA(int)) {
    return(NULL)
  }
  int
}


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
#' @field enum Optional Character: Permitted values for this field.
#' @field required Logical: Whether the field is required by its parent schema.
#'
#' @author EDG
#' @keywords internal
#' @noRd
Field <- S7::new_class(
  "Field",
  properties = list(
    name = prop_string(description = "Field name"),
    type = prop_string(
      default = "string",
      enum = .SCHEMA_FIELD_TYPES,
      description = "JSON Schema type"
    ),
    description = prop_string(
      nullable = TRUE,
      description = "Field description"
    ),
    # Stored as character whatever the field's type: the property factory
    # enforces arity (at least one value), missingness and uniqueness, and the
    # class validator below enforces what it cannot see -- that the values fit
    # the declared `type`. `as_list()` coerces them back on the way out.
    enum = prop_string(
      nullable = TRUE,
      vector = TRUE,
      unique_items = TRUE,
      description = "Permitted values"
    ),
    required = prop_boolean(
      default = NULL,
      description = "Whether the parent schema requires the field"
    ),
    # Typed as `any` because the permitted classes include Field and Schema,
    # neither of which is bound while this class is being defined. The
    # validator below is what actually constrains it.
    items = new_property(
      class = class_any,
      default = NULL
    )
  ),
  validator = function(self) {
    # An array with no `items` is valid JSON Schema and useless in practice:
    # OpenAI's strict mode rejects it and a constrained-decoding backend has
    # nothing to constrain, so the model is left to guess the element type.
    if (self@type == "array") {
      if (is.null(self@items)) {
        abort(
          "An \"array\" field must declare what it contains.\n",
          "Set `items` to one of ",
          paste0("\"", .SCHEMA_ITEM_TYPES, "\"", collapse = ", "),
          ", to a field() for elements carrying their own description or ",
          "`enum`, or to a schema() for an array of objects."
        )
      }
      if (is.null(.item_schema(self@items))) {
        abort(
          "`items` must be one of ",
          paste0("\"", .SCHEMA_ITEM_TYPES, "\"", collapse = ", "),
          ", a Field from field(), or a Schema from schema().\n",
          "Got ",
          paste(class(self@items), collapse = "/"),
          "."
        )
      }
    } else if (!is.null(self@items)) {
      abort(
        "`items` cannot be set on a \"",
        self@type,
        "\" field.\n",
        "Drop `items`, or set `type` to \"array\"."
      )
    }
    if (!is.null(self@enum)) {
      # A fixed value set only means something for a scalar type.
      if (!self@type %in% .SCHEMA_ENUM_TYPES) {
        abort(
          "`enum` cannot be set on a \"",
          self@type,
          "\" field.\n",
          "Drop `enum`, or set `type` to one of ",
          paste0("\"", .SCHEMA_ENUM_TYPES, "\"", collapse = ", "),
          "."
        )
      }
      if (is.null(.enum_values(self@enum, self@type))) {
        abort(
          "`enum` values must all be representable as type \"",
          self@type,
          "\".\n",
          "Got ",
          paste0("\"", self@enum, "\"", collapse = ", "),
          ". Supply values of that type, or change `type` to \"string\"."
        )
      }
    }
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
          enum = x@enum,
          items = if (is.null(x@items)) NULL else .item_schema(x@items),
          required = x@required
        )
      ),
      x@name
    ),
    pad = pad,
    print_class = FALSE,
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
    name = prop_string(nullable = TRUE, description = "Schema name"),
    type = prop_const("object", description = "JSON Schema type"),
    description = prop_string(
      nullable = TRUE,
      description = "Schema description"
    ),
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
    # at least one field is required
    if (length(self@fields) == 0L) {
      abort("Schema must have at least one field. Add a Field.")
    }
    # all fields must be Field objects
    if (!all(sapply(self@fields, function(x) S7_inherits(x, Field)))) {
      abort("All `fields` must be Field objects. Use field().")
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
      abort("Schema field names must be unique. Rename duplicate fields.")
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
                enum = f@enum,
                items = if (is.null(f@items)) NULL else .item_schema(f@items),
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
  repr_ls(out, pad = pad, print_class = FALSE, output_type = output_type)
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
  # JSON Schema `enum` is always an array, so mark it AsIs to stop jsonlite's
  # `auto_unbox` collapsing a single permitted value to a bare scalar.
  if (!is.null(x@enum)) {
    out[["enum"]] <- I(.enum_values(x@enum, x@type))
  }
  # `items` says what an array holds. The validator guarantees it is present
  # and convertible whenever `type` is "array", and absent otherwise.
  if (!is.null(x@items)) {
    out[["items"]] <- .item_schema(x@items)
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
#' @param enum Optional Character: Permitted values for this field. Only for `type` "string",
#'   "number" or "integer". Backends that support constrained decoding (e.g. Ollama) make any
#'   other value impossible rather than merely detectable.
#' @param items Character, Field or Schema: What an array field contains.
#'   Required for `type = "array"` and forbidden otherwise. Give a type name
#'   (`"string"`, `"number"`, `"integer"`, `"boolean"`), a [field] where the
#'   elements need their own description or `enum`, or a [schema] for an array
#'   of objects.
#' @param required Logical: Whether the field is required.
#'
#' @return Field object
#'
#' @author EDG
#' @export
#'
#' @examples
#' # `type` defaults to "string", `required` defaults to TRUE
#' field("lab_name", "Name of the lab test")
#' field("normal_range_low", "Lower bound of normal range", type = "number")
#' field("flag", "Whether the result is out of range", enum = c("low", "normal", "high"))
#'
#' # An array of strings: one element per item, so nothing has to be delimited
#' # inside a single string and later split back apart.
#' field("questions", "Each question, quoted as written", type = "array", items = "string")
#'
#' # An array whose elements carry their own description
#' field(
#'   "codes", "ICD-10 codes found",
#'   type = "array",
#'   items = field("code", "One ICD-10 code, e.g. \"E11.9\"")
#' )
#'
#' # An array of objects
#' field(
#'   "results", "One row per lab result",
#'   type = "array",
#'   items = schema(
#'     "LabResult",
#'     field("name", "Test name"),
#'     field("value", "Result value", type = "number")
#'   )
#' )
field <- function(
  name,
  description = name,
  type = c("string", "number", "integer", "boolean", "array", "object"),
  enum = NULL,
  items = NULL,
  required = TRUE
) {
  type <- match.arg(type)
  Field(
    name = name,
    type = type,
    description = description,
    enum = enum,
    items = items,
    required = required
  )
}


# %% schema() ----
#' Define output schema for LLM responses
#'
#' @param name Optional Character: The name of the schema.
#' @param ... Field objects defining the schema fields. Create using [field].
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
