# rtemis.llm Specifications

## Ollama API Support

Ollama is the reference/initial backend. It runs locally with no API key. All adapter generics have Ollama implementations that reproduce the original behavior.

References:
- Ollama chat API: <https://docs.ollama.com/api/chat>
- Ollama tool calling: <https://docs.ollama.com/capabilities/tool-calling>
- Ollama model library: <https://ollama.com/library>

### Public API

```r
config_Ollama(
  model_name,
  temperature = TEMPERATURE_DEFAULT,
  base_url = OLLAMA_URL_DEFAULT,
  validate_model = TRUE
)

create_Ollama(
  model_name,
  system_prompt = SYSTEM_PROMPT_DEFAULT,
  temperature = TEMPERATURE_DEFAULT,
  output_schema = NULL,
  name = NULL,
  base_url = OLLAMA_URL_DEFAULT,
  validate_model = TRUE
)
```

### Classes

Constants in `R/05_LLMConfig.R`:

```r
OLLAMA_URL_DEFAULT <- "http://localhost:11434"
```

`OllamaConfig`, parent `LLMConfig`:
- Inherits `model_name`, `temperature`, `base_url`, `backend` (set to `"ollama"`).
- Constructor calls `ollama_check_model()` when `validate_model = TRUE`.

`Ollama`, parent `LLM`:
- `config`: `OllamaConfig`
- `output_schema`: Optional list

`OllamaMessage`, parent `LLMMessage`, with `metadata[["provider"]] <- "Ollama"`.

### Adapter Methods

All dispatch on `OllamaConfig` in `R/utils_llm_adapter.R`.

**`build_chat_messages.OllamaConfig`**: Calls `get_message_list(state)`. Tool results are sent as `AgentMessage` with `role = "user"` (Ollama does not support the OpenAI `tool` role).

**`build_chat_request_body.OllamaConfig`**:
```r
list(
  model   = config@model_name,
  messages = build_chat_messages(config, state),
  stream  = FALSE,
  options = list(temperature = config@temperature),
  format  = build_response_format(config, output_schema),
  tools   = lapply(tools, as_list),
  think   = think
)
```

**`perform_chat_request.OllamaConfig`**: POSTs to `{base_url}/api/chat`.

**`parse_chat_response.OllamaConfig`**: Reads `resp[["message"]]`. Captures `thinking` field into `reasoning` when present. Returns normalized list: `content`, `reasoning`, `tool_calls`, `refusal = NULL`, `metadata`.

**`decode_tool_arguments.OllamaConfig`**: Returns `tool_call[["function"]][["arguments"]]` directly — Ollama delivers arguments as an already-parsed R list.

**`build_tool_message.OllamaConfig`**: Appends a `ToolMessage` for local history and an `AgentMessage` (role `"user"`) for the follow-up request body — Ollama requires tool results as user messages.

**`build_response_format.OllamaConfig`**: Passes `output_schema` through directly to the Ollama `format` field (Ollama accepts the raw JSON Schema object).

### Authentication

None required. Ollama is a local service; no `Authorization` header is sent.

### Model Utilities

```r
ollama_list_models(base_url = OLLAMA_URL_DEFAULT)
ollama_get_model_info(x, base_url = OLLAMA_URL_DEFAULT)
ollama_check_model(x)
```

`ollama_list_models()` — `GET {base_url}/api/tags`.
`ollama_get_model_info()` — `POST {base_url}/api/show`.
`ollama_check_model()` — verifies model availability; aborts with corrective message if missing.

### Non-Goals

- Streaming (`stream = TRUE`)
- Ollama generate (text completion) endpoint — chat endpoint only
- Embeddings
- Retry/backoff

---

## OpenAI API Support

### Goal

Add support for OpenAI-compatible chat APIs while preserving the current package shape:

- Users create typed S7 configuration objects with functional constructors.
- `generate()` works for direct LLM calls and stateful `Agent` calls.
- Agent memory, tools, structured output, and validation remain backend-independent at the public API layer.
- Provider-specific request bodies, response parsing, auth, and tool-message formats are isolated behind S7 methods.

The first implementation should target the OpenAI-compatible Chat Completions API (`/v1/chat/completions`). This covers OpenAI and many local/private gateways such as LM Studio, vLLM, llama.cpp server, LiteLLM, and other OpenAI-compatible proxies. The newer OpenAI Responses API should be left for a future provider adapter because it is more capable but less broadly supported by OpenAI-compatible servers.

Current reference points:

- OpenAI Chat Completions API: <https://platform.openai.com/docs/api-reference/chat/create>
- OpenAI API authentication: <https://platform.openai.com/docs/api-reference/authentication>
- OpenAI function calling: <https://platform.openai.com/docs/guides/function-calling>
- OpenAI structured outputs: <https://platform.openai.com/docs/guides/structured-outputs>
- Ollama chat API: <https://docs.ollama.com/api/chat>
- Ollama tool calling: <https://docs.ollama.com/capabilities/tool-calling>

### Package Review

The package already has the right high-level concepts:

- `LLMConfig` is the provider configuration superclass. `OllamaConfig` currently specializes it.
- `LLM` is a stateless superclass. `Ollama` specializes direct generation.
- `Agent` owns a `LLMConfig`, `AgentMemory`, optional tools, optional output schema, and the public `generate()` interaction loop.
- Messages are represented as S7 classes: `SystemMessage`, `InputMessage`, `LLMMessage`, `OllamaMessage`, `AgentMessage`, and `ToolMessage`.
- Tools are S7 objects with JSON-schema-like parameters, and `as_list.Tool()` already emits the OpenAI/Ollama `tools = [{ type = "function", function = ... }]` shape.
- Output schemas are plain JSON Schema-like lists from `schema()` / `field()` and `make_output_schema()`.

The main issue is that `generate.Agent()` is internally Ollama-specific:

- It builds Ollama request bodies directly (`options`, `format`, `think`).
- It posts directly to `/api/chat`.
- It parses Ollama responses directly from `res[["message"]]`.
- It assumes tool arguments are already R lists.
- It converts tool responses into a synthetic user `AgentMessage` instead of provider-native tool messages.

This is the correct place to refactor before adding OpenAI-compatible support. Otherwise each new backend will add conditionals to `generate.Agent()`, making tools and structured output fragile.

### Design Decision

Implement OpenAI-compatible support through provider adapters, not a parallel agent implementation.

`Agent` should continue to own the orchestration:

1. Append user input to memory.
2. Build provider-specific chat messages from memory.
3. Build provider-specific request body.
4. Perform the request.
5. Parse the provider response into a normalized result.
6. Append an `LLMMessage` subclass.
7. Execute allowed tool calls.
8. Append tool responses in a provider-compatible way.
9. Continue until no tool calls or `max_tool_rounds` is reached.

Provider differences should live in internal S7 generics dispatched on `LLMConfig`.

### Public API

Add OpenAI constructors:

```r
config_OpenAI(
  model_name,
  temperature = TEMPERATURE_DEFAULT,
  base_url = OPENAI_URL_DEFAULT,
  api_key = NULL,
  api_key_env = "OPENAI_API_KEY",
  keychain_service = NULL,
  organization = NULL,
  project = NULL,
  timeout = 60,
  extra_headers = NULL,
  extra_body = NULL,
  enable_thinking = NULL,
  validate_model = FALSE
)

create_OpenAI(
  model_name,
  system_prompt = SYSTEM_PROMPT_DEFAULT,
  temperature = TEMPERATURE_DEFAULT,
  output_schema = NULL,
  name = NULL,
  base_url = OPENAI_URL_DEFAULT,
  api_key = NULL,
  api_key_env = "OPENAI_API_KEY",
  keychain_service = NULL,
  organization = NULL,
  project = NULL,
  timeout = 60,
  extra_headers = NULL,
  extra_body = NULL,
  enable_thinking = NULL,
  validate_model = FALSE
)
```

The S7 classes should be named `OpenAIConfig` and `OpenAI`. Use `base_url` to point the same API at OpenAI-compatible local and proxy providers.

Example usage:

```r
llmconfig <- config_OpenAI(
  model_name = "gpt-4o-mini",
  api_key_env = "OPENAI_API_KEY"
)

agent <- create_agent(
  llmconfig = llmconfig,
  system_prompt = "You are a meticulous research assistant.",
  tools = list(tool_wikipedia),
  name = "ResearchAgent"
)

generate(agent, "Find the current title of the rtemis Wikipedia page.")
```

Local OpenAI-compatible endpoint:

```r
llmconfig <- config_OpenAI(
  model_name = "local-model",
  base_url = "http://localhost:1234/v1",
  api_key = NULL,
  validate_model = FALSE
)
```

### New Classes

Add constants in `R/04_S7_LLMConfig.R`:

```r
OPENAI_URL_DEFAULT <- "https://api.openai.com/v1"
OPENAI_API_KEY_ENV_DEFAULT <- "OPENAI_API_KEY"
OPENAI_TIMEOUT_DEFAULT <- 60
```

Add `OpenAIConfig`, parent `LLMConfig`, with properties:

- `api_key`: Optional character. Prefer `NULL` and resolve from env/keychain at request time.
- `api_key_env`: Character. Environment variable name to read.
- `keychain_service`: Optional character. Service name for `get_keychain_secret()`.
- `organization`: Optional character. Sent as `OpenAI-Organization` when defined.
- `project`: Optional character. Sent as `OpenAI-Project` when defined.
- `timeout`: Numeric `(0, Inf)`.
- `extra_headers`: Optional list.
- `extra_body`: Optional list.
- `enable_thinking`: Optional logical. Enables model thinking for local OpenAI-compatible
  providers that support chat-template thinking controls.
- `validate_model`: Logical.

Validation:

- `model_name` must be scalar, non-empty character.
- `temperature` must be numeric scalar in `[0, 2]`, matching `LLMConfig`.
- `base_url` must be scalar, non-empty character with no trailing slash after cleaning.
- `api_key`, `api_key_env`, `keychain_service`, `organization`, `project` must be scalar character or `NULL`.
- `timeout` must be positive numeric scalar.
- `extra_headers` and `extra_body` must be named lists or `NULL`.
- If `validate_model = TRUE`, call `openai_check_model()` and fail with a corrective `cli::cli_abort()` message.
- Do not require an API key in the constructor when `base_url` is not OpenAI's official URL. Many local OpenAI-compatible servers accept no key or a dummy key.

Add `OpenAI`, parent `LLM`, with properties:

- `config`: `OpenAIConfig`
- `output_schema`: Optional list

Add `OpenAIMessage`, parent `LLMMessage`, with constructor metadata:

```r
metadata[["provider"]] <- "OpenAI-compatible"
```

If `base_url` is the official OpenAI URL, set `metadata[["provider"]] <- "OpenAI"`.

### Internal S7 Adapter Generics

Add internal generics in `R/00_S7_init.R` or a new adapter file:

```r
build_chat_messages <- new_generic("build_chat_messages", "x")
build_chat_request_body <- new_generic("build_chat_request_body", "x")
perform_chat_request <- new_generic("perform_chat_request", "x")
parse_chat_response <- new_generic("parse_chat_response", "x")
decode_tool_arguments <- new_generic("decode_tool_arguments", "x")
build_tool_message <- new_generic("build_tool_message", "x")
build_response_format <- new_generic("build_response_format", "x")
```

Each generic dispatches on `LLMConfig` and accepts additional arguments as needed. These are internal, so document with `@keywords internal` and `@noRd`.

Normalized `parse_chat_response()` should return a plain named list:

```r
list(
  content = character(1),
  reasoning = NULL,
  tool_calls = NULL,
  refusal = NULL,
  metadata = list()
)
```

This lets `generate.Agent()` stay provider-neutral.

Reasoning/thinking capture for OpenAI-compatible providers:

- Thinking can be enabled globally with `config_OpenAI(enable_thinking = TRUE)` or per call with
  `generate(..., think = TRUE)` on an `Agent` or `OpenAI` object.
- For local OpenAI-compatible providers, enabling thinking adds both a top-level
  `enable_thinking` request field and `chat_template_kwargs = list(enable_thinking = TRUE)`.
  The latter is required by several MLX/transformers-style servers whose chat templates control
  whether a model emits thinking traces.
- `enable_thinking` is intentionally local-provider-only. For the official OpenAI base URL, pass
  model-specific reasoning controls through `extra_body`.
- Some local and proxy providers expose model reasoning outside the visible answer using non-standard fields. The parser must normalize these into the `reasoning` return field.
- Check explicit assistant-message fields in this order as available: `reasoning`, `reasoning_content`, `thinking`, `thinking_content`, `thought`, and `thoughts`.
- Also check `reasoning_details` and any content-array parts with `type = "reasoning"`.
- If the visible `content` contains embedded `<think>...</think>` or `<thinking>...</thinking>` blocks, extract those blocks into `reasoning` and remove them from the returned `content`.
- The returned `content` should contain only the visible assistant answer. The returned `reasoning` should be `NULL` if no reasoning content is present, or a single character string if one or more reasoning sources are found.
- This behavior is especially important for OpenAI-compatible local servers such as oMLS/oMLX-style backends serving thinking-capable Gemma-family models, where the reasoning trace may not match the official OpenAI field names.

### Message Conversion

Do not keep using `get_message_list()` directly inside `generate.Agent()` for all providers. Keep it for backwards compatibility, but route provider requests through:

```r
build_chat_messages(x@llmconfig, running_state)
```

Ollama behavior:

- Preserve current behavior initially: filter out `ToolMessage`; send tool results back as `AgentMessage` with role `"user"`.
- This avoids changing current Ollama behavior while OpenAI support is added.

OpenAI-compatible behavior:

- `SystemMessage`: `list(role = "system", content = x@content)`
- `InputMessage`: `list(role = "user", content = x@content)` for text.
- `InputMessage` with `image_path`: future work unless a safe base64/image URL helper is added. For now, `cli::cli_abort()` with a corrective message if an image is sent to OpenAI-compatible backend.
- `LLMMessage`: include `role = "assistant"`, `content`, and `tool_calls` when present.
- `ToolMessage`: include `role = "tool"`, `tool_call_id`, and `content`.
- `AgentMessage`: send as `role = "user"` with content.

Update `ToolMessage` to include an optional `tool_call_id` property:

```r
tool_call_id = optional(S7::class_character)
```

This is needed because OpenAI-compatible chat APIs require the tool response to reference the assistant tool call id. Ollama can ignore this field.

### Request Body Mapping

Ollama chat body stays equivalent to current:

```r
list(
  model = config@model_name,
  messages = build_chat_messages(config, state),
  stream = FALSE,
  options = list(temperature = config@temperature),
  format = output_schema,
  tools = lapply(tools, as_list),
  think = think
)
```

OpenAI-compatible chat body:

```r
body <- list(
  model = config@model_name,
  messages = build_chat_messages(config, state),
  stream = FALSE,
  temperature = config@temperature
)
```

Optional fields:

- `tools = lapply(tools, as_list)` when tools are enabled.
- `response_format = build_response_format(config, output_schema)` when an output schema is supplied.
- Merge `config@extra_body` last, after validating it is a named list. This allows provider-specific options without expanding the S7 class for every vendor.

Do not send Ollama-only fields (`options`, `format`, `think`) to OpenAI-compatible endpoints.

### Structured Output

Keep `schema()` returning a JSON Schema object. Add provider-specific wrapping:

Ollama:

```r
build_response_format.OllamaConfig(output_schema) -> output_schema
```

OpenAI-compatible:

```r
build_response_format.OpenAIConfig(output_schema) -> list(
  type = "json_schema",
  json_schema = list(
    name = "rtemis_llm_output",
    strict = TRUE,
    schema = output_schema
  )
)
```

OpenAI strict structured output requires a supported JSON Schema subset. Add a small validator/cleaner before sending:

- Object schemas should include `type = "object"`.
- Object schemas should include `properties`.
- In strict mode, object schemas should set `additionalProperties = FALSE`.
- In strict mode, required fields should be explicit.

Consider updating `schema()` so it can emit strict OpenAI-compatible schemas:

```r
schema(..., strict = FALSE, output_type = c("list", "json"))
```

For first implementation, avoid changing the public schema API unless required. A private `clean_openai_schema()` can recursively add `additionalProperties = FALSE` and ensure required fields are character vectors.

### Tool Calling

`as_list.Tool()` is close to the OpenAI-compatible function schema shape. Tighten it:

- `required` should be a character vector, not a list.
- `properties` should be a named list.
- Add optional `strict = TRUE` only when the schema satisfies OpenAI strict requirements.
- If strict tool schemas are used, each object schema must include `additionalProperties = FALSE`, and all properties must be listed in `required`. Optional fields can be represented as nullable types in a future pass.

Tool argument decoding must become provider-specific:

Ollama:

```r
decode_tool_arguments.OllamaConfig(tool_call) -> tool_call[["function"]][["arguments"]]
```

OpenAI-compatible:

```r
decode_tool_arguments.OpenAIConfig(tool_call) -> jsonlite::fromJSON(
  tool_call[["function"]][["arguments"]],
  simplifyVector = FALSE
)
```

Validate decoded arguments are a named list before `do.call()`. If decoding fails, abort with:

```r
cli::cli_abort(c(
  "Could not decode arguments for tool {.val {tool_name}}.",
  i = "Check that the model returned valid JSON function arguments."
))
```

When appending tool responses:

- For OpenAI-compatible APIs, append `ToolMessage(name = tool_name, tool_call_id = tool_call[["id"]], content = tool_response)`.
- For Ollama, continue appending `ToolMessage` for local history and `AgentMessage` for the provider follow-up request.

### Authentication And Security

Add helper:

```r
resolve_api_key <- function(config)
```

Resolution order:

1. Explicit `config@api_key`
2. `Sys.getenv(config@api_key_env)`
3. `get_keychain_secret(service = config@keychain_service)` when defined
4. `NULL`

Do not print API keys. `repr.OpenAIConfig()` must redact:

```r
api_key = if (has_key) "<redacted>" else NULL
```

Request headers:

```r
Authorization: Bearer <api_key>
Content-Type: application/json
User-Agent: rtemis (www.rtemis.org)
OpenAI-Organization: <organization> # optional
OpenAI-Project: <project>           # optional
```

For local OpenAI-compatible endpoints with no key:

- If `base_url` is official OpenAI and no key is available, `cli::cli_abort()` with a corrective message.
- If `base_url` is not official OpenAI and no key is available, omit the Authorization header.

Errors should use `cli::cli_abort()` and include:

- Provider/backend
- HTTP status
- Parsed API error message when available
- Request id from `x-request-id` when available
- Corrective hint when likely: missing key, invalid model, bad base URL, malformed schema, rate limit

### Model Utilities

Add:

```r
openai_list_models(
  base_url = OPENAI_URL_DEFAULT,
  api_key = NULL,
  api_key_env = OPENAI_API_KEY_ENV_DEFAULT,
  keychain_service = NULL,
  organization = NULL,
  project = NULL
)

openai_check_model(
  x,
  base_url = OPENAI_URL_DEFAULT,
  api_key = NULL,
  api_key_env = OPENAI_API_KEY_ENV_DEFAULT,
  keychain_service = NULL,
  organization = NULL,
  project = NULL
)
```

These should work against `/models` when implemented by the server. Some compatible servers may not implement it fully, so constructors should not call this unless `validate_model = TRUE`.

### Direct LLM Generation

Implement `generate.OpenAI()` using Chat Completions:

1. Build messages from `system_prompt` and `prompt`.
2. Build OpenAI-compatible request body.
3. Perform request.
4. Parse response.
5. Return `OpenAIMessage`.

Unlike `generate.Ollama()`, do not use a text completion endpoint. Chat Completions is the compatibility target.

### Agent Refactor

Refactor `generate.Agent()` in the smallest safe steps:

1. Keep all state/memory behavior as-is.
2. Replace direct request-body construction with `build_chat_request_body(x@llmconfig, ...)`.
3. Replace direct `httr2::request()` calls with `perform_chat_request(x@llmconfig, request_body, verbosity)`.
4. Replace direct response parsing with `parse_chat_response(x@llmconfig, resp)`.
5. Replace direct tool argument extraction with `decode_tool_arguments(x@llmconfig, tool_call)`.
6. Replace direct tool response message construction with `build_tool_message(x@llmconfig, tool_call, tool_name, tool_response)`.
7. For follow-up requests, rebuild messages using `build_chat_messages(x@llmconfig, running_state)`.

This leaves the orchestration in one place while making provider behavior replaceable.

### File Layout

Recommended files:

- `R/04_S7_LLMConfig.R`: add OpenAI-compatible constants and `OpenAIConfig`.
- `R/01_S7_Message.R`: add `OpenAIMessage`; add optional `tool_call_id` to `ToolMessage`.
- `R/05_S7_LLM.R`: add `OpenAI`, `generate.OpenAI()`, public constructors.
- `R/06_S7_Agent.R`: refactor provider-specific logic into adapter calls.
- `R/utils_openai.R`: OpenAI-compatible request helpers, auth helpers, model utilities, response-format helpers.
- `R/utils_llm_adapter.R`: shared internal S7 adapter generics and default helpers if we want to avoid making `R/06_S7_Agent.R` too dense.
- `tests/testthat/test_openai.R`: OpenAI-compatible config, request body, parsing, auth redaction, and tool-call tests.

### Documentation

Every new public function needs roxygen documentation following package rules:

- `@param x Type: Description`
- Optional arguments documented as `Optional Type: Description`
- No default values in `@param` text
- Constraints included after type declaration when applicable
- `@return` includes S7 class name or concrete type

Every internal helper/generic needs roxygen documentation with:

```r
#' @keywords internal
#' @noRd
```

Public functions to document:

- `config_OpenAI()`
- `create_OpenAI()`
- `openai_list_models()`
- `openai_check_model()`

### Tests

Unit tests should avoid real network calls by default.

Test groups:

- `OpenAIConfig` construction and validation
- `config_OpenAI()` and `create_OpenAI()` return correct S7 classes
- `repr()` redacts API keys
- Missing API key aborts only for official OpenAI requests, not for local compatible endpoints
- Request body mapping uses `temperature` top-level and does not include Ollama-only `options`, `format`, or `think`
- Structured output wraps schemas as `response_format`
- Tool definitions encode `required` as a character vector
- OpenAI tool-call arguments decode from JSON string to named list
- Tool messages include `tool_call_id`
- OpenAI response parsing extracts:
  - `choices[[1]][["message"]][["content"]]`
  - `choices[[1]][["message"]][["tool_calls"]]`
  - `choices[[1]][["message"]][["refusal"]]`
  - reasoning from `reasoning`, `reasoning_content`, `thinking`, `thinking_content`, `reasoning_details`, content-array reasoning parts, and embedded `<think>` / `<thinking>` tags
  - `usage`, `id`, `model`, `finish_reason`, and request id metadata
- `generate.Agent()` can complete a mocked no-tool response
- `generate.Agent()` can complete a mocked single-tool response and follow-up response

Optional integration tests:

```r
skip_if_not(Sys.getenv("RTEMIS_LLM_OPENAI_TESTS") == "true")
skip_if(Sys.getenv("OPENAI_API_KEY") == "")
```

Run these only when explicitly enabled.

### Backward Compatibility

Do not change existing public Ollama APIs:

- `config_Ollama()`
- `create_Ollama()`
- `ollama_list_models()`
- `ollama_get_model_info()`
- `ollama_check_model()`

Keep current Ollama behavior working while moving implementation details behind adapter methods. Any change to Ollama tool follow-up semantics should be a separate commit after OpenAI-compatible support has tests.

### Non-Goals For First Pass

- Streaming responses
- Responses API
- Audio inputs/outputs
- Vision support for OpenAI-compatible `image_path`
- Embeddings
- Server-side conversation state
- Automatic retry/backoff
- Token counting
- Tool-choice controls beyond default auto behavior
- Strict schema generation for nested optional tool parameters

These are valuable, but they should not block clean Chat Completions support.

### Implementation Checklist

1. Add `OpenAIConfig`, constants, validation, and redacted `repr()`.
2. Add `OpenAIMessage` and `create_llm_message.OpenAIConfig()`.
3. Add optional `tool_call_id` to `ToolMessage`.
4. Add internal S7 adapter generics.
5. Implement Ollama adapter methods that reproduce current behavior.
6. Implement OpenAI-compatible adapter methods.
7. Implement `config_OpenAI()` and `create_OpenAI()`.
8. Implement `generate.OpenAI()`.
9. Refactor `generate.Agent()` to call adapters.
10. Add OpenAI model utilities.
11. Add tests for config, request body, parser, tool calls, and agent flow.
12. Update roxygen docs and regenerate `NAMESPACE` / `man`.

### Acceptance Criteria

- Existing Ollama tests continue to pass.
- New OpenAI-compatible unit tests pass without network access.
- No OpenAI API key appears in printed objects, errors, snapshots, or test output.
- `create_agent(llmconfig = config_OpenAI(...))` works with the same public `generate()` API as Ollama.
- Tool calls work for both providers, including OpenAI-compatible JSON-encoded function arguments.
- Structured output maps to Ollama `format` and OpenAI-compatible `response_format`.
- All new user-facing errors use `cli::cli_abort()` and give corrective guidance.
- All new code follows S7 patterns and package documentation rules.


## Claude (Anthropic) API Support

### Goal

Extend the same S7 adapter architecture used for Ollama and OpenAI-compatible providers to Anthropic's Claude Messages API. `generate.Agent()` orchestration must stay provider-neutral; Claude-specific request/response shaping must be isolated behind the same internal generics (`build_chat_messages`, `build_chat_request_body`, `perform_chat_request`, `parse_chat_response`, `decode_tool_arguments`, `build_tool_message`, `build_response_format`, `create_llm_message`).

Current reference points:

- Messages API: <https://docs.anthropic.com/en/api/messages>
- Authentication: <https://docs.anthropic.com/en/api/overview>
- Tool use: <https://docs.anthropic.com/en/docs/build-with-claude/tool-use>
- Structured outputs via tool-forcing: <https://docs.anthropic.com/en/docs/build-with-claude/tool-use/json-mode>
- Extended thinking: <https://docs.anthropic.com/en/docs/build-with-claude/extended-thinking>
- Rate limit and error headers: <https://docs.anthropic.com/en/api/errors>

### API Differences From OpenAI-Compatible

The Messages API is close in spirit to Chat Completions but differs in several important places that the adapter layer must translate:

1. **Endpoint**: `POST {base_url}/messages` (default `base_url` is `https://api.anthropic.com/v1`).
2. **Authentication headers**:
   - `x-api-key: <api_key>` (not `Authorization: Bearer ...`)
   - `anthropic-version: 2023-06-01` (required; configurable via config)
   - Optional `anthropic-beta: <feature-ids>` for beta features.
3. **System prompt is a top-level field**, not a message. The `messages` array must only contain `user` and `assistant` roles.
4. **`max_tokens` is required** on every request.
5. **Assistant message content is an array of blocks**, not a single string. Block types include `text`, `thinking`, `redacted_thinking`, `tool_use`, and `server_tool_use`.
6. **Tool calls are `tool_use` blocks** embedded inside the assistant `content` array, not a sibling `tool_calls` field. Each block carries `id`, `name`, and `input` (input is already a parsed object, not a JSON string).
7. **Tool results come back as `user` messages** containing `tool_result` content blocks, each referencing the original `tool_use_id`.
8. **Tool schema shape is flatter**: top-level `name`, `description`, `input_schema` - no wrapping under `{type = "function", function = ...}`.
9. **Structured output** has no native `response_format` equivalent. The standard pattern is a forced tool call: provide a single tool whose `input_schema` is the output schema, plus `tool_choice = {type = "tool", name = "..."}`.
10. **Extended thinking** is requested via `thinking = {type = "enabled", budget_tokens = N}`. Returned thinking comes back as `thinking` and `redacted_thinking` content blocks.
11. **Stop reason** is `stop_reason` (string) not `finish_reason`, with values including `end_turn`, `max_tokens`, `tool_use`, `stop_sequence`, `pause_turn`, and `refusal`.
12. **Request id** is returned on the `request-id` header (not `x-request-id`).
13. **Errors** return `{"type": "error", "error": {"type": "...", "message": "..."}}`; non-2xx status codes should be surfaced with type and message.

### Design Decision

Reuse the existing adapter generics. Add a `ClaudeConfig` subclass of `LLMConfig`, a `Claude` subclass of `LLM`, a `ClaudeMessage` subclass of `LLMMessage`, and provider-specific methods for every internal generic. No changes to `generate.Agent()` orchestration. The Agent loop will continue to use `tool_calls` metadata at the R level; the Claude adapter is responsible for projecting those R-level tool calls back into Claude's native `tool_use` block shape on follow-up requests, and for parsing `tool_use` blocks out of responses into the R-level `tool_calls` representation the loop already understands.

### Public API

```r
config_Claude(
  model_name,
  temperature = TEMPERATURE_DEFAULT,
  base_url = CLAUDE_URL_DEFAULT,
  api_key = NULL,
  api_key_env = "ANTHROPIC_API_KEY",
  keychain_service = NULL,
  anthropic_version = CLAUDE_API_VERSION_DEFAULT,
  anthropic_beta = NULL,
  max_tokens = CLAUDE_MAX_TOKENS_DEFAULT,
  timeout = CLAUDE_TIMEOUT_DEFAULT,
  extra_headers = NULL,
  extra_body = NULL,
  thinking_budget_tokens = NULL,
  validate_model = FALSE
)

create_Claude(
  model_name,
  system_prompt = SYSTEM_PROMPT_DEFAULT,
  temperature = TEMPERATURE_DEFAULT,
  output_schema = NULL,
  name = NULL,
  base_url = CLAUDE_URL_DEFAULT,
  api_key = NULL,
  api_key_env = "ANTHROPIC_API_KEY",
  keychain_service = NULL,
  anthropic_version = CLAUDE_API_VERSION_DEFAULT,
  anthropic_beta = NULL,
  max_tokens = CLAUDE_MAX_TOKENS_DEFAULT,
  timeout = CLAUDE_TIMEOUT_DEFAULT,
  extra_headers = NULL,
  extra_body = NULL,
  thinking_budget_tokens = NULL,
  validate_model = FALSE
)
```

Example usage:

```r
llmconfig <- config_Claude(
  model_name = "claude-sonnet-4-6",
  api_key_env = "ANTHROPIC_API_KEY"
)

agent <- create_agent(
  llmconfig = llmconfig,
  system_prompt = "You are a meticulous research assistant.",
  tools = list(tool_wikipedia),
  name = "ResearchAgent"
)

generate(agent, "Find the current title of the rtemis Wikipedia page.")
```

Extended thinking:

```r
llmconfig <- config_Claude(
  model_name = "claude-sonnet-4-6",
  thinking_budget_tokens = 4000L
)

generate(agent, "Plan a multi-step analysis.", think = TRUE)
```

### New Classes

Add constants in `R/04_S7_LLMConfig.R`:

```r
CLAUDE_URL_DEFAULT <- "https://api.anthropic.com/v1"
CLAUDE_API_KEY_ENV_DEFAULT <- "ANTHROPIC_API_KEY"
CLAUDE_API_VERSION_DEFAULT <- "2023-06-01"
CLAUDE_MAX_TOKENS_DEFAULT <- 4096L
CLAUDE_TIMEOUT_DEFAULT <- 60
```

`ClaudeConfig`, parent `LLMConfig`, adds properties:

- `api_key`: Optional character. Prefer `NULL` and resolve from env/keychain at request time.
- `api_key_env`: Character. Environment variable name to read.
- `keychain_service`: Optional character. Service name for `get_keychain_secret()`.
- `anthropic_version`: Character. Required header value.
- `anthropic_beta`: Optional character or character vector. Sent as the `anthropic-beta` header (comma-joined if length > 1).
- `max_tokens`: Integer `[1, Inf)`. Required by the API.
- `timeout`: Numeric `(0, Inf)`.
- `extra_headers`: Optional named list.
- `extra_body`: Optional named list.
- `thinking_budget_tokens`: Optional integer `[1024, Inf)`. When non-NULL, the request body includes `thinking = list(type = "enabled", budget_tokens = N)`.
- `validate_model`: Logical.

Validation rules mirror `OpenAIConfig`, with these additions:

- `anthropic_version` must be scalar, non-empty character.
- `max_tokens` must be positive integer-coercible scalar (store as integer).
- `thinking_budget_tokens` must be `NULL` or a positive integer-coercible scalar. Extended thinking minimum is 1024 tokens; if the user passes a smaller value, abort with a corrective hint.
- `temperature` stays in `[0, 2]` from `LLMConfig`. Note the Anthropic API actually clamps at `[0, 1]`; we do not re-validate here but trust the server to reject invalid values.

Set `backend = "anthropic"` in the `LLMConfig` parent constructor.

`Claude`, parent `LLM`, with properties:

```r
config = ClaudeConfig
output_schema = optional(S7::class_list)
```

`ClaudeMessage`, parent `LLMMessage`, with constructor metadata `metadata[["provider"]] <- "Anthropic"`. Content on the R side remains a flattened text string; the raw Anthropic content array is preserved in `metadata[["raw_content"]]` so downstream consumers can inspect `tool_use` / `thinking` blocks if they need to.

### Internal Adapter Methods

All of the following dispatch on `ClaudeConfig`:

**`build_chat_messages.ClaudeConfig`**
- Filter out `SystemMessage` instances (Claude takes system as a top-level field).
- Map `InputMessage` -> `list(role = "user", content = <blocks>)`. For text-only, blocks are `list(list(type = "text", text = content))`. Image support is out of scope for first pass; abort with a corrective message if `image_path` is set.
- Map `AgentMessage` -> `list(role = "user", content = list(list(type = "text", text = content)))`.
- Map `LLMMessage` / `ClaudeMessage` -> `list(role = "assistant", content = <blocks>)`. When `metadata[["raw_content"]]` is preserved, reuse it directly. Otherwise rebuild blocks from `content` (as `type = "text"`) and `tool_calls` (each projected back to `type = "tool_use"` with `id`, `name`, and `input`).
- Map `ToolMessage` -> a `user`-role message whose `content` is a single `tool_result` block. Each block carries `tool_use_id` (pulled from `ToolMessage@tool_call_id`) and the response text as `content`. Consecutive `ToolMessage`s that belong to the same assistant turn should be merged into one user message with multiple `tool_result` blocks - the Claude API expects all tool results for one assistant turn in a single user message.
- Abort if a `ToolMessage` has no `tool_call_id`.

**`build_chat_request_body.ClaudeConfig`**
- `model`, `messages`, `max_tokens`, `temperature`.
- Extract `system` from the `SystemMessage` in state (the first one wins). If none, omit the field.
- If `tools` are present and `use_tools` is TRUE, map each `Tool` to `list(name = ..., description = ..., input_schema = ...)` via a Claude-specific helper (do not reuse `as_list.Tool()` which emits OpenAI shape).
- If `thinking_budget_tokens` is non-NULL, or `think = TRUE` is passed to `generate()`, include `thinking = list(type = "enabled", budget_tokens = <N>)`. When `think = TRUE` is passed but no budget is set anywhere, abort with a hint pointing at `thinking_budget_tokens`.
- If `output_schema` is supplied, call `build_response_format.ClaudeConfig(output_schema)` which returns a list with `tools` (injects/augments the request `tools` with a synthetic `respond_with_structured_output` tool) and `tool_choice = list(type = "tool", name = "respond_with_structured_output")`. Merge that into the request body.
- Merge `extra_body` last.
- Never include `stream`, `response_format`, or OpenAI-specific fields.

**`perform_chat_request.ClaudeConfig`**
- Post to `{base_url}/messages`.
- Add headers: `x-api-key`, `anthropic-version`, optional `anthropic-beta`, `content-type: application/json`, user agent, `extra_headers`.
- Apply `timeout`.
- Surface errors using `.check_http_response()` with provider name `"Anthropic"`, extended to also read `body[["error"]][["type"]]` and `request-id` / `x-request-id` headers.

**`parse_chat_response.ClaudeConfig`**
- Read the JSON body. Expect `content` (array of blocks), `stop_reason`, `stop_sequence`, `usage`, `id`, `model`, `role`.
- Concatenate visible `text` blocks with `\n\n` to form `content` (string).
- Concatenate `thinking` and `redacted_thinking` blocks into `reasoning` (NULL if none).
- Project `tool_use` blocks into the R-level `tool_calls` list. Each entry must be shaped like the Agent loop already expects from OpenAI/Ollama:
  ```r
  list(
    id = block[["id"]],
    type = "function",
    `function` = list(
      name = block[["name"]],
      arguments = block[["input"]]  # already a parsed list
    )
  )
  ```
  This lets `decode_tool_arguments.ClaudeConfig` return the list as-is, and lets the generic loop produce `ToolMessage(tool_call_id = id)` unchanged.
- Capture `stop_reason == "refusal"` as the normalized `refusal` field. For all other stop reasons, `refusal` is NULL.
- Metadata: include `id`, `model`, `stop_reason`, `stop_sequence`, `usage`, and `request_id` (read from `request-id` header, falling back to `x-request-id`). Also store `raw_content = <original content array>` so the `ClaudeMessage` constructor can preserve it for assistant-turn replay.

**`decode_tool_arguments.ClaudeConfig`**
- Claude returns tool `input` as an already-parsed JSON object. Reuse the projection in `parse_chat_response.ClaudeConfig`, so `tool_call[["function"]][["arguments"]]` is already a list. Return it. If it is a string (unexpected), fall back to `jsonlite::fromJSON(..., simplifyVector = FALSE)`.
- Validate the result is a named list; otherwise abort with corrective guidance.

**`build_tool_message.ClaudeConfig`**
- Require `tool_call[["id"]]`. Abort with a corrective message if missing.
- Return `ToolMessage(name = tool_name, tool_call_id = tool_call[["id"]], content = .tool_response_to_character(tool_response))`.
- The existing Agent loop does not need a parallel `AgentMessage` for Claude follow-ups (that hack was only for Ollama). `generate.Agent()` already skips it unless `llmconfig inherits OllamaConfig`.

**`build_response_format.ClaudeConfig`**
- Return NULL when `output_schema` is NULL.
- Otherwise return a named list with:
  - `tools`: a single-element list holding `list(name = "respond_with_structured_output", description = "...", input_schema = clean_claude_schema(output_schema))`.
  - `tool_choice`: `list(type = "tool", name = "respond_with_structured_output")`.
- `build_chat_request_body.ClaudeConfig` must merge these into the top-level body without clobbering user-supplied `tools`. If the user also provided `tools`, append the synthetic tool to that list and let the forced `tool_choice` handle dispatch. Document that structured output forces the model into the synthetic tool, so tool_use during structured output mode is limited to that one tool.

**`create_llm_message.ClaudeConfig`**
- Construct `ClaudeMessage(content, name, metadata, model_name, reasoning, tool_calls)`. Make sure `metadata[["raw_content"]]` propagates so `build_chat_messages.ClaudeConfig` can round-trip assistant turns without lossy reconstruction.

### Schema Cleaning For Claude

Claude tool `input_schema` accepts standard JSON Schema. Extended thinking + tool_use + structured-output forcing all work best with:

- `type = "object"`.
- `properties` as a named list.
- `required` as a character vector.
- `additionalProperties` is accepted but not required.

Add `clean_claude_schema(x)` that:

- Verifies top-level `type = "object"`.
- Ensures `properties` exists and is a named list.
- Coerces `required` to a character vector.
- Recurses into nested object / array `items` schemas.
- Leaves `additionalProperties` untouched; unlike OpenAI strict mode, Claude does not require it.

This keeps the cleaner simple and avoids accidentally rejecting schemas that Claude would otherwise accept.

### Authentication And Security

Reuse the resolve-api-key pattern:

```r
resolve_claude_api_key(config, error_if_missing = TRUE)
```

Resolution order: `config@api_key` -> `Sys.getenv(config@api_key_env)` -> `get_keychain_secret(service = config@keychain_service)` -> `NULL`.

If missing when required (official Anthropic URL or any non-empty real host), abort with:

```r
cli::cli_abort(c(
  "No Anthropic API key was found.",
  i = "Set {.envvar {config@api_key_env}}, pass {.var api_key}, or configure {.var keychain_service}.",
  i = "Keys can be created at {.url https://console.anthropic.com/}."
))
```

`as_list.ClaudeConfig()` and `repr.ClaudeConfig()` must redact `api_key` the same way `OpenAIConfig` does: show `"<redacted>"` if any key resolves, `NULL` otherwise. Never log or interpolate the key in error messages.

Request headers applied in `perform_chat_request.ClaudeConfig`:

```
x-api-key: <resolved_api_key>
anthropic-version: <config@anthropic_version>
anthropic-beta: <comma-joined config@anthropic_beta>  # when set
content-type: application/json
User-Agent: rtemis (www.rtemis.org)
```

`extra_headers` are merged last.

### Model Utilities

Add:

```r
claude_list_models(
  base_url = CLAUDE_URL_DEFAULT,
  api_key = NULL,
  api_key_env = CLAUDE_API_KEY_ENV_DEFAULT,
  keychain_service = NULL,
  anthropic_version = CLAUDE_API_VERSION_DEFAULT
)

claude_check_model(
  x,
  base_url = CLAUDE_URL_DEFAULT,
  api_key = NULL,
  api_key_env = CLAUDE_API_KEY_ENV_DEFAULT,
  keychain_service = NULL,
  anthropic_version = CLAUDE_API_VERSION_DEFAULT
)
```

Both hit `GET {base_url}/models` which returns `{"data": [{"id": "...", ...}, ...]}`. Paginate on `has_more = TRUE` and `last_id` if present; otherwise return the first page. Constructors only call `claude_check_model()` when `validate_model = TRUE`.

### Direct LLM Generation

`generate.Claude()` follows the pattern from `generate.OpenAI()`:

1. Build an `InProcessAgentMemory`, append the `SystemMessage` and the user's prompt `InputMessage`.
2. Build the Claude request body via `build_chat_request_body.ClaudeConfig(config, state, output_schema = ..., think = ..., use_tools = FALSE)`.
3. `perform_chat_request.ClaudeConfig()` and `parse_chat_response.ClaudeConfig()`.
4. Return a `ClaudeMessage` carrying `content`, `reasoning`, `tool_calls` (always NULL here since direct LLM has no tools), `metadata`, and `model_name`.

### Agent Refactor

No changes to `generate.Agent()` orchestration. The Ollama-specific `AgentMessage` shim remains gated on `S7_inherits(x@llmconfig, OllamaConfig)`. Claude goes through the native tool-result path already used by OpenAI.

One small enhancement: when appending `ToolMessage` objects to state, the current loop emits them one at a time. The Claude adapter's `build_chat_messages` must merge consecutive `ToolMessage`s (same assistant turn) into a single `user` message with multiple `tool_result` blocks. That merging is local to the adapter and does not require touching `generate.Agent()`.

### File Layout

- `R/04_S7_LLMConfig.R`: add Claude constants and `ClaudeConfig` class.
- `R/01_S7_Message.R`: add `ClaudeMessage`.
- `R/05_S7_LLM.R`: add `Claude` class, `generate.Claude()`, `config_Claude()`, `create_Claude()`.
- `R/06_S7_Agent.R`: add `create_llm_message.ClaudeConfig()`.
- `R/utils_llm_adapter.R`: add Claude adapter methods for all internal generics.
- `R/utils_claude.R`: Claude-specific helpers (auth, headers, schema cleaning, tool projection, model listing, error handling).
- `tests/testthat/test_claude.R`: config, request body, tool projection, response parsing, structured output, auth redaction.

### Tests

Network-free unit tests must cover:

- `ClaudeConfig` construction and field validation (required `max_tokens`, `anthropic_version`, optional `thinking_budget_tokens` lower bound).
- `config_Claude()` and `create_Claude()` return correct S7 classes.
- `repr()` and `as_list()` redact the API key.
- Missing API key aborts against the official Anthropic URL.
- Request body includes `max_tokens`, uses top-level `system`, does not include OpenAI-only `response_format`, and does not send `stream`.
- System messages are stripped from the `messages` array.
- `InputMessage`, `AgentMessage`, `LLMMessage`, and `ToolMessage` each map to the expected role + content blocks. Consecutive `ToolMessage`s merge into a single user message with multiple `tool_result` blocks.
- Tool definitions encode flat `{name, description, input_schema}` - no `{type: "function", function: {...}}` wrapping.
- `tool_use` blocks in the response project to the loop's expected `tool_calls` shape.
- `decode_tool_arguments.ClaudeConfig()` returns the `input` object as a named list.
- `build_tool_message.ClaudeConfig()` requires a `tool_call_id` and returns a `ToolMessage` with it.
- Thinking is injected when `thinking_budget_tokens` is set, and when `think = TRUE` is passed.
- `think = TRUE` with no budget aborts with corrective guidance.
- Structured output injects the synthetic `respond_with_structured_output` tool and forces `tool_choice`.
- Response parsing extracts `content` text blocks, `thinking` blocks, `tool_use` blocks, `stop_reason`, `usage`, `id`, `model`, `request_id`, and recognizes `stop_reason == "refusal"`.
- `ClaudeMessage` preserves `raw_content` in metadata so assistant turns replay correctly.

Optional integration tests gated on:

```r
skip_if_not(Sys.getenv("RTEMIS_LLM_CLAUDE_TESTS") == "true")
skip_if(Sys.getenv("ANTHROPIC_API_KEY") == "")
```

### Backward Compatibility

- No existing Ollama or OpenAI public function signatures change.
- `generate.Agent()` logic is unchanged.
- `as_list.Tool()` still emits the OpenAI/Ollama function shape; Claude tool encoding is a separate helper that does not touch `Tool` S7 semantics.

### Non-Goals For First Pass

- Streaming (`stream = TRUE`), event-stream parsing, and server-sent events.
- Vision / PDF / image blocks in user messages.
- Batch API.
- Citations and document blocks.
- Message-tokens counting (`/messages/count_tokens`).
- Server tool blocks (`web_search`, `computer_use`, `bash`, `code_execution`) - these are wired server-side by Anthropic and require their own allow-listing.
- Prompt caching cache-control blocks (can be added later via `extra_body` if needed).
- Fine-grained retry/backoff.

### Implementation Checklist

1. Add Claude constants and `ClaudeConfig` to `R/04_S7_LLMConfig.R`, with validators and redacted `as_list`/`repr`.
2. Add `ClaudeMessage` to `R/01_S7_Message.R`.
3. Add `Claude` class, `config_Claude()`, `create_Claude()`, `generate.Claude()` to `R/05_S7_LLM.R`.
4. Add `create_llm_message.ClaudeConfig()` to `R/06_S7_Agent.R`.
5. Add Claude adapter methods to `R/utils_llm_adapter.R`.
6. Add Claude helpers (auth, headers, schema cleaning, tool projection, model utilities) to `R/utils_claude.R`.
7. Add tests in `tests/testthat/test_claude.R`.
8. Update roxygen / regenerate `NAMESPACE` and `man/`.

### Acceptance Criteria

- Existing Ollama and OpenAI tests continue to pass unchanged.
- New Claude unit tests pass without network access.
- No Anthropic API key appears in printed objects, error messages, or test output.
- `create_agent(llmconfig = config_Claude(...))` works with the same public `generate()` API as Ollama and OpenAI-compatible.
- Tool calls work round-trip: model emits `tool_use` -> loop invokes local tool -> adapter appends `tool_result` user block -> model continues.
- Structured output produces a single synthetic forced tool call and returns the parsed object as the final assistant content.
- Extended thinking is respected both when configured globally and when toggled per call with `think = TRUE`.
- All new code follows S7 patterns and package documentation rules.
