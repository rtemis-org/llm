# rtemis.llm NEWS

## 0.8.5

- Structured responses are validated locally by default with a cached Ajv validator.
  `validate_output = FALSE` skips validation while still requesting the schema.
  `on_validation_failure = "warn"` retains invalid output and emits an informational
  rtemis.core message, not an R warning; `"collect"` is silent and `"abort"` raises
  an error carrying the response and diagnostics. Batches validate per item and
  summarize mismatches once at completion.
- Added standalone `validate_output()` and the `validation_results()` accessor.
  Reports preserve original text, input positions/names, statuses, and diagnostics,
  including when batch responses are extracted. Validation does not repair output.
- Agent final answers are validated before memory commit. `responses()` recognizes
  histories returned by `generate(agent, ...)` and extracts their final answer.
- Anthropic synthetic structured-output tool inputs are returned as answer JSON,
  retaining raw content metadata and excluding the synthetic tool from execution.
- Added `enum` to `field()`: restrict a field to a fixed set of permitted values. Emitted as the
  JSON Schema `enum` key and passed through unchanged by all three backends, so a backend with
  constrained decoding (Ollama) makes an invalid value impossible rather than merely detectable.
- Added `on_error` to `llmapply()`, `agentapply()`, and `map()`, defaulting to `"na"`: a failed
  call now warns and yields `NA` rather than discarding every result in the batch, and the result
  carries an `errors` attribute (a data.frame of `index` and `message`) for retrying just the
  failures. Pass `on_error = "abort"` for the previous behavior of propagating the first error.
- `responses()` and `reasoning()` now accept `NULL` elements, mapping them to `NA_character_`.
- Added `num_ctx` and `keep_alive` as per-call Ollama options in `generate()`.
- Added `logprobs` and `top_logprobs` per-call options for the Ollama and OpenAI-compatible
  backends, with new `logprobs()` and `token_probs()` accessors. `token_probs()` reads the
  probability of each candidate answer straight off the model's token distribution, which is
  better calibrated than asking a model to emit a number. Anthropic does not return log
  probabilities; its messages yield `NULL` rather than an error.
- Per-call options passed to `generate()` (`temperature`, `top_p`, `max_tokens`, `stop`,
  `top_k`, `seed`, `num_ctx`, `keep_alive`, `logprobs`, `top_logprobs`) are validated against
  each backend's documented bounds before a request is built, so an out-of-range or wrong-typed
  value fails locally instead of on the server. Bounds follow the backend: `temperature` accepts
  up to 2 on Ollama and OpenAI-compatible backends and up to 1 on Anthropic, and `top_logprobs`
  up to 20 on OpenAI. `top_logprobs` requires `logprobs = TRUE`, which every backend that
  returns alternatives needs in order to return them.

## 0.8.4

- Added `zero_data_retention` to `config_OpenAI()` and `create_OpenAI()` for per-request OpenRouter ZDR routing.
- Added `think` to `config_Ollama()`, `create_Ollama()`, and `generate()` for Ollama thinking control.
