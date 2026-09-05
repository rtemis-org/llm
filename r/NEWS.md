# rtemis.llm NEWS

## 0.8.5

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

## 0.8.4

- Added `zero_data_retention` to `config_OpenAI()` and `create_OpenAI()` for per-request OpenRouter ZDR routing.
- Added `think` to `config_Ollama()`, `create_Ollama()`, and `generate()` for Ollama thinking control.
