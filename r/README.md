[![R CI](https://github.com/rtemis-org/llm/actions/workflows/r-ci-r2u.yml/badge.svg)](https://github.com/rtemis-org/llm/actions/workflows/r-ci-r2u.yml) [![rtemis.llm status badge](https://rtemis-org.r-universe.dev/rtemis.llm/badges/version)](https://rtemis-org.r-universe.dev/rtemis.llm) [![Docs](https://img.shields.io/badge/docs-rtemis.org/r-blue)](https://docs.rtemis.org/r/llm/)

# rtemis.llm R package

Unified interface for creating **`LLM`** and **`Agent`** objects, generating responses, and 
performing batch inference.  
Built on a type-checked and validated '**S7**' backend.  
Features **reasoning**, **structured output**, **memory management**, and **tool use**.  
Supports **Ollama**, **OpenAI**-compatible, and **Anthropic**-compatible endpoints.

## Features

|                   | `LLM` | `Agent` |
| ----------------: | :---: | :-----: |
|         Reasoning |   ✓   |    ✓    |
| Structured output |   ✓   |    ✓    |
|          Tool use |   x   |    ✓    |
| Memory management |   x   |    ✓    |
|  Batch generation |   ✓   |    ✓    |

## Installation

### R-universe

```r
pak::repo_add(myuniverse = "https://rtemis-org.r-universe.dev")
pak::pak("rtemis.llm")
```

### GitHub

```r
pak::pak("rtemis-org/llm")
```

## Documentation

For detailed documentation, see the [**rtemis.llm documentation**](https://docs.rtemis.org/r/llm/).

## Quick Usage

```r
library(rtemis.llm)
```

List available Ollama models

```r
ollama_list_models()
```

### LLM

Create an `LLM` object

```r
llm <- create_Ollama(
  model_name = "gemma4:26b",
  system_prompt = "You are a meticulous research assistant.",
  temperature = 0.3
)
```

```r
generate(llm, "What is the role of the telomere?")
```

### Agent

Create an `Agent` object

```r
agent <- create_agent(
  llmconfig = config_Ollama(
    model_name = "gemma4:26b",
    temperature = 0.3
  ),
  system_prompt = "You are a meticulous research assistant.",
  name = "Kaimana"
)
```

```r
generate(agent, "Explain quantum superposition in seven bullet points.")
```

### Structured output validation

Validation runs locally when an output schema is supplied. Invalid output is
retained by default, with an informational message through `rtemis.core::warn()`
(not an R warning). This applies to single responses and batches, including small
local models that may not reliably follow schemas.

```r
sch <- schema("Count", field("n", type = "integer"))
out <- llmapply(
  c("How many days are in a week?", "How many months are in a year?"),
  "gemma4:e4b",
  output_schema = sch
)
report <- validation_results(out)
report@status   # valid, invalid, unavailable, or not_validated
report@issues   # input index, JSON path, keyword, and diagnostic message
```

Set `on_validation_failure = "collect"` to record diagnostics silently, or
`"abort"` to raise an error on a mismatch. Batch validation occurs per response;
the default logs a single summary. Validation aborts follow the batch's `on_error`
policy, with rejected text retained in the validation report.

You can also generate with `validate_output = FALSE` and validate later, or check
any saved JSON directly:

```r
report <- validate_output(c('{"n":10}', '{"n":"10"}'), sch)
report@status  # "valid" "invalid"
```

Validation checks the requested schema without coercing values, stripping Markdown,
or repairing JSON. Current schemas allow extra properties, optional fields permit
omission but not null, and array/object fields constrain only the outer type.
