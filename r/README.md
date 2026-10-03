[![CRAN status](https://www.r-pkg.org/badges/version/rtemis.llm)](https://CRAN.R-project.org/package=rtemis.llm)
[![rtemis.llm status badge](https://rtemis-org.r-universe.dev/rtemis.llm/badges/version)](https://rtemis-org.r-universe.dev/rtemis.llm)
[![R CI](https://github.com/rtemis-org/llm/actions/workflows/r-ci-r2u.yml/badge.svg)](https://github.com/rtemis-org/llm/actions/workflows/r-ci-r2u.yml)
[![R-Docs](https://img.shields.io/badge/docs-rtemis.org/r-blue)](https://docs.rtemis.org/r/llm/)

# rtemis.llm R package

Unified interface for creating **`LLM`** and **`Agent`** objects, generating responses, and 
performing batch inference.  
Built on a type-checked and validated '**S7**' backend.  
Features **reasoning**, **structured output**, **memory management**, and **tool use**.  
Supports **Ollama**, **OpenAI**-compatible, and **Anthropic**-compatible endpoints, and
**Apple Foundation Models** on-device through the [rtemis-afm](https://github.com/rtemis-org/rtemis-afm) bridge.  
Fills closed schemas with **decision models** on Ollama and OpenRouter.

## Features

|                   | `LLM` | `Agent` | `DecisionModel` |
| ----------------: | :---: | :-----: | :-------------: |
|         Reasoning |   ✓   |    ✓    |        x        |
| Structured output |   ✓   |    ✓    |       ✓¹        |
|   Typed questions |   x   |    x    |        ✓        |
|          Tool use |   x   |    ✓    |        x        |
| Memory management |   x   |    ✓    |        x        |
|  Batch generation |   ✓   |    ✓    |        ✓        |
|       Image input |   ✓   |    ✓    |        ✓        |

¹ Closed schemas only: every field is an `enum`, a boolean, or an array of `enum` values.

## Installation

### CRAN

```{r}
install.packages("rtemis.llm")
```

or

```{r}
pak::pak("rtemis.llm")
```

### R-universe

```{r}
install.packages("rtemis.llm", repos = "https://rtemis-org.r-universe.dev")
```

or

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

### Apple Foundation Models

On an Apple silicon Mac with macOS 27 and Apple Intelligence turned on, the on-device
model is served by the [rtemis-afm](https://github.com/rtemis-org/rtemis-afm) bridge.
Install and start it once in a terminal (`curl -fsSL https://live.rtemis.org/afm.sh | sh`,
or `brew install rtemis-org/tap/rtemis-afm` then `rtemis-afm`); no API key is needed.

```r
llm <- create_Apple(system_prompt = "You are a meticulous research assistant.")
generate(llm, "What is the role of the telomere?")

agent <- create_agent(config_Apple(), tools = list(tool_datetime))
generate(agent, "What is the date today?")
```

`config_Apple()` checks the bridge's health first and says what to do if it is not
running or the model is unavailable; `apple_health()` reports the served model and its
context window (8,192 tokens on macOS 27.0).

### Image input

Send local PNG, JPEG, GIF, or WebP files with a prompt to any vision model on Ollama,
OpenAI-compatible (including Apple Foundation Models), or Anthropic-compatible backends.

```r
llm <- create_Ollama("gemma4:e4b")
generate(llm, "What does this figure show?", image_path = "figure1.png")

# One question over many images
figs <- llmapply(
  "Describe this figure.",
  llm,
  image_path = c("figure1.png", "figure2.png", "figure3.png")
)
```

A list of character vectors sends several images with each prompt. Every file is
checked before the first request.

### Decision models

A decision model answers typed questions about a passage with a probability for
every option, and writes no text. Ollama serves decision models such as
`clef-flash` locally; OpenRouter serves others with an API key.

**Questions are its natural interface.** A `choice()` picks among 2 to 26
options, each with what it means; a `noul()` asks how true a statement is. You
write the wording and the option meanings, and the model judges the passage
against them. For example, to screen study abstracts:

```r
dm <- create_DecisionModel(config_OllamaDecision("clef-flash"))
qs <- list(
  design = choice(
    "Which study design does the abstract describe?",
    c(
      randomized_trial = "Participants are randomly assigned to the interventions being compared",
      cohort = "A group is followed over time to relate exposures to later outcomes",
      case_control = "People with and without an outcome are compared on past exposures",
      diagnostic_accuracy = "A test or model is evaluated against a reference standard"
    )
  ),
  significant = noul("Does the abstract report a statistically significant primary result?")
)
abstract <- paste(
  "We followed 12,840 nurses for 20 years to examine the association between rotating",
  "night-shift work and incident breast cancer. Night-shift work was associated with a slightly",
  "higher risk that was not statistically significant (HR 1.12; 95% CI 0.98 to 1.28)."
)
d <- decide(dm, abstract, qs)
probabilities(d)  # every option's probability

# The same questions over many abstracts, with the wall time of each call
res <- dmapply(abstracts, dm, questions = qs)
attr(res, "elapsed")
```

**A schema adapts it to code written for LLMs.** `generate()` and `dmapply()`
also fill a closed schema, one whose every field is a field with an `enum`, a
boolean, or an array whose `items` is a field with an `enum`. The result is a
validated JSON document, shaped like an LLM's structured output. The schema is
first converted into questions, worded from each field's name and description.
`as_questions()` returns them, to read, edit and pass to `decide()`.

```r
study <- schema(
  "Study",
  field("design", "Study design", enum = c("randomized_trial", "cohort", "case_control", "diagnostic_accuracy")),
  field("significant", "Whether the primary result is statistically significant", type = "boolean")
)
is_decidable(study)  # TRUE; otherwise the open fields are listed
as_questions(study)  # what the decision model is asked

dm_study <- create_DecisionModel(config_OllamaDecision("clef-flash"), output_schema = study)
msg <- generate(dm_study, abstract)
msg@content          # {"design":"cohort","significant":false}
```

An `enum` value carries no meaning beyond its label, so a `choice()` with
defined options is usually the more accurate way to ask the same thing.
`llmapply()` and `agentapply()` record the wall time of each call as `dmapply()`
does, so a decision model and an LLM can be compared on the same items.

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
