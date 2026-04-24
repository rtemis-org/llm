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
response
```
