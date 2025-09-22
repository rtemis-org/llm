# kaimana R package

Kaimana is a package to interface with Large Language Models, part of the [rtemis](https://rtemis.org) ecosystem.

## Installation

```r
remotes::install_github("egenn/kaimana-r")
```

## Usage

Load the package

```r
library(kaimana)
```

List available Ollama models

```r
ollama_list_models()
```

Chat with a model

```r
ollama_chat("granite3.2:8b", "Explain quantum superposition to a 5 year old.")
```
