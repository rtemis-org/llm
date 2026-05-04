[![CRAN status](https://www.r-pkg.org/badges/version/rtemis.llm)](https://CRAN.R-project.org/package=rtemis.llm)
[![rtemis.llm status badge](https://rtemis-org.r-universe.dev/rtemis.llm/badges/version)](https://rtemis-org.r-universe.dev/rtemis.llm)
[![R CI](https://github.com/rtemis-org/llm/actions/workflows/r-ci-r2u.yml/badge.svg)](https://github.com/rtemis-org/llm/actions/workflows/r-ci-r2u.yml)
[![R-Docs](https://img.shields.io/badge/docs-rtemis.org/r-blue)](https://docs.rtemis.org/r/llm/)

# rtemis llm

- **Functional user-facing API** to create agents and generate responses
- **Type-checked and validated backend** with support for **Ollama**, **OpenAI**-compatible, and **Anthropic**-compatible API endpoints
- Support for **structured output**, **tool calling**, **agent memory**

## R package installation

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

```{r}
pak::repo_add(myuniverse = "https://rtemis-org.r-universe.dev")
pak::pak("rtemis.llm")
```

## GitHub

```{r}
pak::pak("rtemis-org/llm")
```
