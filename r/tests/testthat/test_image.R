# test_image.R
# ::rtemis.llm::
# 2026- EDG rtemis.org
# spec: llm/image-input

fixture <- function(...) testthat::test_path("fixtures", ...)

red_png <- fixture("red.png")
blue_jpg <- fixture("blue.jpg")
b64 <- function(path) {
  openssl::base64_encode(readBin(path, what = "raw", n = file.size(path)))
}

# One system and one user message, the user message carrying `image_path`.
image_state <- function(content = "What color is this?", image_path = red_png) {
  state <- InProcessAgentMemory()
  append_message(
    state,
    SystemMessage(content = "System prompt."),
    echo = FALSE,
    verbosity = 0L
  )
  append_message(
    state,
    InputMessage(content = content, image_path = image_path),
    echo = FALSE,
    verbosity = 0L
  )
  state
}

openai_test_config <- function() {
  config_OpenAI(
    model_name = "gpt-test",
    base_url = "http://localhost:1234/v1",
    api_key = "test-key",
    validate_model = FALSE
  )
}

anthropic_test_config <- function() {
  config_Anthropic(
    model_name = "claude-test",
    api_key = "test-key",
    validate_model = FALSE
  )
}


# %% .image_media_type ----
test_that("media type is read from the file signature", {
  expect_identical(.image_media_type(red_png), "image/png")
  expect_identical(.image_media_type(blue_jpg), "image/jpeg")
  expect_identical(.image_media_type(fixture("pixel.gif")), "image/gif")
  expect_identical(.image_media_type(fixture("pixel.webp")), "image/webp")
  # The extension does not decide.
  expect_identical(.image_media_type(fixture("red_png_named.jpg")), "image/png")
  expect_identical(.image_media_type(fixture("text.png")), NA_character_)
})


# %% InputMessage with images ----
test_that("InputMessage encodes each image and stores absolute paths", {
  msg <- InputMessage(content = "Compare.", image_path = c(red_png, blue_jpg))
  expect_identical(msg@image_path, normalizePath(c(red_png, blue_jpg)))
  expect_length(msg@images, 2L)
  expect_identical(msg@images[[1L]][["media_type"]], "image/png")
  expect_identical(msg@images[[2L]][["media_type"]], "image/jpeg")
  expect_identical(msg@images[[1L]][["data"]], b64(red_png))
  expect_false(grepl("\n", msg@images[[1L]][["data"]], fixed = TRUE))
})

test_that("InputMessage without images has no image fields", {
  msg <- InputMessage(content = "Hi")
  expect_null(msg@image_path)
  expect_null(msg@images)
})

test_that("InputMessage rejects missing, unsupported and malformed paths", {
  expect_error(
    InputMessage(content = "x", image_path = fixture("missing.png")),
    "Image file not found"
  )
  expect_error(
    InputMessage(content = "x", image_path = fixture("text.png")),
    "Not a supported image file"
  )
  expect_error(
    InputMessage(content = "x", image_path = fixture()),
    "Image file not found"
  )
  expect_error(
    InputMessage(content = "x", image_path = character()),
    "character vector of file paths"
  )
  expect_error(
    InputMessage(content = "x", image_path = 1),
    "character vector of file paths"
  )
  expect_error(
    InputMessage(content = "x", image_path = c(red_png, NA)),
    "character vector of file paths"
  )
})

test_that("InputMessage prints its image paths", {
  msg <- InputMessage(content = "Compare.", image_path = c(red_png, blue_jpg))
  out <- repr(msg, output_type = "plain")
  expect_match(out, "Images:")
  expect_match(out, "red.png", fixed = TRUE)
  expect_match(out, "blue.jpg", fixed = TRUE)
})


# %% Ollama wire ----
test_that("Ollama messages carry raw base64 in `images`", {
  messages <- get_message_list(image_state(image_path = c(red_png, blue_jpg)))
  user <- messages[[2L]]
  expect_identical(user[["content"]], "What color is this?")
  expect_identical(user[["images"]], list(b64(red_png), b64(blue_jpg)))
  expect_false("image_path" %in% names(user))
  expect_null(messages[[1L]][["images"]])
})

test_that("A single Ollama image serializes as a JSON array", {
  messages <- get_message_list(image_state())
  json <- jsonlite::toJSON(messages[[2L]], auto_unbox = TRUE)
  expect_match(json, '"images":["', fixed = TRUE)
})


# %% OpenAI wire ----
test_that("OpenAI user content is text then image_url parts", {
  body <- build_chat_request_body(
    openai_test_config(),
    state = image_state(image_path = c(red_png, blue_jpg))
  )
  content <- body[["messages"]][[2L]][["content"]]
  expect_length(content, 3L)
  expect_identical(content[[1L]], list(type = "text", text = "What color is this?"))
  expect_identical(content[[2L]][["type"]], "image_url")
  expect_identical(
    content[[2L]][["image_url"]][["url"]],
    paste0("data:image/png;base64,", b64(red_png))
  )
  expect_match(content[[3L]][["image_url"]][["url"]], "^data:image/jpeg;base64,")
})

test_that("OpenAI sends images alone for an empty prompt", {
  body <- build_chat_request_body(
    openai_test_config(),
    state = image_state(content = "")
  )
  content <- body[["messages"]][[2L]][["content"]]
  expect_length(content, 1L)
  expect_identical(content[[1L]][["type"]], "image_url")
})

test_that("OpenAI text-only content stays a string", {
  state <- InProcessAgentMemory()
  append_message(
    state,
    InputMessage(content = "Hi"),
    echo = FALSE,
    verbosity = 0L
  )
  body <- build_chat_request_body(openai_test_config(), state = state)
  expect_identical(body[["messages"]][[1L]][["content"]], "Hi")
})

test_that("Apple sends images as OpenAI image_url parts", {
  config <- config_Apple(validate_model = FALSE)
  body <- build_chat_request_body(config, state = image_state())
  content <- body[["messages"]][[2L]][["content"]]
  expect_identical(content[[2L]][["type"]], "image_url")
  expect_match(content[[2L]][["image_url"]][["url"]], "^data:image/png;base64,")
})


# %% Anthropic wire ----
test_that("Anthropic user content is image blocks then text", {
  body <- build_chat_request_body(
    anthropic_test_config(),
    state = image_state(image_path = c(red_png, blue_jpg))
  )
  content <- body[["messages"]][[1L]][["content"]]
  expect_length(content, 3L)
  expect_identical(
    content[[1L]],
    list(
      type = "image",
      source = list(
        type = "base64",
        media_type = "image/png",
        data = b64(red_png)
      )
    )
  )
  expect_identical(content[[2L]][["source"]][["media_type"]], "image/jpeg")
  expect_identical(content[[3L]], list(type = "text", text = "What color is this?"))
})

test_that("Anthropic sends images alone for an empty prompt", {
  body <- build_chat_request_body(
    anthropic_test_config(),
    state = image_state(content = "")
  )
  content <- body[["messages"]][[1L]][["content"]]
  expect_length(content, 1L)
  expect_identical(content[[1L]][["type"]], "image")
})

test_that("Images in memory are resent on later turns", {
  state <- image_state()
  append_message(
    state,
    LLMMessage(content = "Red.", model_name = "claude-test"),
    echo = FALSE,
    verbosity = 0L
  )
  append_message(
    state,
    InputMessage(content = "Are you sure?"),
    echo = FALSE,
    verbosity = 0L
  )
  body <- build_chat_request_body(anthropic_test_config(), state = state)
  expect_identical(body[["messages"]][[1L]][["content"]][[1L]][["type"]], "image")
  expect_identical(
    body[["messages"]][[3L]][["content"]],
    list(list(type = "text", text = "Are you sure?"))
  )
})


# %% generate() checks images before the request ----
test_that("generate() fails on a missing image before any request", {
  llm <- create_OpenAI(
    model_name = "gpt-test",
    base_url = "http://localhost:1/v1",
    api_key = "test-key",
    validate_model = FALSE
  )
  expect_error(
    generate(llm, "Describe.", image_path = fixture("missing.png"), verbosity = 0L),
    "Image file not found"
  )
})


# %% .batch_image_path ----
test_that(".batch_image_path splits, recycles and checks", {
  expect_null(.batch_image_path(NULL, 3L))
  expect_identical(
    .batch_image_path(c(red_png, blue_jpg), 2L),
    list(red_png, blue_jpg)
  )
  expect_identical(.batch_image_path(red_png, 3L), rep(list(red_png), 3L))
  both <- list(c(red_png, blue_jpg), NULL)
  expect_identical(.batch_image_path(both, 2L), both)
  expect_error(
    .batch_image_path(c(red_png, blue_jpg), 3L),
    "length 1 or the length of `x` \\(3\\)"
  )
  expect_error(
    .batch_image_path(c(red_png, fixture("missing.png")), 2L),
    "Image file not found"
  )
})


# %% llmapply with images ----
# Backend-free LLM that answers with the prompt and the names of its images,
# and counts its calls, so the batch path is tested without a server.
ImageStubLLM <- new_class(
  "ImageStubLLM",
  parent = LLM,
  properties = list(calls = class_environment),
  constructor = function() {
    calls <- new.env()
    calls[["n"]] <- 0L
    new_object(LLM(system_prompt = "stub"), calls = calls)
  }
)

method(get_model_name, ImageStubLLM) <- function(x) "stub-model"

method(generate, ImageStubLLM) <- function(
  x,
  prompt,
  temperature = NULL,
  top_p = NULL,
  max_tokens = NULL,
  stop = NULL,
  image_path = NULL,
  think = NULL,
  output_schema = NULL,
  verbosity = 1L,
  validate_output = TRUE,
  on_validation_failure = c("warn", "collect", "abort"),
  ...
) {
  x@calls[["n"]] <- x@calls[["n"]] + 1L
  msg <- InputMessage(content = prompt, image_path = image_path)
  LLMMessage(
    content = paste(c(prompt, basename(msg@image_path %||% character())), collapse = " "),
    model_name = "stub-model"
  )
}

test_that("llmapply sends one image with each prompt", {
  out <- llmapply(
    c("a", "b"),
    ImageStubLLM(),
    image_path = c(red_png, blue_jpg),
    verbosity = 0L
  )
  expect_identical(as.vector(out), c("a red.png", "b blue.jpg"))
})

test_that("llmapply recycles one prompt over many images", {
  images <- c(first = red_png, second = blue_jpg)
  out <- llmapply("Describe", ImageStubLLM(), image_path = images, verbosity = 0L)
  expect_identical(as.vector(out), c("Describe red.png", "Describe blue.jpg"))
  expect_identical(names(out), c("first", "second"))
})

test_that("llmapply sends several images per prompt from a list", {
  out <- llmapply(
    c("a", "b"),
    ImageStubLLM(),
    image_path = list(c(red_png, blue_jpg), NULL),
    verbosity = 0L
  )
  expect_identical(as.vector(out), c("a red.png blue.jpg", "b"))
})

test_that("llmapply checks every image before the first call", {
  llm <- ImageStubLLM()
  expect_error(
    llmapply(
      c("a", "b"),
      llm,
      image_path = c(red_png, fixture("missing.png")),
      verbosity = 0L
    ),
    "Image file not found"
  )
  expect_identical(llm@calls[["n"]], 0L)
})


# %% Live: Ollama ----
ollama_vision_model <- "gemma4:e2b"

test_that("Ollama vision model sees the image", {
  skip_if_ollama_model_missing(ollama_vision_model)
  llm <- create_Ollama(ollama_vision_model, temperature = 0)
  out <- generate(
    llm,
    "What color is this image? Answer with one word.",
    image_path = red_png,
    think = FALSE,
    verbosity = 0L
  )
  expect_match(tolower(out@content), "red")
})

test_that("Ollama Agent sees the image", {
  skip_if_ollama_model_missing(ollama_vision_model)
  agent <- create_agent(
    config_Ollama(ollama_vision_model, temperature = 0),
    verbosity = 0L
  )
  out <- generate(
    agent,
    "What color is this image? Answer with one word.",
    image_path = blue_jpg,
    think = FALSE,
    verbosity = 0L
  )
  expect_match(tolower(responses(out[[length(out)]])), "blue")
})


# %% Live: Apple ----
test_that("Apple Foundation Model sees the image", {
  skip_if_apple_unavailable()
  llm <- create_Apple(temperature = 0)
  out <- generate(
    llm,
    "What color is this image? Answer with one word.",
    image_path = red_png,
    verbosity = 0L
  )
  expect_match(tolower(out@content), "red")
})
