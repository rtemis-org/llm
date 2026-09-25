# utils_image.R
# Image input: file checks, encoding, and per-backend wire shapes.
# spec: llm/image-input

# %% .image_media_type ----
#' Detect an image's media type from its leading bytes
#'
#' The file's signature decides, not its extension, so a mislabeled file is
#' sent with the media type the provider will find when it decodes it.
#'
#' @param path Character: Path to an existing file.
#'
#' @return Character: One of "image/png", "image/jpeg", "image/gif",
#' "image/webp", or `NA_character_` for anything else.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.image_media_type <- function(path) {
  b <- readBin(path, what = "raw", n = 12L)
  starts_with <- function(sig, offset = 0L) {
    length(b) >= offset + length(sig) &&
      identical(b[offset + seq_along(sig)], sig)
  }
  if (starts_with(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))) {
    "image/png"
  } else if (starts_with(as.raw(c(0xff, 0xd8, 0xff)))) {
    "image/jpeg"
  } else if (
    starts_with(charToRaw("GIF87a")) || starts_with(charToRaw("GIF89a"))
  ) {
    "image/gif"
  } else if (
    starts_with(charToRaw("RIFF")) && starts_with(charToRaw("WEBP"), 8L)
  ) {
    "image/webp"
  } else {
    NA_character_
  }
}


# %% .check_image_file ----
#' Check that a path is a readable image in a supported format
#'
#' @param path Character: Path to check.
#'
#' @return Character: The image's media type.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.check_image_file <- function(path) {
  if (!file.exists(path) || dir.exists(path)) {
    abort("Image file not found: ", path)
  }
  if (file.access(path, mode = 4L) != 0L) {
    abort("Image file is not readable: ", path)
  }
  media_type <- .image_media_type(path)
  if (is.na(media_type)) {
    abort(
      "Not a supported image file: ",
      path,
      "\nSupported formats: PNG, JPEG, GIF, WebP."
    )
  }
  media_type
}


# %% .check_image_path ----
#' Check an `image_path` argument
#'
#' @param image_path Optional character: Paths to image files.
#'
#' @return NULL, invisibly. Aborts on the first path that is not a readable
#' image in a supported format.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.check_image_path <- function(image_path) {
  if (is.null(image_path)) {
    return(invisible(NULL))
  }
  if (
    !is.character(image_path) ||
      length(image_path) == 0L ||
      anyNA(image_path) ||
      !all(nzchar(image_path))
  ) {
    abort("`image_path` must be a character vector of file paths.")
  }
  for (path in image_path) {
    .check_image_file(path)
  }
  invisible(NULL)
}


# %% .read_images ----
#' Read and encode image files
#'
#' Images are encoded once, when the `InputMessage` is built, so a message in
#' agent memory carries the bytes that were sent and later turns do not depend
#' on the file still being there or unchanged.
#'
#' @param image_path Optional character: Paths to image files.
#'
#' @return NULL, or a list with one element per image, each a list of `path`
#' (absolute), `media_type`, and `data` (base64, no line breaks).
#'
#' @author EDG
#' @keywords internal
#' @noRd
.read_images <- function(image_path) {
  if (is.null(image_path)) {
    return(NULL)
  }
  .check_image_path(image_path)
  lapply(image_path, function(path) {
    list(
      path = normalizePath(path),
      media_type = .check_image_file(path),
      data = openssl::base64_encode(
        readBin(path, what = "raw", n = file.size(path))
      )
    )
  })
}


# %% .openai_image_parts ----
#' OpenAI Chat Completions image content parts
#'
#' @param images List: Output of `.read_images()`.
#'
#' @return List of `image_url` content parts carrying `data:` URIs.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.openai_image_parts <- function(images) {
  lapply(images, function(img) {
    list(
      type = "image_url",
      image_url = list(
        url = paste0("data:", img[["media_type"]], ";base64,", img[["data"]])
      )
    )
  })
}


# %% .anthropic_image_blocks ----
#' Anthropic Messages image content blocks
#'
#' @param images List: Output of `.read_images()`.
#'
#' @return List of `image` content blocks with base64 sources.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.anthropic_image_blocks <- function(images) {
  lapply(images, function(img) {
    list(
      type = "image",
      source = list(
        type = "base64",
        media_type = img[["media_type"]],
        data = img[["data"]]
      )
    )
  })
}


# %% .batch_image_path ----
#' Split a batch `image_path` into one entry per call
#'
#' @param image_path NULL, a character vector (one path per call, or one path
#' for every call), or a list of character vectors (several images per call;
#' `NULL` entries send none), each of length 1 or `n`.
#' @param n Integer: Number of calls in the batch.
#'
#' @return NULL, or a list of length `n`. Every path has been checked, so a
#' missing or unsupported file fails before the first request.
#'
#' @author EDG
#' @keywords internal
#' @noRd
.batch_image_path <- function(image_path, n) {
  if (is.null(image_path)) {
    return(NULL)
  }
  if (is.character(image_path)) {
    image_path <- as.list(image_path)
  }
  if (!is.list(image_path) || !length(image_path) %in% c(1L, n)) {
    abort(
      "`image_path` must have length 1 or the length of `x` (",
      n,
      ").\n",
      "To send several images with each prompt, pass a list of character vectors."
    )
  }
  for (paths in image_path) {
    .check_image_path(paths)
  }
  if (length(image_path) == 1L) {
    image_path <- rep(image_path, n)
  }
  image_path
}
