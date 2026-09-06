# Credential resolution: a named key source that yields nothing is a
# configuration error, not a reason to call an API unauthenticated.

test_that("a literal api_key wins over every other source", {
  with_env(c(FAKE_KEY_ENV = "from-env"), {
    expect_identical(
      rtemis.llm:::.resolve_key_sources(
        api_key = "literal",
        api_key_env = "FAKE_KEY_ENV",
        keychain_service = NULL,
        default_env = "OPENAI_API_KEY"
      ),
      "literal"
    )
  })
})

test_that("a set environment variable resolves", {
  with_env(c(FAKE_KEY_ENV = "from-env"), {
    expect_identical(
      rtemis.llm:::.resolve_key_sources(
        api_key = NULL,
        api_key_env = "FAKE_KEY_ENV",
        keychain_service = NULL,
        default_env = "OPENAI_API_KEY"
      ),
      "from-env"
    )
  })
})

test_that("naming a non-default variable that is unset is an error", {
  # The failure this prevents: every call in a batch goes out without
  # credentials and comes back NA, long after the typo was made.
  with_env(c(FAKE_KEY_ENV = NA), {
    expect_error(
      rtemis.llm:::.resolve_key_sources(
        api_key = NULL,
        api_key_env = "FAKE_KEY_ENV",
        keychain_service = NULL,
        default_env = "OPENAI_API_KEY"
      ),
      "FAKE_KEY_ENV"
    )
  })
})

test_that("the backend default being unset is not an error", {
  # A local OpenAI-compatible server legitimately needs no key, so an unset
  # default variable is ambient rather than a stated intent.
  with_env(c(OPENAI_API_KEY = NA), {
    expect_null(
      rtemis.llm:::.resolve_key_sources(
        api_key = NULL,
        api_key_env = "OPENAI_API_KEY",
        keychain_service = NULL,
        default_env = "OPENAI_API_KEY"
      )
    )
  })
})

test_that("a named keychain service with no item is an error", {
  skip_if_no_keychain()
  expect_error(
    rtemis.llm:::.resolve_key_sources(
      api_key = NULL,
      api_key_env = "",
      keychain_service = "rtemis_llm_service_that_does_not_exist",
      default_env = "OPENAI_API_KEY"
    ),
    "Keychain"
  )
})

test_that("an unset variable does not mask a key the keychain holds", {
  # Both sources are tried before anything is reported.
  local_mocked_bindings(
    get_keychain_secret = function(service, ...) "from-keychain"
  )
  with_env(c(FAKE_KEY_ENV = NA), {
    expect_identical(
      rtemis.llm:::.resolve_key_sources(
        api_key = NULL,
        api_key_env = "FAKE_KEY_ENV",
        keychain_service = "some-service",
        default_env = "OPENAI_API_KEY"
      ),
      "from-keychain"
    )
  })
})

test_that("get_keychain_secret returns NULL for a missing item", {
  # Not character(0): a zero-length result is not NULL, so a caller guarding on
  # is.null() would read a failed lookup as a successful one and go on to send
  # an empty credential.
  skip_if_no_keychain()
  out <- rtemis.llm:::get_keychain_secret(
    service = "rtemis_llm_service_that_does_not_exist"
  )
  expect_null(out)
})

test_that("get_keychain_secret does not let the shell interpret a service name", {
  # `system2()` quotes the command but not its arguments, so an unquoted
  # service name would be evaluated rather than matched.
  skip_if_no_keychain()
  out <- rtemis.llm:::get_keychain_secret(
    service = "no_such; echo pwned"
  )
  expect_null(out)
})

test_that("a named keychain service outranks the ambient default variable", {
  # `api_key_env` always holds a name and cannot be blanked, so without this an
  # unrelated OPENAI_API_KEY in the environment would be sent to whatever
  # `base_url` points at -- a wrong-key 401 that looks like a bad endpoint.
  local_mocked_bindings(
    get_keychain_secret = function(service, ...) "from-keychain"
  )
  with_env(c(OPENAI_API_KEY = "ambient-openai-key"), {
    expect_identical(
      rtemis.llm:::.resolve_key_sources(
        api_key = NULL,
        api_key_env = "OPENAI_API_KEY",
        keychain_service = "OPENROUTER_API_KEY",
        default_env = "OPENAI_API_KEY"
      ),
      "from-keychain"
    )
  })
})

test_that("the default variable is still used when nothing else is named", {
  with_env(c(OPENAI_API_KEY = "ambient-openai-key"), {
    expect_identical(
      rtemis.llm:::.resolve_key_sources(
        api_key = NULL,
        api_key_env = "OPENAI_API_KEY",
        keychain_service = NULL,
        default_env = "OPENAI_API_KEY"
      ),
      "ambient-openai-key"
    )
  })
})
