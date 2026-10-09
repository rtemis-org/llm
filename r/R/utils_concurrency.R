# Bounded input tasks. Each worker executes the existing generate/decide path.
# spec: llm/parallel-batch#worker-execution

.map_call <- function(
  f,
  prompt,
  image_path,
  verbosity,
  extra,
  questions = NULL
) {
  start <- proc.time()[["elapsed"]]
  error <- NULL
  value <- tryCatch(
    if (is.null(questions)) {
      do.call(
        generate,
        c(
          list(
            x = f,
            prompt = prompt,
            image_path = image_path,
            verbosity = verbosity
          ),
          extra
        )
      )
    } else {
      decide(
        f,
        prompt,
        questions,
        image_path = image_path,
        verbosity = verbosity
      )
    },
    error = function(e) {
      error <<- e
      NULL
    }
  )
  list(value = value, error = error, elapsed = proc.time()[["elapsed"]] - start)
}

# Reconstruct the agent rather than copying its environment-backed history.
.independent_agent <- function(x, logfile) {
  Agent(
    llmconfig = x@llmconfig,
    state = InProcessAgentMemory(),
    system_prompt = x@system_prompt,
    use_memory = x@use_memory,
    tools = x@tools,
    max_tool_rounds = x@max_tool_rounds,
    output_schema = x@output_schema,
    name = x@name,
    allow_custom_tools = x@allow_custom_tools,
    logfile = logfile,
    verbosity = 0L
  )
}

.map_worker <- function(
  f,
  prompt,
  image_path,
  extra,
  questions,
  directory,
  index
) {
  previous <- options(rtemis.llm.batch_directory = directory)
  on.exit(options(previous), add = TRUE)
  if (S7_inherits(f, Agent)) {
    logfile <- file.path(directory, paste0("agent-", index, ".jsonl"))
    f <- .independent_agent(f, logfile)
    extra[["logfile"]] <- logfile
  }
  conditions <- list()
  result <- withCallingHandlers(
    .map_call(f, prompt, image_path, 0L, extra, questions),
    warning = function(w) {
      conditions[[length(conditions) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  result[["warnings"]] <- conditions
  result
}

.map_concurrent <- function(
  x,
  f,
  image_path,
  extra,
  questions,
  concurrency,
  collect,
  verbosity
) {
  check_dependencies("mirai")
  if (!is.null(getOption("rtemis.llm.batch_directory"))) {
    abort("Concurrent mapping cannot be nested inside a concurrent input task.")
  }
  count <- min(concurrency, length(x))
  if (count == 0L) {
    return(list())
  }
  log_destination <- if (S7_inherits(f, Agent)) {
    extra[["logfile"]] %||% f@logfile
  }
  if (!is.null(log_destination)) {
    check_character_scalar(log_destination, "logfile")
  }
  directory <- tempfile("rtemis-llm-batch-")
  dir.create(directory, mode = "0700")
  profile <- basename(directory)
  jobs <- list()
  flush_logs <- function(index = NULL) {
    pattern <- if (is.null(index)) {
      "^agent-.*[.]jsonl$"
    } else {
      paste0("^agent-", index, "[.]jsonl$")
    }
    for (path in list.files(directory, pattern = pattern, full.names = TRUE)) {
      cat(
        readLines(path, warn = FALSE),
        file = log_destination,
        sep = "\n",
        append = TRUE
      )
      unlink(path)
    }
  }
  on.exit(
    {
      for (job in jobs) {
        if (mirai::unresolved(job)) mirai::stop_mirai(job)
      }
      mirai::daemons(0L, .compute = profile)
      flush_logs()
      unlink(directory, recursive = TRUE)
    },
    add = TRUE
  )
  mirai::daemons(count, .compute = profile)
  # Source checkouts use pkgload in both processes; installed packages use
  # the caller's library path. Configuration objects are sent after loading.
  package_path <- getNamespaceInfo(asNamespace("rtemis.llm"), "path")
  library_paths <- .libPaths()
  working_directory <- getwd()
  startup <- mirai::everywhere(
    {
      .libPaths(library_paths)
      setwd(working_directory)
      if (file.exists(file.path(package_path, "Meta", "package.rds"))) {
        loadNamespace("rtemis.llm", lib.loc = dirname(package_path))
      } else {
        pkgload::load_all(package_path, quiet = TRUE)
      }
      TRUE
    },
    library_paths = library_paths,
    working_directory = working_directory,
    package_path = package_path,
    .compute = profile
  )
  for (job in startup) {
    if (!isTRUE(job[])) {
      abort("Could not load rtemis.llm in a concurrent worker.")
    }
  }
  # Never serialize the template's accumulated conversation to workers.
  if (S7_inherits(f, Agent)) {
    f <- .independent_agent(f, f@logfile)
  }
  progress <- progress_begin(
    length(x),
    label = repr_bracket(get_model_name(f)),
    kind = "llm_map",
    verbosity = verbosity
  )
  completed <- FALSE
  on.exit(
    progress_end(progress, status = if (completed) "done" else "error"),
    add = TRUE
  )
  out <- vector("list", length(x))
  next_index <- 1L
  while (next_index <= length(x) || length(jobs)) {
    while (next_index <= length(x) && length(jobs) < count) {
      index <- next_index
      task <- list(
        f = f,
        prompt = x[[index]],
        image_path = image_path[[index]],
        extra = extra,
        questions = questions,
        directory = directory,
        index = index
      )
      jobs[[as.character(index)]] <- mirai::mirai(
        do.call(get(".map_worker", asNamespace("rtemis.llm")), task),
        task = task,
        .compute = profile
      )
      next_index <- next_index + 1L
    }
    ready <- names(jobs)[!vapply(jobs, mirai::unresolved, logical(1L))]
    if (!length(ready)) {
      Sys.sleep(0.01)
      next
    }
    for (key in ready) {
      result <- jobs[[key]][["data"]]
      jobs[[key]] <- NULL
      flush_logs(key)
      if (mirai::is_error_value(result) || mirai::is_mirai_error(result)) {
        abort("Concurrent worker failed: ", as.character(result))
      }
      for (w in result[["warnings"]]) {
        warning(w)
      }
      out[as.integer(key)] <- list(collect(result, as.integer(key)))
      progress_update(progress)
    }
  }
  completed <- TRUE
  out
}

# Only cooldown deadlines are shared between workers. Each process owns one
# file and publishes it with an atomic rename; readers observe complete values.
.batch_cooldown <- function(directory) {
  paths <- list.files(directory, pattern = "^cooldown-", full.names = TRUE)
  max(c(0, vapply(paths, readRDS, numeric(1L))))
}

.publish_batch_cooldown <- function(directory, seconds) {
  target <- file.path(directory, paste0("cooldown-", Sys.getpid()))
  temporary <- tempfile("deadline-", tmpdir = directory)
  saveRDS(as.numeric(Sys.time()) + seconds, temporary)
  if (!file.rename(temporary, target)) {
    abort("Could not publish provider cooldown.")
  }
}

.batch_request <- function(
  req,
  directory,
  max_attempts = 3L,
  retry_seconds = 60
) {
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  deadline <- Inf
  response <- NULL
  for (attempt in seq_len(max_attempts)) {
    repeat {
      delay <- .batch_cooldown(directory) - as.numeric(Sys.time())
      if (delay <= 0) {
        break
      }
      if (as.numeric(Sys.time()) + delay >= deadline) {
        return(response)
      }
      Sys.sleep(min(delay, 0.1))
    }
    remaining <- deadline - as.numeric(Sys.time())
    if (remaining <= 0) {
      return(response)
    }
    request <- req
    if (is.finite(remaining)) {
      timeout <- req[["options"]][["timeout_ms"]] %||% Inf
      request <- httr2::req_timeout(req, min(remaining, timeout / 1000))
    }
    response <- httr2::req_perform(request)
    if (!httr2::resp_status(response) %in% c(429L, 503L)) {
      return(response)
    }
    if (!is.finite(deadline)) {
      deadline <- as.numeric(Sys.time()) + retry_seconds
    }
    delay <- suppressWarnings(httr2::resp_retry_after(response))
    if (length(delay) != 1L || !is.finite(delay) || delay < 0) {
      delay <- stats::runif(1L, 1, 2^attempt)
    }
    .publish_batch_cooldown(directory, delay)
    if (attempt == max_attempts || as.numeric(Sys.time()) + delay >= deadline) {
      return(response)
    }
  }
  response
}
