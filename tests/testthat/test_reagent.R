# test_reagent.R
# ::kaimana::
# 2025 EDG rtemis.org

# %% Setup ----
model_name <- "ibm/granite3.3:8b"
system_prompt <- "You are a meticulous, expert clinician."

reagent <- create_ReAgent(
  model_name = model_name,
  system_prompt = system_prompt
)
reagent

repr(reagent)

# %% Test invoke ----
prompt <- "What is your name and who built you?"
res <- invoke(reagent, prompt, verbosity = 1L)

res_flight <- invoke(
  reagent,
  "Explain how planes are able to fly to a 3-year-old."
)
res_flight
