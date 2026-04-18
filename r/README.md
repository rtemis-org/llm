[![R CI](https://github.com/rtemis-org/llm/actions/workflows/r-ci-r2u.yml/badge.svg)](https://github.com/rtemis-org/llm/actions/workflows/r-ci-r2u.yml)

# rtemis.llm R package

rtemis.llm is a package to interface with Large Language Models, part of the [rtemis](https://rtemis.org) ecosystem.

## Installation

```r
pak::pak("rtemis-org/llm")
```

## Usage

Load the package

```r
library(rtemis.llm)
```

List available Ollama models

```r
ollama_list_models()
```

Create an `Agent`

```r
agent <- create_agent(
  llmconfig = config_Ollama(
    model_name = "gemma4:26b",
    temperature = 0.3,
  ),
  system_prompt = "You are a meticulous research assistant.",
  name = "Kaimana"
)
agent
```

**Output**
```
<Agent>
         Name: (Undefined)
System Prompt: You are a meticulous research assistant.
       Memory: Enabled
        Tools: (None)
   LLM Config:
               <OllamaConfig>
                model_name: gemma4:26b
               temperature: 0.30
                   backend: ollama
                  base_url: http://localhost:11434
 
```

Generate response

```r
response <- agent |> 
  generate("Explain quantum superposition in seven bullet points.")
response
```

<details>
<summary> <b>Output</b> (Click to expand)</summary>  
<br><br>

```
[Input]
Explain quantum superposition in seven bullet points. 
2026-04-17 23:48:19 [InProcessAgentMemory] Appended rtemis.llm::InputMessage from user [append_message]
2026-04-17 23:48:19 [gemma4:26b] working... [generate]
2026-04-17 23:48:55 [gemma4:26b] done. [generate]
[Reasoning]

*   Topic: Quantum superposition.
*   Format: Seven bullet points.
*   Persona: Meticulous research assistant (implies accuracy, clarity, and structured information).

    *   What is it? A fundamental principle of quantum mechanics.
    *   Core concept: A quantum system can exist in multiple states simultaneously.
    *   The "Measurement" problem: Observing/measuring the system causes "collapse" into a single state.
    *   Probability: It's not just "unknown," it's a mathematical combination of probabilities (wavefunction).
    *   Analogy: Schrödinger's Cat (classic, though often misunderstood, it's a standard teaching tool).
    *   Mathematical basis: Linear combination of basis states.
    *   Applications: Quantum computing (qubits), quantum cryptography.

    *   *Point 1: Definition.* Start with the core concept. A quantum system exists in all possible configurations at once.
    *   *Point 2: The Wavefunction.* Mention the mathematical representation ($\psi$). It's a superposition of states.
    *   *Point 3: Probability.* It's not random chance in the classical sense; it's a distribution of probabilities.
    *   *Point 4: Measurement/Collapse.* This is crucial. The act of observation forces the system into one definite state.
    *   *Point 5: Coherence vs. Decoherence.* To maintain superposition, the system must be isolated. Interaction with the environment causes decoherence.
    *   *Point 6: The Analogy (Schrödinger's Cat).* Use it to illustrate the concept of a system being both "dead and alive" until observed.
    *   *Point 7: Practical Application (Quantum Computing).* Why does this matter? Qubits can be 0 and 1 simultaneously, allowing for massive parallel processing.

    *   *Drafting Point 1:* Fundamental principle where a particle exists in multiple states simultaneously rather than one discrete state.
    *   *Drafting Point 2:* Mathematically described by a "wavefunction," which represents the probability amplitude of all possible outcomes.
    *   *Drafting Point 3:* Unlike classical bits (0 or 1), quantum bits (qubits) utilize superposition to represent both 0 and 1 at the same time.
    *   *Drafting Point 4:* The "Collapse of the Wavefunction" occurs when a measurement is made, forcing the system to settle into a single, definite state.
    *   *Drafting Point 5:* The concept is often illustrated by the Schrödinger’s Cat thought experiment, where a cat is theoretically both alive and dead until the box is opened.
    *   *Drafting Point 6:* Maintaining superposition requires "coherence," meaning the system must be shielded from external environmental interference (decoherence).
    *   *Drafting Point 7:* This principle is the engine behind quantum computing, enabling complex calculations by processing vast amounts of data in parallel.

    *   Seven points? Yes.
    *   Meticulous tone? Yes (precise terminology like "wavefunction," "coherence," "decoherence").
    *   Accurate? Yes.
[Response]
As a research assistant, I have synthesized the fundamental principles of quantum superposition into the following seven points:

*   **Simultaneous States:** At its core, quantum superposition is the principle that a quantum system—such as an electron or photon—can exist in multiple configurations or states simultaneously, rather than being restricted to a single, discrete state.
*   **The Wavefunction ($\psi$):** Mathematically, superposition is described by a "wavefunction," a mathematical function that encapsulates all possible states of a particle. This function represents the "probability amplitude" for every potential outcome of a measurement.
  
*   **Probability Distribution:** Superposition does not imply that a particle is "hidden" in a specific state; rather, it exists in a linear combination of all possible states. The likelihood of finding the particle in a specific state is determined by the square of the amplitude of its wavefunction.
*   **Wavefunction Collapse:** The act of measurement or observation fundamentally alters the system. When a quantum system is measured, the superposition "collapses," forcing the particle to transition from a multi-state existence into a single, definite state.
*   **Quantum Bit (Qubit) Advantage:** In classical computing, a bit is strictly a 0 or a 1. In quantum computing, a qubit utilizes superposition to represent 0, 1, or any mathematical combination of both simultaneously, allowing for massive parallel processing capabilities.
*   **The Role of Decoherence:** Maintaining superposition is extremely difficult because any interaction with the external environment (such as heat or electromagnetic radiation) can cause "decoherence." This is the process where the quantum system loses its superposition and begins to behave according to classical physics.
*   **Schrödinger’s Cat Analogy:** To illustrate the counterintuitive nature of this concept, physicist Erwin Schrödinger proposed a thought experiment involving a cat in a sealed box that is theoretically both "alive" and "dead" (in a superposition of states) until the box is opened and the system is observed. 
2026-04-17 23:48:55 [InProcessAgentMemory] Appended rtemis.llm::OllamaMessage from assistant [append_message]
```

</details>
