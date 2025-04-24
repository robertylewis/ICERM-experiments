import Lean
import Mathlib.Tactic
open Lean Elab Command Syntax

elab "#formalize " s:str : command => do
  let input := s.getString
  -- logInfo m!"You said: {s.getString}"

  let output ← IO.Process.output {
    cmd := "ollama",
    args := #["run", "mistral", s!"Translate this informal statement to Lean code, please, with no extra text: {input}"],
    stdin := .null,
    stdout := .piped,
    stderr := .inherit
  }
  if output.exitCode == 0 then
    logInfo m!"Output:\n{output.stdout}"
  else
    throwError
      "Command failed with code {output.exitCode}"

-- #check ℕ

#formalize "every natural number is nonnegative"
