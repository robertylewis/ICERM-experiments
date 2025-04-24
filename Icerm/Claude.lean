import Lean
import Icerm.API
import Mathlib.Tactic
open Lean Elab Command IO Syntax

elab "#ask_claude " s:str : command => do
  let input := s.getString
  let prompt := s!"Translate this informal statement to a Lean proposition based on mathlib. The response must begin with ```lean and end with ``` and should just be a type, without proof and without `theorem`: {input}"
  let msg : String :=
  "{\"model\": \"claude-3-haiku-20240307\", \"max_tokens\": 256, \"messages\": [{\"role\": \"user\", \"content\": \"" ++ prompt ++ "\"}]}"
  let payload := msg

  let output ← IO.Process.output {
    cmd := "curl",
    args := #[
      "https://api.anthropic.com/v1/messages",
      "-H", s!"x-api-key: {API_key}",
      "-H", "anthropic-version: 2023-06-01",
      "-H", "content-type: application/json",
      "-d", payload
    ],
    stdin := .null,
    stdout := .piped,
    stderr := .inherit
  }

  if output.exitCode == 0 then
    -- Rough way to extract Claude's response (not robust JSON parsing)
    let response := output.stdout
    let content :=
      match response.splitOn "\"text\":\"" with
      | _ :: after =>
        match after.head?.bind (fun s => s.splitOn "\"" |>.head?) with
        | some text => text.replace "\\n" "\n"
        | none => "Could not extract Claude's reply"
      | _ => "No Claude output found"
    let content := content.drop 8 |>.take (content.length - 8 - 4)
    logInfo m!"Claude says:\n{content}"

#ask_claude "every natural number is either even or odd"
