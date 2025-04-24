import Lean
import Icerm.API
open Lean Elab Command IO Syntax

elab "#ask_claude " s:str : command => do
  let prompt := s.getString
  let msg : String :=
  "{\"model\": \"claude-3-haiku-20240307\", \"max_tokens\": 256, \"messages\": [{{\"role\": \"user\", \"content\": \"" ++ prompt ++ "\"}}]}"
  logInfo msg
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
    logInfo m!"response: {response}"
    let content :=
      match response.splitOn "\"text\":\"" with
      | _ :: after =>
        match after.head?.bind (fun s => s.splitOn "\"" |>.head?) with
        | some text => text.replace "\\n" "\n"
        | none => "Could not extract Claude's reply"
      | _ => "No Claude output found"
    logInfo m!"Claude says:\n{content}"
  else
    throwError "Claude command failed with exit code {output.exitCode}"

#ask_claude "are you there?"
