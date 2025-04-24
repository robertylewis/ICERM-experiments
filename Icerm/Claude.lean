import Lean
import Icerm.API
import Mathlib.Tactic
open Lean Elab Command IO Syntax

elab "#ask_claude " s:str : command => do
  let input := s.getString
  let prompt := s!"Translate this informal statement to a Lean proposition based on mathlib. The response must begin with ```lean and end with ``` and should just be a type, without proof and without `theorem`: {input}" |> Json.escape
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
        | none => s!"Could not extract Claude's reply: {response}"
      | _ => "No Claude output found"
    if content.startsWith "```lean\n" && content.endsWith "```" then
      let content := content.drop 8 |>.take (content.length - 8 - 4)
    -- logInfo m!"Claude says:\n{content}"

      liftTermElabM <|
        Meta.Tactic.TryThis.addSuggestion (←getRef) { suggestion := s!"#check {content}" }
    else throwError "Claude gave us a response that was not just Lean code: \n\n {content}"

  else
    throwError "Claude command failed with exit code {output.exitCode}"


#ask_claude "Let $R$ be a ring with $1$ and $M$ be a left $R$-module. Then $R^\\times$ and $M$ satisfy the two axioms for a group action of the multiplicative group $R^\\times$ on the set $M$, i.e., (1) $r_1 \\cdot (r_2 \\cdot m)= (r_1r_2)\\cdot m$, for all $r_1, r_2 \\in R^\\times$, $m \\in M$, and (2) $1 \\cdot m =m$ for all $m \\in M$."
