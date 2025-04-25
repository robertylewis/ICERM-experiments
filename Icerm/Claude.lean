import Lean
import Mathlib.Tactic
open Lean Elab Command IO Syntax

def anthropicApiKey : IO String := do 
  match ← IO.getEnv "ANTHROPIC_API_KEY" with 
  | some key => return key
  | none => throw <| .userError "Environment variable ANTHROPIC_API_KEY not found"

elab "#ask_claude " s:str : command => do
  let input := s.getString
  let prompt := s!"Translate this informal statement to a Lean proposition based on mathlib. The response must begin with ```lean and end with ``` and should just be a type, without proof and without `theorem`: {input}" |> Json.escape
  let msg : String := Json.compress <| json% {
    model : "claude-3-haiku-20240307",
    max_tokens : 256,
    messages : [{
      role : "user",
      content : $prompt
    }]
  }
  let payload := msg

  let output ← IO.Process.output {
    cmd := "curl",
    args := #[
      "https://api.anthropic.com/v1/messages",
      "-H", s!"x-api-key: {← anthropicApiKey}",
      "-H", "anthropic-version: 2023-06-01",
      "-H", "content-type: application/json",
      "-d", payload
    ],
    stdin := .null,
    stdout := .piped,
    stderr := .inherit
  }

  unless output.exitCode == 0 do 
    throwError "Claude command failed with exit code {output.exitCode}"

  let response := output.stdout
  let response ← show CommandElabM Json from 
    match Json.parse response with
    | .ok j => return j
    | .error e => throwError "Failed to parse response\n{response}\nas json:\n{e}"
  let content ← show CommandElabM (Array Json) from 
    match response.getObjValAs? (Array Json) "content" with
    | .ok j => return j
    | .error e => throwError "Failed to obtain content from\n{response}\nas JSON array:\n{e}"
  let content ← show CommandElabM Json from 
    match content[0]? with 
    | some j => return j
    | none => throwError "Content\n{content}\nhas no elements"
  let text ← show CommandElabM String from 
    match content.getObjValAs? String "text" with
    | .ok j => return j
    | .error e => throwError "Content\n{content}\nhas no text:\n{e}"
  unless text.startsWith "```lean\n" && text.endsWith "```" do
    throwError "Claude gave us a response that was not just Lean code: \n\n {content}"
  let text := text.drop 8 |>.take (text.length - 8 - 4)
  liftTermElabM <|
    Meta.Tactic.TryThis.addSuggestion (←getRef) { suggestion := s!"#check {text}" }

#ask_claude "Let $R$ be a ring with $1$ and $M$ be a left $R$-module. Then $R^\\times$ and $M$ satisfy the two axioms for a group action of the multiplicative group $R^\\times$ on the set $M$, i.e., (1) $r_1 \\cdot (r_2 \\cdot m)= (r_1r_2)\\cdot m$, for all $r_1, r_2 \\in R^\\times$, $m \\in M$, and (2) $1 \\cdot m =m$ for all $m \\in M$."
