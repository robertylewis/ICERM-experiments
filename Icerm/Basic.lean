import Lean
open Lean Elab Tactic IO

def RunLs : TacticM Unit := do
  let output ← IO.Process.output {
    cmd := "python3",
    args := #["-c", "print(\"hello\")"],
    stdin := .null,
    stdout := .piped,
    stderr := .inherit
  }
  if output.exitCode == 0 then
    logInfo m!"Output of `ls`:\n{output.stdout}"
  else
    throwError
      "Command `ls` failed with code {output.exitCode}"


elab "run_ls" : tactic => RunLs

#print Process.output

example : True := by
  run_ls
  trivial
