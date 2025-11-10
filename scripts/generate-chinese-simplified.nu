let textsFile = "./source/elm/Texts.elm"

let found = ^comby -match-only -match-newline-at-toplevel -lang .elm "chineseTraditional = {:[1]}" "" $textsFile
let traditional = $found
  | split row ":"
  | skip 2
  | str join ":"
  | str replace "chineseTraditional =" ""
  | str replace --all '\n' "\n"

comby -in-place -lang .elm "chineseSimplified = {:[1]}" $"chineseSimplified = ($traditional)" $textsFile
