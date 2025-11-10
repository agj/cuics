## Generates simplified Chinese texts from the traditional Chinese texts.

let textsFile = "./source/elm/Texts.elm"

# Recover the traditional Chinese texts.
let found = ^comby -match-only -match-newline-at-toplevel -lang .elm "chineseTraditional = {:[1]}" "" $textsFile

# Parse only the record part.
let traditional = $found
  | split row ":"
  | skip 2
  | str join ":"
  | str replace "chineseTraditional =" ""
  | str replace --all '\n' "\n"

# Convert to simplified Chinese.
let simplified = $traditional
  | opencc -c ./scripts/opencc.json

# Update text in `Texts.elm` file.
comby -in-place -lang .elm "chineseSimplified = {:[1]}" $"chineseSimplified = ($simplified)" $textsFile

# Reformat the `.elm` file.
elm-format --yes $textsFile
