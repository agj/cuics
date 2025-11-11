port := "4321"

[private]
default:
    just --list

# Build.
build: install generate-tw update-sc-texts
    rm -rf dist
    pnpm exec vite build --base ./

# Run development server.
develop: install generate-tw update-sc-texts qr
    pnpm exec vite --port {{port}} --clearScreen false --host

# Generates Tailwind modules.
generate-tw:
    pnpm exec elm-tailwind-modules --dir ./generated/elm-tailwind-modules

# Regenerates the simplified Chinese texts.
update-sc-texts:
    nu ./scripts/generate-chinese-simplified.nu

# Deploy on Github Pages.
deploy-ghpages: build
    pnpm exec gh-pages -d dist

[private]
install:
    pnpm install

[private]
qr:
    #!/usr/bin/env nu
    let ip = sys net | where name == "en0" | get 0.ip | where protocol == "ipv4" | get 0.address
    let url = $"http://($ip):{{port}}"
    qrtool encode -t ansi256 $url
    print $url
