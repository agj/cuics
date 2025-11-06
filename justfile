port := "4321"

[private]
default:
    just --list

# Build.
build: install
    rm -rf dist
    pnpm exec vite build --base ./

# Run development server.
develop: install qr
    pnpm exec vite --port {{port}} --clearScreen false --host

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
