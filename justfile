[private]
default:
    just --list

# Build.
build: install
  rm -rf dist
  pnpm exec vite build --base ./

# Run development server.
develop: install
  pnpm exec vite --clearScreen false

[private]
install:
    pnpm install
