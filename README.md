# elm-package-insights

Detects broken Elm packages.

Example output:

```
Done!

Total packages: 1860
Broken packages: 169

Broken package list: [workdir]/.package-test/.outputs/broken-packages.txt
Details: [workdir]/.package-test/.outputs/broken-packages.json
```

Note: If the package registry problems become more complex, this tool will be transformed to a webapp and will run the scans periodically. Contact me if you think this needs to be done, I am open for discussions. For now it looks like running it manually is enough.

## Prerequisites

- Node.js (e.g. via [NVM](https://github.com/nvm-sh/nvm))
- elm (e.g. `npm install -g elm`)
- elm-json (e.g. `npm install -g elm-json`)
- Haskell Stack (e.g. via [GHCup](https://www.haskell.org/ghcup/))

## Usage

Checking all packages may take about 30m to complete.

For quick local re-run:

```sh
cd scanner
stack run scanner-exe
```

For longer, more comprehensive check:

```sh
cd scanner
stack run scanner-exe -- --reset-elm-packages-cache --reset-test-directories
```

### Flags

- `--reset-elm-packages-cache`
  Reset local elm packages cache (required for reliable test, will slow down the test a lot).
- `--reset-test-directories`
  Remove test directories before running tests (shouldn't affect the test much, and will slow down the test).
