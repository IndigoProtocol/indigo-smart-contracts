# Indigo

## Making CI happy

Formatting:
```
ormolu --mode inplace $(git ls-files '*.hs')
```

Lint:
```
hlint $(git ls-files '*.hs')
```

## Benchmark reports

To get a presentable benchmarking report in a YAML like format run the following:
```
cabal run indigo-tests -- --benchmark -- --xml=benchmark-result.xml; cat benchmark-result.xml | cabal -v0 run format-benchmarks > benchmark-results.yaml
```
It is necessary that all the benchmark test succeed and only the limits testing fail.

When no test-options are passed, the suite runs all the tests by default:
```
cabal test indigo-tests
```
Otherwise, you need to specify the suite and separate by `--` to pass `tasty` arguments:
```
cabal test indigo-tests --test-options "--test -- -p \"CDP\""
```

## Prerequisite

Make sure `nix` is installed.

Subsequent instructions assume you are inside the dev env shell.
To get there do `nix develop` (if this doesn't work and your nix is older then do `nix-shell`).

## Build

```sh
cabal build
```

## Development

We can utilize a repl for rapid development.

``` sh
cabal repl
:l Indigo/Contracts
```

To reload the code after making changes, type `:r` into the repl. Most of the interaction can be tested through `EmulatorTrace`. For example, load `Interactions.hs` and run `runGov` to test the Governance contract.

## Test

``` sh
cabal test
```

## Flags

There are two Cabal flags that control the optimization level:

* `plutonomy` controls if the on-chain code should be automatically optimized with Plutonomy, while
* `debug` additionally configures Plutonomy to emit the trace checks and strings.

Both flags default to true; to turn them off use Cabal options `-f-debug` and `-f-plutonomy`.

## Updating
On a newer `plutus-apps` release you can do the following:
1. Copy new `plutus-apps` revision and dependencies from its `cabal.project` to `cabal-haskell.nix.project`.
2. Update `sha256map` in `flake.nix`: add/remove new dependency urls, edit revisions used and change hashes. To get a new hash use `nix-prefetch-git`.

For example, for this dependency:
```
source-repository-package
   type: git
   location: https://github.com/mlabs-haskell/plutus-simple-model
   tag: 30f492e6e11be719aa0ba5203ab961ae578aaaaf
```

Run a command `nix-prefetch-git https://github.com/mlabs-haskell/plutus-simple-model 30f492e6e11be719aa0ba5203ab961ae578aaaaf`:

```git revision is 30f492e6e11be719aa0ba5203ab961ae578aaaaf
path is /nix/store/axd90ncjj2a99m05695kl7yj3xyvyc5q-plutus-simple-model-30f492e
git human-readable version is -- none --
Commit date is 2022-07-05 13:33:55 +0300
hash is 1kcnj24ap0a9r8l8mwmbbqlv5gk7xf9l5vm0b6xab0ihwd3kf206
{
  "url": "https://github.com/mlabs-haskell/plutus-simple-model",
  "rev": "30f492e6e11be719aa0ba5203ab961ae578aaaaf",
  "date": "2022-07-05T13:33:55+03:00",
  "path": "/nix/store/axd90ncjj2a99m05695kl7yj3xyvyc5q-plutus-simple-model-30f492e",
  "sha256": "1kcnj24ap0a9r8l8mwmbbqlv5gk7xf9l5vm0b6xab0ihwd3kf206",
  "fetchLFS": false,
  "fetchSubmodules": false,
  "deepClone": false,
  "leaveDotGit": false
}
```

Edit `sha256map` entry to this:

```"https://github.com/mlabs-haskell/plutus-simple-model"."30f492e6e11be719aa0ba5203ab961ae578aaaaf" = "1kcnj24ap0a9r8l8mwmbbqlv5gk7xf9l5vm0b6xab0ihwd3kf206".```

## Bug Bounty

A bug bounty program for Indigo Protocol's smart contracts is ongoing and has been running since April 17th, 2023. Indigo Labs looks forward to working with the security community to find vulnerabilities in order to keep the users of the protocol safe.

Rewards will be managed by Indigo Labs as part of its services to the Foundation. Rewards will be decided by Labs until the Indigo DAO adopts a more detailed rewards structure in the near future. The quality of the report and reproduction instructions can impact the reward. Rewards are denominated and paid out in INDY.

The scope of the bug bountry program is all smart contract components of the Indigo Protocol. You can find the repository here: https://github.com/IndigoProtocol/indigo-smart-contracts

Any frontend applications, client-side code, and mismatching spec documents are outside of the scope of this bug bounty program.

For this initial bug bounty program, sponsored by Indigo Labs, there is a maximum bounty pool of 50,000 INDY.

### Reporting a Vulnerability

Please responsibly disclose any findings to Indigo Labs, following these instructions:

- In order to report a vulnerability, please write an email to security@indigo-labs.io with [SECURITY DISCLOSURE] in the subject of the email.
- For sensitive vulnerabilities, please the encrypt the email using this [PGP key](https://keys.mailvelope.com/pks/lookup?op=get&search=security@indigo-labs.io).
- We will make our best effort to reply in a timely manner and provide a timeline for resolution.
- Please include a detailed report on the vulnerability with clear reproduction steps. The quality of the report can impact the reward amount.
- Only unknown vulnerabilities will be awarded a bounty; in case of duplicate reports, the first report will be awarded the bounty.
- Public disclosure of the vulnerability, before explicit consent from Indigo Labs to do so, will make the vulnerability ineligible for a bounty.
- Attempting to exploit the vulnerability in a public Cardano network will also make it ineligible for a bounty.

## Licensing

The primary license for Indigo Protocol V1 Core is the Business Source License 1.1 (`BUSL-1.1`), see [`LICENSE`](./LICENSE).