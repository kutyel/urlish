# urlish

[![Actions Status](https://github.com/kutyel/urlish/workflows/Haskell%20CI/badge.svg)](https://github.com/kutyel/urlish/actions)
[![ormolu](https://img.shields.io/badge/styled%20with-ormolu-blueviolet)](https://github.com/tweag/ormolu)

🚀 An URI shortener made with Haskell and Redis!

## Run Nix!

```sh
$ nix-shell --pure shell.nix --run "cabal repl"
```

### Start Redis

```sh
$ redis-server /usr/local/etc/redis.conf
```

### Stop Redis

```sh
$ redis-cli shutdown
```
