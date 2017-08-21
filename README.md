# Git + Haskell: git-hell

A command-line utility/library for git interaction.


## Building and Running with Stack

```bash
stack build && stack exec hsh
```

## Build the Executable using Stack within Docker

```bash
$ docker run -v ~/.stack:/root/.stack -v ~/.local/bin:/root/.local/bin -v /path/to/git-hell:/git-hell -it --rm haskell:8.0.1 /bin/bash
$ cd /git-hell
$ stack config set system-ghc --global true
$ stack install --allow-different-user
```
