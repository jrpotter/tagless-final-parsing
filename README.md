# tagless-final-parsing

This repository contains collections of code snippets related to my
[Tagless Final Parsing](https://jrpotter.github.io/posts/tagless-final-parsing/)
blog post.

> In his [introductory text](https://okmij.org/ftp/tagless-final/course/lecture.pdf),
Oleg Kiselyov discusses the **tagless final** strategy for implementing DSLs.
The approach permits leveraging the strong typing provided by some host language
in a performant way. This post combines key thoughts from a selection of papers
and code written on the topic. We conclude with an implementation of an
interpreter for a toy language that runs quickly, remains strongly-typed, and
can be extended without modification.

## Experimenting

This repository uses Nix for reproducible builds. First
[install Nix](https://nixos.org/download.html) if you do not currently have it
on your system. Afterward, enable [flakes](https://nixos.wiki/wiki/Flakes) by
adding line

```
experimental-features = nix-command flakes
```

to `$HOME/.config/nix/nix.conf`. You may then use `nix build` and `nix develop`.
To makes things easier, we recommend using [Home Manager](https://github.com/nix-community/home-manager)
to install [direnv](https://github.com/direnv/direnv) and [nix-direnv](https://github.com/nix-community/nix-direnv).
Once you run

```bash
$ direnv allow
```

from the root directory, `nix develop` will be automatically invoked each time
a change is detected in `flake.nix` or you return to the directory.

## Formatting

Link in `.githooks` by running:

```bash
$ git config --local core.hooksPath .githooks/
```
