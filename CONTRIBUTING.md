# Contributing to Cranelift

## Welcome!

Cranelift is a very ambitious project with many goals, and while we're
confident we can achieve some of them, we see many opportunities for people
to get involved and help us achieve even more.

### Ask questions! Give feedback!

This is a relatively young project, and not everything we hope to do with it
is reflected in the code or documentation yet. If you see things that seem
missing or that don't make sense, or even that just don't work the way you
expect them to, we're interested to hear about it!

We have a [Cranelift chat on Gitter], and questions are also welcome as issues
in the [Cranelift issue tracker].

[Cranelift chat on Gitter]: https://gitter.im/CraneStation/Lobby
[Cranelift issue tracker]: https://github.com/CraneStation/cranelift/issues/new

### Mentoring

We're happy to mentor people, whether you're learning Rust, learning about
compiler backends, learning about machine code, learning about how Cranelift
does things, or all together at once.

We tag issues in the issue tracker marked [good first issue] when we can, so
that's sometimes a good place to get started. Also, we encourage people to just
look around and find things they're interested in. This a good time to get
involved, as there aren't a lot of things set in stone yet.

[good first issue]: https://github.com/CraneStation/cranelift/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22

## Coding Guidelines

For the most part, Cranelift follows common Rust conventions and
[Pull Request] (PR) workflows, though we do have a few additional things to
be aware of.

[Pull Request]: https://help.github.com/articles/about-pull-requests/

### rustfmt

All PRs must be formatted according to the current stable [rustfmt], and this
is checked in the continuous integration tests. See the [rustfmt quickstart]
for setup.

[format-all.sh] is a script for running the appropriate version of rustfmt,
which may be convenient when there are multiple versions installed.

[rustfmt]: https://github.com/rust-lang-nursery/rustfmt
[rustfmt quickstart]: https://github.com/rust-lang-nursery/rustfmt#quick-start
[format-all.sh]: https://github.com/CraneStation/cranelift/blob/master/format-all.sh

### Rustc version support

Our current policy is to support the version of Rustc that ships with the
latest Ubuntu LTS release, as well as the current stable version. This means
we don't use some of the very latest released Rust features.

Some of the developer scripts depend on nightly Rust, for example to run
clippy and other tools, however we avoid depending on these for the main
build.

That said, if there are any new Rust features that would be particularly
valuable to use, please bring them up, as we may be able to find ways to
accommodate them.

### Python

Our Python code is checked with [mypy](http://mypy-lang.org/) and
[flake8](http://flake8.pycqa.org/en/latest/); see the
[check.sh](https://github.com/CraneStation/cranelift/blob/master/lib/codegen/meta-python/check.sh)
file for details. The versions available in common package repositories such
as Ubuntu or Homebrew typically work fine.
