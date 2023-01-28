<img src="airloom.jpg" alt="Logo: a detail of John William Waterhouse's paintinf of the Lady of Shalott, showing a loom." width=250 height=250 />

# Air Loom

**Air Loom** is a reverse literate programming tool: it takes code and documentation and weaves them together. It is both language-agnostic and markup language-agnostic.

## Example

## Conceptual Overview

Traditionally, there are two approaches to literate programming:

1. You write code inside your documentation, and a program extracts and concatenates the listings into source code. This worked in the 70's, but nowadays, you lose all the benefits of IDEs.
2. A newer approach is to write the documentation within the code as block comments, then a program strips the comment markers and turns everything that's not a comment into a code block. The problem with this approach is many languages require declarations to appear in a specific order, which is rarely the ideal reading order.

With Air Loom, you write your code in source files, retaining all the benefits of using IDEs, and use comment directives to define named chunks of code called _fragments_. Then, in your documentation, you use directives to include fragments. Air Loom then lifts the fragments out of the code and weaves them into the documentation. Code and documentation are entirely separated.

## Usage

The `lift` command extracts fragments from a set of files into a fragments file:

```bash
airloom lift lib.c hello.c test.c -o fragments.json
```

The `weave` command takes a number of documentation files and a fragments file, it weaves the fragments into the documentation and concatenates them together:

```bash
airloom weave intro.md examples.md api.md -f fragments.json -o docs.md
```

## Downloading

## Building

You need [stack][stack] installed. Then:

[stack]: https://docs.haskellstack.org/en/stable/

```bash
$ git clone https://github.com/eudoxia0/airloom.git
$ cd airloom
$ stack build
```

## License

Copyright (c) 2023 [Fernando Borretti](https://borretti.me/).

Licensed under the Apache 2.0 license.
