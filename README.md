# Air Loom

**Air Loom** is a reverse literate programming tool: it takes code and documentation and weaves them together. It is both language-agnostic and markup language-agnostic.

## Conceptual Overview

Traditionally, there are two approaches to literate programming:

1. You write code inside your documentation, and a program extracts and concatenates the listings into source code. This worked in the 70's, but nowadays, you lose all the benefits of IDEs.
2. A newer approach is to write the documentation within the code as block comments, then a program strips the comment markers and turns everything that's not a comment into a code block. The problem with this approach is many languages require declarations to appear in a specific order, which is rarely the ideal reading order.

With Air Loom, you write your code in source files, retaining all the benefits of using IDEs, and use comment directives to define named chunks of code called _fragments_. Then, in your documentation, you use directives to include fragments. Air Loom then lifts the fragments out of the code and weaves them into the documentation. Code and documentation are entirely separated.

## Usage

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