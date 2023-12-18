Exercise solutions from [Functional Programming in Scala, Second Edition](https://www.manning.com/books/functional-programming-in-scala-second-edition).

[![](https://github.com/asarkar/fp-scala/workflows/CI/badge.svg)](https://github.com/asarkar/fp-scala/actions)

Official GitHub repo: https://github.com/fpinscala/fpinscala

## Syllabus

### Part 1: Introduction to functional programming

1. What is functional programming?

2. Getting started with functional programming in Scala

3. Functional data structures

4. Handling errors without exception

5. Strictness and laziness

6. Purely functional state

## Executing a main method
```
./millw <module>.runMain --mainClass <fully-qualified main class>
```

## Executing tests
```
./.github/run.sh <chapter>
```

## VSCode

Open command palette: `Ctrl+Shift+P`

Open another window: `File > New Window > Open Recent/Open Folder`

## Mill

Install a BSP connection file:
```
mill mill.bsp.BSP/install
```

Then open VSCode command palette, and select `Metals: Switch build server`.
