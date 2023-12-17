[Functional Programming in Scala, Second Edition](https://www.manning.com/books/functional-programming-in-scala-second-edition)

[![](https://github.com/asarkar/fp-scala/workflows/CI/badge.svg)](https://github.com/asarkar/fp-scala/actions)

Official GitHub repo: https://github.com/fpinscala/fpinscala

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


## References

### ScalaCheck Generators

* [ScalaCheck custom generator examples](https://alvinalexander.com/scala/scalacheck-custom-generator-examples/)

* [davidallsopp/PropertyTests.scala](https://gist.github.com/davidallsopp/60d7474a1fe8dc9b1f2d)

* [Property Based Testing: ScalaTest + ScalaCheck](https://medium.com/analytics-vidhya/property-based-testing-scalatest-scalacheck-52261a2b5c2c)

* [Generators in Detail](https://booksites.artima.com/scalacheck/examples/html/ch06.html)