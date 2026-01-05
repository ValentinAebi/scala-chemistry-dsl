# A Scala DSL for stoichiometric calculations

A DSL allowing to describe chemical equations and reactions, balance equations, and perform related computations.

## Example

```Scala
equation ~ "C6H12O6" + "O2" --> "H2O" + "CO2" as reaction:
    reactants:
      200.g of "C6H12O6"
    efficiency == 60.percent
    PRINT
```
displays
```
Computation succeeded:
  using reactants
    C6H12O6: 1.110 mol (200.000 g) of which 100.00% were used
    O2: 6.661 mol (213.149 g)
  according to C6H12O6 + 6 O2 --> 6 H2O + 6 CO2
  with an efficiency of 60.00%
  limiting reactant is C6H12O6
  the products are
    H2O: 3.997 mol (71.994 g)
    CO2: 3.997 mol (175.895 g)
```

## Note on using the DSL

Do not forget to import the `dsl` and `units` packages:
```Scala
import dsl.*
import units.*
```

## Syntax of molecules

`<molecule> ::= {{<atom>|(<molecule>)}[<coef>]}*[^<charge>{+|-}]`

Examples: `H2O`, `CO(NH2)2`, `CrO4^2-`

## Structure of this repository

Examples can be found [here](src/main/scala/main.scala) and [here](src/main/scala/report_chained_example.scala).

DSL implementation: [`dsl` package](src/main/scala/dsl)

Resolution engine: [`chemistry` package](src/main/scala/chemistry), [`math` package](src/main/scala/math)

Definitions of physical units: [`units` package](src/main/scala/units)
