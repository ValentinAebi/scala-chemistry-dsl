import dsl.*
import units.*

@main def chained(): Unit = {

  val first =
    equation ~ "K2C2O4" + "KMnO4" + "H2O" --> "CO2" + "Mn(OH)2" + "KOH" as reaction:
      reactants:
        25.g of "K2C2O4"
  end first

  val `CO2` = first.asSuccess.amountOfProduct("CO2")

  val second = equation ~ "CO2" + "NH3" --> "CO(NH2)2" + "H2O" as reaction:
    reactants:
      `CO2` of "CO2"
  end second

  val `CO(NH2)2` = second.asSuccess.amountOfProduct("CO(NH2)2")
  val mass = `CO(NH2)2` * "CO(NH2)2".mass
  println(s"$mass of CO(NH2)2")
  
}
