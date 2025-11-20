import chemistry.{BalancedEquation, Molecule, NoCoefEquation, Reaction}
import dsl.*
import units.*


object Main {

  def main(args: Array[String]): Unit = {

    // problem 5.2.1.4 in https://chem.libretexts.org/Courses/Oregon_Tech_PortlandMetro_Campus/OT_-_PDX_-_Metro%3A_General_Chemistry_I/05%3A_Transformations_of_Matter/5.02%3A_Reaction_Stoichiometry/5.2.01%3A_Practice_Problems-_Reaction_Stoichiometry
    reaction of equation ~ "C10H10N4SO2" + "Ag2O" --> "AgC10H9N4SO2" + "H2O" and parameters:
      8.12.g of "Ag2O"
      2000.g of "C10H10N4SO2"
      efficiency == 100.percent
      compute and display

    // example 5 in https://chem.libretexts.org/Bookshelves/Inorganic_Chemistry/Supplemental_Modules_and_Websites_(Inorganic_Chemistry)/Chemical_Reactions/Stoichiometry_and_Balancing_Reactions
    reaction of equation ~ "C3H8" + "O2" --> "H2O" + "CO2" and parameters:
      200.g of "C3H8"
      10_000_000.mol of "O2"
      compute and display

    reaction of equation ~ "C6H12O6" + "O2" --> "H2O" + "CO2" and parameters:
      2500.mol of "C6H12O6"
      25.g of "O2"
      efficiency == 50.percent
      compute and display

  }

  private def balanceAndDisplay(equation: NoCoefEquation): Unit = {
    println(BalancedEquation.balancedFrom(equation).getOrElse("equation could not be balanced"))
  }

  private def printMass(molecule: Molecule): Unit = {
    println(s"$molecule has mass ${molecule.mass}")
  }

}
