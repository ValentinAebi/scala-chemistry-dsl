import chemistry.Molecule
import dsl.*
import units.*

@main def main(): Unit = {

  title("Motivating example from report")
  equation ~ "C6H12O6" + "O2" --> "H2O" + "CO2" as reaction:
    reactants:
      200.g of "C6H12O6"
    efficiency == 60.percent
    PRINT

  title("Silver sulfadiazine")
  // problem 5.2.1.4 in https://chem.libretexts.org/Courses/Oregon_Tech_PortlandMetro_Campus/OT_-_PDX_-_Metro%3A_General_Chemistry_I/05%3A_Transformations_of_Matter/5.02%3A_Reaction_Stoichiometry/5.2.01%3A_Practice_Problems-_Reaction_Stoichiometry
  equation ~ "C10H10N4SO2" + "Ag2O" --> "AgC10H9N4SO2" + "H2O" as reaction:
    products:
      25.g of "AgC10H9N4SO2"
    PRINT

  title("Combustion of propane")
  // example 5 in https://chem.libretexts.org/Bookshelves/Inorganic_Chemistry/Supplemental_Modules_and_Websites_(Inorganic_Chemistry)/Chemical_Reactions/Stoichiometry_and_Balancing_Reactions
  equation ~ "C3H8" + "O2" --> "H2O" + "CO2" as reaction:
    reactants:
      200.g of "C3H8"
    PRINT

  title("Production of urea")
  // problem 5.2.1.7 in https://chem.libretexts.org/Courses/Oregon_Tech_PortlandMetro_Campus/OT_-_PDX_-_Metro%3A_General_Chemistry_I/05%3A_Transformations_of_Matter/5.02%3A_Reaction_Stoichiometry/5.2.01%3A_Practice_Problems-_Reaction_Stoichiometry
  val carbonCombustionResult = equation ~ "C" + "O2" --> "CO2" as reaction:
    reactants:
      1e3.g of "C"
  val molCO2 = carbonCombustionResult.asSuccess amountOfProduct "CO2"
  equation ~ "CO2" + "NH3" --> "CO(NH2)2" + "H2O" as reaction:
    reactants:
      molCO2 of "CO2"
    PRINT

  title("Silver extraction")
  // problem 5.2.1.3 from https://chem.libretexts.org/Courses/Oregon_Tech_PortlandMetro_Campus/OT_-_PDX_-_Metro%3A_General_Chemistry_I/05%3A_Transformations_of_Matter/5.02%3A_Reaction_Stoichiometry/5.2.01%3A_Practice_Problems-_Reaction_Stoichiometry
  equation ~ "KAg(CN)2" + "Zn" --> "Ag" + "Zn(CN)2" + "KCN" as reaction:
    reactants:
      37.27.g of "KAg(CN)2"
    PRINT

  title("Redox")
  // question 1 in https://chemistry-worksheets.s3.amazonaws.com/chem-1-vol-4/Chemistry+1+Tutor+-+Vol+4+-+Worksheet+28+-+Redox+Stoichiometry+-+Part+2.pdf
  equation ~ "OH^-" + "IO3^-" + "CrO2^-" --> "I^-" + "CrO4^2-" + "H2O" as reaction:
    reactants:
      0.06.mol of "IO3^-"
    PRINT

}

private def title(title: String): Unit = {
  println(s"\n---- $title ".padTo(50, '-'))
}

