import chemistry.Molecule
import dsl.*
import units.*

@main def main(): Unit = {

  println("\n======= Reaction 1 =======")
  // problem 5.2.1.4 in https://chem.libretexts.org/Courses/Oregon_Tech_PortlandMetro_Campus/OT_-_PDX_-_Metro%3A_General_Chemistry_I/05%3A_Transformations_of_Matter/5.02%3A_Reaction_Stoichiometry/5.2.01%3A_Practice_Problems-_Reaction_Stoichiometry
  reaction of equation ~ "C10H10N4SO2" + "Ag2O" --> "AgC10H9N4SO2" + "H2O" and parameters:
    8.12.g of "Ag2O"
    2000.g of "C10H10N4SO2"
    efficiency == 100.percent
    compute and display

  println("\n======= Reaction 2 =======")
  // example 5 in https://chem.libretexts.org/Bookshelves/Inorganic_Chemistry/Supplemental_Modules_and_Websites_(Inorganic_Chemistry)/Chemical_Reactions/Stoichiometry_and_Balancing_Reactions
  reaction of equation ~ "C3H8" + "O2" --> "H2O" + "CO2" and parameters:
    200.g of "C3H8"
    nolimit on "O2"
    compute and display

  println("\n======= Reaction 3 =======")
  // problem 5.2.1.7 in https://chem.libretexts.org/Courses/Oregon_Tech_PortlandMetro_Campus/OT_-_PDX_-_Metro%3A_General_Chemistry_I/05%3A_Transformations_of_Matter/5.02%3A_Reaction_Stoichiometry/5.2.01%3A_Practice_Problems-_Reaction_Stoichiometry
  val carbonCombustionResult = reaction of equation ~ "C" + "O2" --> "CO2" and parameters:
    1_000_000.g of "C"
    nolimit on "O2"
    compute
  val molCO2 = carbonCombustionResult.asSuccess producedAmountOf "CO2"
  reaction of equation ~ "CO2" + "NH3" --> "CO(NH2)2" + "H2O" and parameters:
    molCO2 of "CO2"
    nolimit on "NH3"
    compute and display

  println("\n======= Reaction 4 =======")
  reaction of equation ~ "C6H12O6" + "O2" --> "H2O" + "CO2" and parameters:
    25.g of "C6H12O6"
    nolimit on "O2"
    efficiency == 50.percent
    compute and display

  println("\n======= Reaction 5 =======")
  // question 1 in https://chemistry-worksheets.s3.amazonaws.com/chem-1-vol-4/Chemistry+1+Tutor+-+Vol+4+-+Worksheet+28+-+Redox+Stoichiometry+-+Part+2.pdf
  reaction of equation ~ "OH^-" + "IO3^-" + "CrO2^-" --> "I^-" + "CrO4^2-" + "H2O" and parameters:
    nolimit on "CrO2^-"
    nolimit on "OH^-"
    0.06.mol of "IO3^-"
    compute and display

}

