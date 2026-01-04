import chemistry.Molecule
import dsl.*
import units.*

@main def main(): Unit = {

  // report example
  equation ~ "C6H12O6" + "O2" --> "H2O" + "CO2" in reaction:
    available:
      200.g of "C6H12O6"
    efficiency == 60.percent
    PRINT

  // problem 5.2.1.4 in https://chem.libretexts.org/Courses/Oregon_Tech_PortlandMetro_Campus/OT_-_PDX_-_Metro%3A_General_Chemistry_I/05%3A_Transformations_of_Matter/5.02%3A_Reaction_Stoichiometry/5.2.01%3A_Practice_Problems-_Reaction_Stoichiometry
  equation ~ "C10H10N4SO2" + "Ag2O" --> "AgC10H9N4SO2" + "H2O" in reaction:
    target:
      25.g of "AgC10H9N4SO2"
    PRINT


}

