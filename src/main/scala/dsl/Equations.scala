package dsl

final case class NoCoefEquation(leftMember: NoCoefEquation.Member, rightMember: NoCoefEquation.Member)

object NoCoefEquation {
  
  final case class Member(molecules: List[NoChargeMolecule]) {
    def +(that: Member) = Member(this.molecules ++ that.molecules)
    
    def ===>(that: Member) = NoCoefEquation(this, that)
  }
  
}
