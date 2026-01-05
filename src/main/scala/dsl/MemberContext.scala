package dsl

import chemistry.Molecule
import units.Mol

trait MemberContext {
  def saveAmount(molecule: Molecule, amount: Mol): Unit
}

final class ReactantsContext(ctx: Context) extends MemberContext {
  export ctx.saveReactantConstraint as saveAmount
}

final class ProductsContext(ctx: Context) extends MemberContext {
  export ctx.saveProductConstraint as saveAmount
}
