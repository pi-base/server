import { Formula, and, negate } from '../models/Formula'
import { Id, Theorem } from '../types'
import { Proof, disprove } from '../logic'

import { Table } from '../models/Table'

export class Prover {
  traits: Table<Id, Id, boolean>
  theorems: Theorem[]

  constructor(traits: Table<Id, Id, boolean>, theorems: Theorem[]) {
    this.traits = traits
    this.theorems = theorems
  }

  disprove(formula: Formula<Id>): Proof | undefined {
    return disprove(this.theorems, formula)
  }

  prove(theorem: Theorem): Proof | undefined {
    return disprove(this.theorems, and(
      theorem.if,
      negate(theorem.then)
    ))
  }
}