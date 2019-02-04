import * as React from 'react'
import * as S from '../../selectors'

import { List } from 'immutable'

import { Property, Space, Theorem } from '../../types'
import { State } from '../../reducers'

import Implication from '../Implication'
import { Link } from 'react-router-dom'
import TraitTable from '../Trait/Table'
import { connect } from 'react-redux'
import { converse } from '../../logic'

type OwnProps = {
  theorem: Theorem
}
type StateProps = {
  properties: Property[]
  converse: {
    counterexamples: Space[]
    proof: List<Theorem> | 'tautology' | false | undefined
  }
}
type Props = OwnProps & StateProps

const Counterexamples = ({ theorem, properties, converse: { counterexamples, proof } }: Props) => {
  // TODO: check DB for recorded converses

  if (counterexamples.length > 0) {
    return (
      <aside>
        <p>This implication does not reverse, as shown by</p>
        <TraitTable
          spaces={counterexamples}
          properties={properties}
        />
      </aside>
    )
  }

  if (proof) {
    if (proof === 'tautology') { return <span /> }

    return (
      <aside>
        <p>The converse also holds</p>
        <table className="table table-condensed">
          <thead>
            <tr>
              <th><Implication theorem={converse(theorem)} link={false} /></th>
              <th>By</th>
            </tr>
          </thead>
          <tbody>
            {proof.toSet().toList().map(thrm => (
              <tr key={thrm!.uid}>
                <td>
                  <Implication theorem={thrm!} link={false} />
                </td>
                <td>
                  <Link to={`/theorems/${thrm!.uid}`}>{thrm!.uid}</Link>
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </aside>
    )
  }

  return <aside>No examples found disproving the converse.</aside>
}

const mapStateToProps = (
  state: State,
  { theorem }: OwnProps
): StateProps => {
  const reverse = converse(theorem)
  const counterexamples = S.counterexamples(state, reverse) || []
  const proof = counterexamples.length == 0 && S.prover(state).prove(reverse)
  const properties = S.theoremProperties(state, theorem)

  return { properties, converse: { counterexamples, proof } }
}

export default connect(
  mapStateToProps
)(Counterexamples)
