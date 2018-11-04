import * as React from 'react'
import * as S from '../../selectors'

import { Property, Space, Theorem } from '../../types'

import Detail from './Detail'
import { State } from '../../reducers'
import TraitTable from '../Trait/Table'
import { connect } from 'react-redux'

interface OwnProps {
  result: Theorem
}
interface StateProps {
  properties: Property[]
  counterexamples: Space[]
}
type Props = OwnProps & StateProps

const Preview = (props: Props) => {
  const {
    result: theorem,
    properties,
    counterexamples
  } = props

  return (
    <article>
      <Detail {...props} theorem={theorem} />
      <hr />
      {counterexamples.length > 0
        ?
        <div>
          <p>Found counterexamples:</p>
          <TraitTable spaces={counterexamples} properties={properties} />
        </div>
        : <p>No couterexamples found</p>
      }
    </article>
  )
}

const mapStateToProps = (
  state: State,
  { result: theorem }: OwnProps
): StateProps => ({
  counterexamples: S.counterexamples(state, theorem),
  properties: S.theoremProperties(state, theorem)
})

export default connect(
  mapStateToProps
)(Preview)