import * as React from 'react'
import * as S from '../../selectors'

import { Route, RouteComponentProps, Switch } from 'react-router'
import { Space, Trait } from '../../types'
import { State } from '../../reducers'

import NotFound from '../NotFound'
import Title from '../Title'
import { connect } from 'react-redux'

import Edit from './Edit'
import Show from './Show'

type OwnProps = {
  space: Space
} & RouteComponentProps<{ propertyId: string }>
type StateProps = {
  trait: Trait | undefined
}
type Props = OwnProps & StateProps & RouteComponentProps<{ propertyId: string }>

const Component: React.SFC<Props> = props => {
  const { trait } = props

  if (!trait) { return <NotFound {...props} /> }

  // TODO: extract this to a helper
  const title = `${trait.space.name}: ${trait.property.name}`

  return (
    <div>
      <Title title={trait.space.name} />
      <Switch>
        <Route
          path={props.match.url + '/edit'}
          render={ps => <Edit {...ps} trait={trait} />}
        />
        <Route
          path={props.match.url}
          render={ps => <Show {...ps} trait={trait} />}
        />
      </Switch>
    </div>
  )
}

const mapStateToProps = (state: State, props: OwnProps): StateProps => ({
  trait: S.getTrait(state, props.space, props.match.params.propertyId)
})

export default connect(
  mapStateToProps
)(Component)
