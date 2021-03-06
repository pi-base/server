import * as React from 'react'
import { Space } from '../../types'

import { Route, RouteComponentProps, Switch } from 'react-router'

import Edit from './Edit'
import NotFound from '../NotFound'
import Show from './Show'
import { State } from '../../reducers'
import Title from '../Title'
import { connect } from 'react-redux'

type StateProps = {
  space: Space | undefined
}
type Props = StateProps & RouteComponentProps<{ spaceId: string }>

const Component: React.SFC<Props> = props => {
  const { space } = props

  if (!space) { return <NotFound {...props} /> }

  return (
    <div>
      <Title title={space.name} />
      <Switch>
        <Route path={props.match.url + '/edit'} render={ps => <Edit {...ps} space={space} />} />
        <Route path={props.match.url} render={ps => <Show {...ps} space={space} />} />
      </Switch>
    </div>
  )
}

export default connect(
  (state: State, props: Props) => ({
    space: state.spaces.get(props.match.params.spaceId)
  })
)(Component)