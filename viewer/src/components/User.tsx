import * as React from 'react'

import Branches from './Branch/Table'
import { connect } from 'react-redux'

type StateProps = {
  user: {
    name: string
  } | undefined
}
type Props = StateProps

const UserDetail = ({ user }: Props) => {
  if (!user) {
    return (<div />)
  } else {
    return (
      <div>
        <h1>{user.name}</h1>
        <h2>Active Branches</h2>
        <Branches />
      </div>
    )
  }
}

export default connect(
  (state: any) => {
    if (state.user === 'unauthenticated') {
      return { user: undefined }
    } else {
      return { user: { name: state.user.name } }
    }
  }
)(UserDetail)
