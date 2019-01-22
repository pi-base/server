import * as A from '../../actions'
import { State } from '../../reducers'
import * as React from 'react'

import { RouteComponentProps, withRouter } from 'react-router'

import { Dispatch } from '../../types'
import { Link } from 'react-router-dom'
import { connect } from 'react-redux'

type StateProps = {
  username: string | undefined
}
type DispatchProps = {
  startLogin: () => void
  loginUrl: () => string
  logout: () => void
}
type OwnProps = RouteComponentProps<{}>
type Props = StateProps & DispatchProps & OwnProps

const UserTab = ({ username, startLogin, loginUrl, logout }: Props) => {
  if (username) {
    return (
      <ul className="nav navbar-nav pull-right">
        <li>
          <Link to="/user">{username}</Link></li>
        <li>
          <a href="#" onClick={() => logout()}>Logout</a>
        </li>
      </ul>
    )
  } else {
    return (
      <ul className="nav navbar-nav pull-right">
        <li>
          <a href={loginUrl()} onClick={() => startLogin()}>
            Login with Github
          </a>
        </li>
      </ul>
    )
  }
}

const mapStateToProps = (state: State): StateProps => ({
  username: state.user === 'unauthenticated' ? undefined : state.user.name
})

const mapDispatchToProps = (dispatch: Dispatch): DispatchProps => ({
  loginUrl: () => dispatch(A.loginUrl(window)),
  startLogin: () => dispatch(A.startLogin(window)),
  logout: () => dispatch(A.logout())
})

export default withRouter(
  connect<StateProps, DispatchProps, OwnProps, any>(mapStateToProps, mapDispatchToProps)(UserTab)
)