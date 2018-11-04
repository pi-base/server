import * as React from 'react'

import { RouteComponentProps, withRouter } from 'react-router'

import { Dispatch } from '../types'
import Footer from './Footer'
import Navbar from './Navbar'
import { State } from '../reducers'
import { boot } from '../actions'
import { connect } from 'react-redux'

type StateProps = {
  booted: boolean
  debug: boolean
}
type DispatchProps = {
  boot: () => void
}
type Props = StateProps & DispatchProps & RouteComponentProps<{}>

class Layout extends React.PureComponent<Props> {
  componentWillMount() {
    if (!this.props.booted) {
      this.props.boot()
    }
  }

  render() {
    const { booted, debug, location } = this.props

    return (
      <div>
        <Navbar />

        <div className="container">
          {booted || location.pathname === '/'
            ? this.props.children
            : 'Loading...'}
        </div>

        {debug ? <Footer /> : ''}
      </div>
    )
  }
}

const mapStateToProps = (state: State): StateProps => ({
  booted: state.spaces.size > 0,
  debug: state.debug
})

const mapDispatchToProps = (dispatch: Dispatch): DispatchProps => ({
  boot: () => dispatch(boot())
})

// `withRouter` is required so that location changes will trigger a re-render
// see https://github.com/ReactTraining/react-router/blob/master/packages/react-router/docs/guides/blocked-updates.md
export default withRouter(connect(
  mapStateToProps,
  mapDispatchToProps
)(Layout))
