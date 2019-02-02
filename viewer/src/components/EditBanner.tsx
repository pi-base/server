import * as React from 'react'
import * as S from '../selectors'

import Alert from './Alert'
import { Branch, BranchAccess } from '../types'
import { State } from '../reducers'
import { connect } from 'react-redux'
import { MASTER } from '../constants'
import { Link } from 'react-router-dom'

interface StateProps {
  branch: Branch
  access: BranchAccess
}

type Props = StateProps

const EditBanner = ({ branch, access }: Props) => {
  if (!branch || branch === MASTER) { return null }

  return (
    <div className="container">
      <Alert type="info">
        <p>
          You are currently {S.canEdit(access) ? 'editing' : 'reading'} {branch}.
          Click <Link to="/user">here</Link> to manage branches.
        </p>
      </Alert>
    </div>
  )
}

const mapStateToProps = (state: State): StateProps => {
  const branch = S.activeBranch(state)

  return {
    branch,
    access: S.branchAccess(state, branch)
  }
}

export default connect(
  mapStateToProps
)(EditBanner)