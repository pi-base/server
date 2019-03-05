import * as A from '../../actions'
import * as React from 'react'

import { Branch, BranchState, Dispatch } from '../../types'

import { State } from '../../reducers'
import Submit from './Submit'
import { bindActionCreators } from 'redux';
import { by } from '../../utils'
import { connect } from 'react-redux'

type DispatchProps = {
  changeBranch: (branch: Branch) => void
  submitBranch: (branch: Branch) => void
}

export const Row: React.SFC<{ branch: BranchState } & DispatchProps> = ({ branch, changeBranch, submitBranch }) => {
  return (
    <tr>
      <td>{branch.name}</td>
      <td>
        {branch.active
          ? <span className="btn-group">
              <button className="btn btn-info btn-sm">Current</button> 
              <Submit branch={branch} submitBranch={submitBranch} />
            </span>
          : <button className="btn btn-default btn-sm branch-change" onClick={() => changeBranch(branch.name)}>
            Switch
          </button>
        }
      </td>
    </tr>
  )
}

type Props = { branches: BranchState[] } & DispatchProps

export const Table: React.SFC<Props> = ({ branches, changeBranch, submitBranch }) => {
  if (branches.length === 0) { return null }

  return (
    <table className="table table-condensed table-hover">
      <thead>
        <tr>
          <th>Branch Name</th>
          <th>Actions</th>
        </tr>
      </thead>
      <tbody>
        {branches.map(branch => <Row key={branch.name} branch={branch} changeBranch={changeBranch} submitBranch={submitBranch} />)}
      </tbody>
    </table>
  )
}

const mapStateToProps = (state: State) => {
  if (state.user === 'unauthenticated') {
    return { branches: [] }
  }

  const branches = Array.from(state.version.branches.values()).sort(by('name'))
  return {
    branches: branches.map((branch: any) => ({
      ...branch,
      active: branch.name === state.version.active
    }))
  }
}

const mapDispatchToProps = (dispatch: Dispatch) => bindActionCreators({
  changeBranch: A.changeBranch,
  submitBranch: A.submitBranch
}, dispatch)

// TODO:
// * submitBranch should probably be connected directly to Branch.Submit
// * should changeBranch be connected to Row?
export default connect(
  mapStateToProps,
  mapDispatchToProps
)(Table)
