import * as React from 'react'
import * as S from '../../selectors'

import { Branch, BranchState } from '../../types'

type Props = {
  branch: BranchState
  submitBranch: (b: Branch) => void
}

const Submit: React.SFC<Props> = ({ branch, submitBranch }) => {
  const { access, submitting, pullRequestUrl } = branch

  if (!S.canSubmit(branch.name, access)) { return null }

  if (pullRequestUrl) {
    return (
      <a className="btn btn-success btn-sm" href={pullRequestUrl}>
        Review Status
      </a>
    )
  }

  return (
    <button
      className="btn btn-primary btn-sm branch-submit"
      onClick={() => submitBranch(branch.name)}
      disabled={submitting}
    >
      {submitting
        ? "Submitted"
        : "Submit for Review"
      }
    </button>
  )
}

export default Submit
