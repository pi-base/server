import * as React from 'react'

import { Branch } from '../../types'

type Props = {
  branch: Branch
  submitBranch: (b: Branch) => void
}

const Submit: React.SFC<Props> = ({ branch, submitBranch }) => {
  const { access, submitting, pullRequestUrl } = branch

  if (access !== 'admin') { return null }

  if (pullRequestUrl) {
    return <a href={pullRequestUrl}>View pull request</a>
  }

  return (
    <button
      className="btn btn-primary btn-sm branch-submit"
      onClick={() => submitBranch(branch)}
      disabled={submitting}
    >
      Submit for Review
    </button>
  )
}

export default Submit
