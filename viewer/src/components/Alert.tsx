import * as React from 'react'

const Alert = ({ type = 'info', children }) => (
  <div className={`alert alert-${type}`}>
    {children}
  </div>
)

export default Alert
