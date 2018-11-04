import * as React from 'react'
import classNames from 'classnames'

// FIXME - right now, it's possible to assign passthrough props to <Control>
//   which don't match the types that the wrapped component is expecting
export interface Props {
  input: any // FIXME: component type or string or ...
  label?: string
  name: string
  placeholder?: string
}

const Control = (props: Props & Partial<any>) => {
  const {
    input,
    label,
    name,
    errors,
    ...rest
  } = props

  const Component = input

  const className = classNames('form-group', { 'has-error': errors !== undefined })

  return (
    <div className={className}>
      {label &&
        <label htmlFor={name}>{label}</label>
      }

      <div>
        <Component
          {...rest}
          className="form-control"
          name={name}
        />
        {errors &&
          <span className="help-block">{errors}</span>
        }
      </div>
    </div>
  )
}

export default Control
