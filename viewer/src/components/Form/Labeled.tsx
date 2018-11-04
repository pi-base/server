import * as F from 'formik'
import * as React from 'react'

export const Wrapped = props => {
  const Component = props.component
  return (
    <div className="form-group">
      <label htmlFor={props.name}>{props.label}</label>
      <div>
        <Component
          {...props}
          className="form-control"
        />
      </div>
    </div>
  )
}

interface Props {
  placeholder?: string
}

const Labeled = (Component, label: string) => (props: F.FieldProps & Props) => {
  const { placeholder, field, form } = props

  const showError = !!(form.errors[field.name] && form.touched[field.name])

  return (
    <div className={`form-group ${showError ? 'has-error' : ''}`}>
      <label htmlFor={field.name}>{label}</label>
      <div>
        <Component
          placeholder={placeholder || label}
          className="form-control"
          {...field}
        />
        <F.ErrorMessage
          name={field.name}
          component="span"
          className="help-block"
        />
      </div>
    </div>
  )
}

export default Labeled