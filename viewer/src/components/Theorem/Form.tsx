import * as F from 'formik'
import * as React from 'react'

import { Theorem } from '../../types'

import PreviewForm from '../PreviewForm'
import Preview from './Preview'

interface Props<Values> {
  Fields: React.SFC<{}>
  initialValues: Values
  validate: (values: Values) => {
    result?: Theorem
    errors: F.FormikErrors<Values>
  }
  onSubmit: (result: Theorem) => any
}

function Form<Values>(props: Props<Values>) {
  const {
    Fields,
    initialValues,
    validate,
    onSubmit
  } = props

  return (
    <PreviewForm<Values, Theorem>
      Preview={Preview}
      Fields={Fields}
      initialValues={initialValues}
      validate={validate}
      onSubmit={onSubmit}
    />
  )
}

export default Form
