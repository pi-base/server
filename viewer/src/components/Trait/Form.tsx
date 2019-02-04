import * as F from 'formik'
import * as React from 'react'

import { Trait } from '../../types'

import PreviewForm from '../PreviewForm'
import Detail from './Detail'

const Preview = ({ result, ...props }) =>
  <Detail {...props} trait={result} />

interface Props<Values> {
  Fields: React.SFC<{}>
  initialValues: Values
  validate: (values: Values) => {
    result?: Trait
    errors: F.FormikErrors<Values>
  }
  onSubmit: (result: Trait) => any
}

function Form<Values>(props: Props<Values>) {
  const {
    Fields,
    initialValues,
    validate,
    onSubmit
  } = props

  return (
    <PreviewForm<Values, Trait>
      Preview={Preview}
      Fields={Fields}
      initialValues={initialValues}
      validate={validate}
      onSubmit={onSubmit}
    />
  )
}

export default Form
