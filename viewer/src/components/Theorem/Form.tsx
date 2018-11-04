import * as F from 'formik'
import * as React from 'react'
import * as S from '../../selectors'
import { Formula } from '../../models/Formula'

import { Citation, Id, Property, Space, Theorem } from '../../types'
import { State } from '../../reducers'

import PreviewForm from '../PreviewForm'
import Preview from './Preview'
import { RouteComponentProps } from 'react-router'
import { connect } from 'react-redux'
import uuid from 'uuid/v4'

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
