import * as React from 'react'
import * as F from 'formik'

import { Citation, Dispatch, Theorem } from '../../types'
import { RouteComponentProps } from 'react-router'
import { updateTheorem } from '../../actions'

import Citations from '../Form/Citations'
import { Field } from '../Form'
import Form from './Form'
import { connect } from 'react-redux'

export type Values = {
  description: string
  references: Citation[]
}

interface DispatchProps {
  onSubmit: (theorem: Theorem) => any
}
interface OwnProps {
  theorem: Theorem
}
type Props = OwnProps & DispatchProps

export const Fields = () => (
  <>
    <Field
      name="description"
      label="Description"
      input="textarea"
    />
    <Field
      name="references"
      input={Citations}
    />
  </>
)

const Edit = ({
  theorem,
  onSubmit
}: Props) => {
  const initialValues = {
    description: theorem.description,
    references: theorem.references
  }

  const validate = (values: Values) => {
    let errors: F.FormikErrors<Values> = {}

    if (theorem.description && !values.description) {
      errors.description = 'Description is required'
    }

    const result: Theorem = { ...theorem, ...values }

    return { result, errors }
  }

  return (
    <Form<Values>
      Fields={Fields}
      initialValues={initialValues}
      validate={validate}
      onSubmit={onSubmit}
    />
  )
}

const mapDispatchToProps = (
  dispatch: Dispatch,
  { history }: RouteComponentProps<{}>
): DispatchProps => ({
  onSubmit: result => {
    dispatch(updateTheorem(result)).then(theorem => {
      history.push(`/theorems/${theorem.uid}`)
    })
  }
})

export default connect(
  null,
  mapDispatchToProps
)(Edit)