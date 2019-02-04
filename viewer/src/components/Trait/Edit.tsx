import * as React from 'react'
import * as F from 'formik'

import { Citation, Dispatch, Trait } from '../../types'
import { updateTrait } from '../../actions'
import { RouteComponentProps } from 'react-router'

import Citations from '../Form/Citations'
import Form from './Form'
import { Field } from '../Form'
import { connect } from 'react-redux'

export type Values = {
  description: string
  references: Citation[]
}

type OwnProps = {
  trait: Trait
} & RouteComponentProps<{}>
type DispatchProps = {
  onSubmit: (trait: Trait) => void
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
  trait,
  onSubmit
}: Props) => {
  const initialValues = {
    description: trait.description,
    references: trait.references
  }

  const validate = (values: Values) => {
    let errors: F.FormikErrors<Values> = {}

    if (trait.description && !values.description) {
      errors.description = 'Description is required'
    }

    const result: Trait = { ...trait, ...values }

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
  { history }: OwnProps
): DispatchProps => ({
  onSubmit: result => {
    dispatch(updateTrait(result)).then(trait => {
      history.push(`/spaces/${trait.space.uid}/properties/${trait.property.uid}`)
    })
  }
})

export default connect(
  null,
  mapDispatchToProps
)(Edit)