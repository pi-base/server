import * as React from 'react'

import Citations from '../Form/Citations'
import Detail from './Detail'
import { Field } from '../Form'
import PreviewForm from '../PreviewForm'
import { Property } from '../../types'
import uuid from 'uuid/v4'

interface Props {
  property?: Property
  onSubmit: (property: Property) => any
}

const validate = (values: Property) => {
  let errors: any = {}

  if (!values.name) {
    errors.name = 'Name is required'
  }

  return { result: values, errors }
}

const Preview = ({ result, ...props }) => (
  <Detail
    {...props}
    property={result}
    editable={false}
  />
)

const Fields = _ => (
  <>
    <Field
      name="name"
      label="Name"
      input="input"
    />
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

const Form: React.SFC<Props> = props => {
  const { property, onSubmit } = props

  const initial = {
    uid: uuid(),
    name: '',
    description: '',
    references: [],
    ...property || {}
  }

  return (
    <PreviewForm<Property, Property>
      Fields={Fields}
      Preview={Preview}
      initialValues={initial}
      validate={validate}
      onSubmit={onSubmit}
    />
  )
}

export default Form
