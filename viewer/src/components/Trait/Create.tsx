import * as React from 'react'
import * as S from '../../selectors'

import { Dispatch, Id, Property, Space, Trait } from '../../types'
import { Fields as EditFields, Values as EditValues } from './Edit'
import { assertTrait, checkProofs } from '../../actions'

import { Field } from '../Form'
import Form from './Form'
import { FormikErrors } from 'formik'
import { RouteComponentProps } from 'react-router'
import { State } from '../../reducers'
import { by } from '../../utils'
import { connect } from 'react-redux'
import uuid from 'uuid/v4'

type Values = {
  uid: string
  propertyId: string
  value: string
  deduced: boolean
} & EditValues

type OwnProps = {
  space: Space
} & RouteComponentProps<{}>

type StateProps = {
  findProperty: (propertyId: Id) => Property | undefined
  unknownProperties: Property[]
}
type DispatchProps = {
  onSubmit: (trait: Trait) => void
}
type Props = OwnProps & StateProps & DispatchProps

const PropertySelect = (unknownProperties: Property[]) => props => (
  <select {...props}>
    <option />
    {unknownProperties.sort(by('name')).map(p => (
      <option key={p.uid} value={p.uid}>{p.name}</option>
    ))}
  </select>
)

const ValueSelect = props => (
  <select {...props}>
    <option key="true" value="true">True</option>
    <option key="false" value="false">False</option>
  </select>
)

const makeFields = ({ unknownProperties }) => () => (
  <>
    <Field
      name="propertyId"
      label="Property"
      input={PropertySelect(unknownProperties)}
    />
    <Field name="value" label="Value" input={ValueSelect} />
    <EditFields />
  </>
)

const Create = ({
  space,
  onSubmit,
  findProperty,
  unknownProperties
}: Props) => {
  const Fields = makeFields({ unknownProperties })

  const initialValues = {
    uid: uuid(),
    propertyId: '',
    value: 'true',
    description: '',
    references: [],
    deduced: false
  }

  const validate = (values: Values) => {
    let errors: FormikErrors<Values> = {}

    // TODO: verify that propertyId, value are in select?
    if (!values.description) {
      errors.description = 'Description is required'
    }

    const property = findProperty(values.propertyId)

    if (!property) {
      errors.description = 'Could not find property'
      return { errors }
    }

    const result: Trait = {
      ...values,
      space,
      property,
      value: values.value === 'true',
    }

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

const mapStateToProps = (state: State, ownProps: OwnProps): StateProps => ({
  findProperty: (propertyId: Id) => state.properties.get(propertyId),
  unknownProperties: S.unknownProperties(state, ownProps.space)
})

const mapDispatchToProps = (
  dispatch: Dispatch,
  { history }: OwnProps
) => ({
  onSubmit: (result: Trait) => {
    dispatch(assertTrait(result)).then(trait => {
      dispatch(checkProofs({ trait }))
      history.push(`/spaces/${trait.space.uid}/properties/${trait.property.uid}`)
    })
  }
})

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(Create)