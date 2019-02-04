import * as React from 'react'
import * as S from '../../selectors'

import { Dispatch, Id, Space, Theorem } from '../../types'
import { Fields as EditFields, Values as EditValues } from './Edit'
import { assertTheorem, checkProofs } from '../../actions'

import { Field } from '../Form'
import Form from './Form'
import { FormikErrors } from 'formik'
import { Formula } from '../../models/Formula'
import FormulaInput from '../Formula/Input'
import { RouteComponentProps } from 'react-router'
import { State } from '../../reducers'
import { connect } from 'react-redux'
import uuid from 'uuid/v4'

type Values = {
  if: string
  then: string
  description: string
} & EditValues

interface StateProps {
  parseFormula: (raw: string) => Formula<Id> | undefined
  counterexamples: (theorem: Theorem) => Space[]
}
interface DispatchProps {
  onSubmit: (theorem: Theorem) => void
}
type Props = StateProps & DispatchProps

export const Fields = () => (
  <>
    <Field
      name="if"
      label="If"
      placeholder="compact + t_2"
      input={FormulaInput}
    />
    <Field
      name="then"
      label="Then"
      placeholder="t_4"
      input={FormulaInput}
    />
    <EditFields />
  </>
)

const Create = ({
  onSubmit,
  parseFormula,
  counterexamples
}: Props) => {
  const initialValues = {
    if: '',
    then: '',
    description: '',
    references: []
  }

  const validate = (values: Values) => {
    let antecedent: Formula<Id> | undefined
    let consequent: Formula<Id> | undefined

    let errors: FormikErrors<Values> = {}
    if (values.if) {
      antecedent = parseFormula(values.if)
      if (!antecedent) {
        errors.if = 'Could not be parsed'
      }
    } else {
      errors.if = 'Required'
    }

    if (values.then) {
      consequent = parseFormula(values.then)
      if (!consequent) {
        errors.if = 'Could not be parsed'
      }
    } else {
      errors.then = 'Required'
    }

    if (Object.keys(errors).length > 0) {
      return { result: undefined, errors }
    }

    const result = {
      ...values,
      uid: uuid(),
      if: antecedent!,
      then: consequent!,
    }

    const cxs = counterexamples(result)
    if (cxs.length > 0) {
      errors.then = 'Counterexamples found'
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

const mapStateToProps = (
  state: State
): StateProps => ({
  counterexamples: theorem => S.counterexamples(state, theorem),
  parseFormula: text => S.parseFormula(state, text)
})

const mapDispatchToProps = (
  dispatch: Dispatch,
  { history }: RouteComponentProps<any>
): DispatchProps => ({
  onSubmit: result => {
    dispatch(assertTheorem(result)).
      then(theorem => {
        dispatch(checkProofs({ theorem }))
        history.push(`/theorems/${theorem.uid}`)
      })
  }
})

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(Create)