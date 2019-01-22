import * as F from 'formik'
import * as React from 'react'

import { Citation } from '../../types'
import { CitationType } from '../../types'
import Icon from '../Icon'
import { Map } from 'immutable'

const citationTypes: Map<CitationType, string> = Map([
  ['doi', 'DOI'],
  ['mr', 'Math Reviews'],
  ['wiki', 'Wikipedia']
])

const CitationDetail = ({ name, onRemove }: {
  name: string,
  onRemove: () => void
}) => {
  return (
    <div>
      <div className="form-group row">
        <div className="col-sm-2">
          <F.Field
            name={`${name}.type`}
            className="form-control"
            component="select"
          >
            <option />
            {citationTypes.map((label, key) => (
              <option key={key} value={key}>{label}</option>
            )).toList()}
          </F.Field>
        </div>
        <div className="col-sm-4">
          <F.Field
            name={`${name}.ref`}
            className="form-control"
            component="input"
            placeholder="Reference"
          />
        </div>
        <div className="col-sm-5">
          <F.Field
            name={`${name}.name`}
            className="form-control"
            component="input"
            placeholder="Description"
          />
        </div>
        <div className="col-sm-1">
          <button className="btn btn-default btn-xs" type="button" onClick={onRemove}>
            <Icon type="remove" />
          </button>
        </div>
      </div>
    </div>
  )
}

const Citations = (props: F.FieldProps['field']) => {
  const { name, value = [] } = props

  return (
    <F.FieldArray
      name={name}
      render={({ push, remove }: F.ArrayHelpers) => {
        return (
          <div className="form-group">
            <label>
              Citations
              {' '}
              <button
                type="button"
                className="btn btn-default btn-xs"
                onClick={() => {
                  const citation: Citation = { type: 'doi', ref: '', name: '' }
                  push(citation)
                }}
              >
                Add
              </button>
            </label>
            {
              value.map((citation: Citation, index: number) => (
                <CitationDetail key={index} name={`${name}.${index}`} onRemove={() => remove(index)} />
              ))
            }
          </div>
        )
      }}
    />
  )
}

export default Citations