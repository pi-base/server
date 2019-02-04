import * as F from '../models/Formula'
import * as React from 'react'
import * as S from '../selectors'

import { Control, EditLink, FormulaInput } from './Form'
import { Property, SearchModifier } from '../types'
import { RouteComponentProps, withRouter } from 'react-router'

import { Finder } from '../models/Finder'
import { Formula } from '../models/Formula'
import _Results from './Search/Results'
import { State } from '../reducers'
import { connect } from 'react-redux'
import { useLast, useSearch } from '../hooks'
import Title from './Title'

export const Results = _Results

type StateProps = {
  properties: Finder<Property>
}
type Props = StateProps & RouteComponentProps<{}>

interface QueryState {
  text: string
  formula: string
}

type FormulaSearch = {
  formula: Formula<Property>
  modifier: SearchModifier
}

export const parseFormulaQuery = ({ properties, formula }: {
  properties: Finder<Property>
  formula: string
}): FormulaSearch | undefined => {
  let modifier: SearchModifier = 'true'
  if (formula.startsWith('!!')) {
    modifier = 'not_false'
    formula = formula.slice(2)
  } else if (formula.startsWith('!')) {
    modifier = 'false'
    formula = formula.slice(1)
  } else if (formula.startsWith('?')) {
    modifier = 'unknown'
    formula = formula.slice(1)
  }

  const parsed = F.parseWith(term => properties.find(term), formula)

  if (parsed) {
    return { modifier, formula: parsed }
  }
}

const Search = ({
  properties,
  location,
  history
}: Props) => {
  const [query, setQuery] = useSearch<QueryState>({ history, location })

  let search = useLast<FormulaSearch>(
    () => {
      if (query.formula) {
        return parseFormulaQuery({ properties, formula: query.formula || '' })
      } else {
        return null
      }
    }
  )

  // TODO: add widget to allow displaying extra traits inline
  return (
    <form className="search row" >
      <Title title="Search" />
      <div className="col-md-4">
        <Control
          label="Filter by Text"
          name="text"
          placeholder="plank"
          input="input"
          value={query.text || ''}
          onChange={e => setQuery({ text: e.target.value })}
        />
        <Control
          label="Filter by Formula"
          name="formula"
          input={FormulaInput}
          placeholder="compact + ~metrizable"
          value={query.formula || ''}
          onChange={e => setQuery({ formula: e.target.value })}
        />

        <EditLink to="/spaces/new" className="btn btn-default">
          New Space
        </EditLink>
      </div>

      <div className="col-md-8" data-test="search-results">
        <Results
          text={query.text}
          formula={search && search.formula}
          modifier={search && search.modifier}
          onExampleSelect={ex => setQuery({ text: '', formula: ex })}
          data-test="search-results"
        />
      </div>
    </form >
  )
}

const mapStateToProps = (state: State): StateProps => ({
  properties: S.propertyFinder(state)
})

export default withRouter(connect(
  mapStateToProps
)(Search))