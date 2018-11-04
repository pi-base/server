import * as Q from '../../queries'
import * as R from '../../reducers'
import * as React from 'react'
import * as S from '../../selectors'
import { Property } from '../../types'

import { Finder } from '../../models/Finder'
import Suggestions from './Suggestions'
import { connect } from 'react-redux'
import { useReducer } from 'react'

export const TAB = 9, ENTER = 13, UP = 38, DOWN = 40

interface OwnProps {
  name: string
  value?: string
  onChange: React.ChangeEventHandler<HTMLInputElement>
  placeholder?: string
}

interface StateProps {
  properties: Finder<Property>
}

type Props = OwnProps & StateProps

interface State {
  value: string
  suggestions: Property[]
  selected: number
  showSuggestions: boolean
}

type Action =
  { type: 'UPDATE_TEXT', value: string } |
  { type: 'HIDE_SUGGESTIONS' } |
  { type: 'EXPAND_SUGGESTION', suggestion: Property } |
  { type: 'CHANGE_SELECTION', delta: number }

const buildReducer = (props: Props) =>
  (state: State, action: Action): State => {
    switch (action.type) {
      case 'UPDATE_TEXT':
        return updateFormula(props, state, action.value)
      case 'HIDE_SUGGESTIONS':
        return collapseDropdown(state)
      case 'EXPAND_SUGGESTION':
        const expanded = Q.replaceFragment(state.value, action.suggestion.name)
        return collapseDropdown(updateFormula(props, state, expanded))
      case 'CHANGE_SELECTION':
        const suggestions = state.suggestions.length
        const selected = suggestions === 0 ? 0 : (state.selected + action.delta) % suggestions
        return { ...state, selected }
      default:
        return state
    }
  }

const updateFormula = (
  props: Props,
  state: State,
  value: string
): State => {
  const suggestions = Q.suggestionsFor(props.properties, value, 10)

  return {
    ...state,
    value,
    suggestions,
    selected: 0,
    showSuggestions: !!value && value[0] !== ':'
  }
}

const collapseDropdown = (state: State): State => ({
  ...state,
  suggestions: [],
  selected: 0,
  showSuggestions: false
})

export const Input: React.SFC<Props> = (props: Props) => {
  const { placeholder, onChange, name } = props

  const initial: State = {
    value: props.value || '',
    suggestions: [],
    selected: 0,
    showSuggestions: false
  }

  const reducer = buildReducer(props)

  const [state, dispatch] = useReducer(reducer, initial)

  const { suggestions, selected, showSuggestions } = state

  const handleKeyDown: React.KeyboardEventHandler<HTMLInputElement> = e => {
    switch (e.which) {
      case UP:
        e.preventDefault()
        return dispatch({ type: 'CHANGE_SELECTION', delta: -1 })
      case DOWN:
        e.preventDefault()
        return dispatch({ type: 'CHANGE_SELECTION', delta: 1 })
      case ENTER:
      case TAB:
        // TODO: we don't fire an onChange event here, so e.g. the URL doesn't
        //   update when the user accepts a suggestion
        if (showSuggestions) {
          e.preventDefault()
          dispatch({ type: 'EXPAND_SUGGESTION', suggestion: suggestions[selected] })
        }
        return
      default:
        return
    }
  }

  const handleChange: React.ChangeEventHandler<HTMLInputElement> = e => {
    if (e.target.value !== state.value) {
      onChange(e)
      dispatch({ type: 'UPDATE_TEXT', value: e.target.value })
    }
  }

  return (
    <div>
      <input
        type="text"
        autoComplete="off"
        className="form-control"
        name={name}
        value={state.value || props.value}
        placeholder={placeholder}
        onKeyDown={handleKeyDown}
        onChange={handleChange}
        onBlur={() => dispatch({ type: 'HIDE_SUGGESTIONS' })}
      />

      {showSuggestions
        ? <Suggestions
          suggestions={suggestions}
          selected={selected}
          limit={10}
          onSelect={suggestion => dispatch({ type: 'EXPAND_SUGGESTION', suggestion })}
        />
        : null
      }
    </div>
  )
}

const mapStateToProps = (state: R.State) => ({
  properties: S.propertyFinder(state)
})

export default connect(
  mapStateToProps
)(Input)
