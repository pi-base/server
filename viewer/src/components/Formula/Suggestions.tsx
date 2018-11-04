import * as React from 'react'

import { Property } from '../../types'

export const Suggestion: React.SFC<{
  property: Property
  selected: boolean
  onSelect: () => void
}> = ({ property, selected, onSelect }) => {
  return (
    <a
      className={'list-group-item ' + (selected ? 'active' : '')}
      onClick={onSelect}
      href="#"
    >
      {property.name}
    </a>
  )
}

export const Suggestions: React.SFC<{
  suggestions: Property[]
  limit: number
  selected: number
  onSelect: (p: Property) => void
}> = ({ suggestions, limit, selected, onSelect }) => {
  return (
    <div className="list-group">
      {suggestions.slice(0, limit).map((p, i) => (
        <Suggestion
          key={p.uid}
          property={p}
          selected={selected === i}
          onSelect={() => onSelect(p)}
        />
      ))}
    </div>
  )
}

export default Suggestions
