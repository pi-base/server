import { History, Location } from 'history'

import qs from 'query-string'

export type Query<T> = { [k in keyof T]?: string }

export interface Args {
  history: History<any>
  location: Location<any>
}

export type UseSearch<T> = [
  Query<T>,
  (q: Partial<Query<T>>) => void
]

function updateQuery<T>(query: Query<T>, patch: Query<T>): Query<T> {
  const merged: Query<T> = Object.assign({}, query, patch)

  let filtered: Query<T> = {}
  for (const key in merged) {
    if (merged.hasOwnProperty(key) && merged[key]) {
      filtered[key] = merged[key]
    }
  }

  return filtered
}

function useSearch<T>({
  history,
  location
}: Args): UseSearch<T> {
  // TODO: populate keys that aren't present with '' and update types
  const query: Query<T> = qs.parse(location.search)

  const setQuery = (patch: Query<T>) => {
    const search = `?${qs.stringify(updateQuery(query, patch))}`
    if (history.location.search === search) { return }

    const path = `${history.location.pathname}${search}`
    // Only record a history entry when going from no-search => search
    // (We don't want a history entry for each keypress)
    if (history.location.search) {
      history.replace(path)
    } else {
      history.push(path)
    }
  }

  return [query, setQuery]
}

export default useSearch