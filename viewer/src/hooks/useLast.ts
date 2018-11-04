import { useRef } from 'react'

function useLast<F>(factory: () => F | undefined | null) {
  const ref = useRef<F | undefined>(undefined)

  const value = factory()
  if (value) {
    ref.current = value
  } else if (value === null) {
    ref.current = undefined
  }

  return ref.current
}

export default useLast