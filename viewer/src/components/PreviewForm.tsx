import * as F from 'formik'
import * as React from 'react'
import { useState } from 'react'

interface PreviewFormProps<Values, Result> {
  // FIXME: sort out these component types
  Preview: any
  Fields: any
  initialValues: Values
  validate: (values: Values) => {
    result?: Result
    errors: F.FormikErrors<Values>
  }
  onSubmit: (result: Result) => any
}

const noErrors = (errors: F.FormikErrors<any>): boolean =>
  Object.keys(errors).length === 0

function PreviewForm<Values, Result>(
  props: PreviewFormProps<Values, Result>
) {
  const {
    Preview,
    Fields,
    initialValues,
    validate,
    onSubmit,
    ...rest
  } = props
  const [preview, setPreview] = useState<Result | undefined>(undefined)

  const handleValidate = (values: Values) => {
    const { result, errors } = validate(values)
    if (result && result !== preview) {
      setPreview(result)
    }
    return errors
  }

  const handleSubmit = (values: Values) => {
    const { result, errors } = validate(values)
    if (noErrors(errors)) { onSubmit(result!) }
  }

  if (!preview) {
    handleValidate(initialValues)
  }

  return (
    <F.Formik
      initialValues={initialValues}
      onSubmit={handleSubmit}
      validate={handleValidate}
      validateOnBlur={true}
      render={(props: F.FormikProps<Values>) => {
        const { isSubmitting } = props

        return (
          <div className="row">
            <F.Form className="col-md-6">
              <Fields />
              <button
                className="btn btn-default"
                type="submit"
                disabled={isSubmitting}
              >
                Save
              </button>
            </F.Form>
            <div className="col-md-6">
              {preview && <Preview result={preview} />}
            </div>
          </div>
        )
      }}
      {...rest}
    />
  )
}

export default PreviewForm
