import type { ComponentProps } from 'react'
import styles from './Form.module.css'
import './auth-styles.css'

// PRIVATE API
export const Form = ({ children, className, ...props }: ComponentProps<'form'>) => (
  <form className={`${styles.form} ${className || ""}`} {...props}>
    {children}
  </form>
)

// PUBLIC API
export const FormItemGroup = ({ children, className, ...props }: ComponentProps<'div'>) => (
  <div className={`${styles.formItemGroup} ${className || ""}`} {...props}>
    {children}
  </div>
)

// PUBLIC API
export const FormLabel = ({ children, className, ...props }: ComponentProps<'label'>) => (
  <label className={`${styles.formLabel} ${className || ""}`} {...props}>
    {children}
  </label>
)

// PUBLIC API
export const FormInput = ({ className, ...props }: ComponentProps<'input'>) => (
  <input className={`${styles.formInput} ${className || ""}`} {...props} />
)

// PUBLIC API
export const FormTextarea = ({ className, ...props }: ComponentProps<'textarea'>) => (
  <textarea className={`${styles.formTextarea} ${className || ""}`} {...props} />
)

// PUBLIC API
export const FormError = ({ children, className, ...props }: ComponentProps<'div'>) => (
  <div className={`${styles.formError} ${className || ""}`} {...props}>
    {children}
  </div>
)

// PRIVATE API
export const SubmitButton = ({ children, className, ...props }: ComponentProps<'button'>) => (
  <button className={`${styles.submitButton} ${className || ""}`} {...props}>
    {children}
  </button>
)
