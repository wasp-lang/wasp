import styles from './Form.module.css'
import './auth-styles.css'

// PRIVATE API
export const Form = ({ children, ...props }: React.FormHTMLAttributes<HTMLFormElement>) => (
  <form className={styles.form} {...props}>
    {children}
  </form>
)

// PUBLIC API
export const FormItemGroup = ({ children, ...props }: React.HTMLAttributes<HTMLDivElement>) => (
  <div className={styles.formItemGroup} {...props}>
    {children}
  </div>
)

// PUBLIC API
export const FormLabel = ({ children, ...props }: React.LabelHTMLAttributes<HTMLLabelElement>) => (
  <label className={styles.formLabel} {...props}>
    {children}
  </label>
)

// PUBLIC API
export const FormInput = ({ ...props }: React.InputHTMLAttributes<HTMLInputElement>) => (
  <input className={styles.formInput} {...props} />
)

// PUBLIC API
export const FormTextarea = ({ ...props }: React.TextareaHTMLAttributes<HTMLTextAreaElement>) => (
  <textarea className={styles.formTextarea} {...props} />
)

// PUBLIC API
export const FormError = ({ children, ...props }: React.HTMLAttributes<HTMLDivElement>) => (
  <div className={styles.formError} {...props}>
    {children}
  </div>
)

// PRIVATE API
export const SubmitButton = ({ children, ...props }: React.ButtonHTMLAttributes<HTMLButtonElement>) => (
  <button className={styles.submitButton} {...props}>
    {children}
  </button>
)
