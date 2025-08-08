import styles from './Message.module.css'
import './auth-styles.css'

// PRIVATE API
export const Message = ({ children, ...props }: React.HTMLAttributes<HTMLDivElement>) => (
  <div className={styles.message} {...props}>
    {children}
  </div>
)

// PRIVATE API
export const MessageError = ({ children, ...props }: React.HTMLAttributes<HTMLDivElement>) => (
  <div className={styles.messageError} {...props}>
    {children}
  </div>
)

// PRIVATE API
export const MessageSuccess = ({ children, ...props }: React.HTMLAttributes<HTMLDivElement>) => (
  <div className={styles.messageSuccess} {...props}>
    {children}
  </div>
)
