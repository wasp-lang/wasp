import '../auth-styles.css'
import styles from './SocialButton.module.css'

// PRIVATE API
export const SocialButton = ({ children, ...props }: React.AnchorHTMLAttributes<HTMLAnchorElement>) => (
  <a className={styles.socialButton} {...props}>
    {children}
  </a>
)
