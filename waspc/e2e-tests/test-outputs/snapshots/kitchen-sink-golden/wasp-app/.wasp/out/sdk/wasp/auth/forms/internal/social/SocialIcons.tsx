import {
  DiscordIcon,
  GitHubIcon,
  GoogleIcon,
  KeycloakIcon,
  MicrosoftIcon,
  SlackIcon,
} from '@wasp.sh/lib-auth/browser'
import '../auth-styles.css'
import styles from './SocialIcons.module.css'

// PRIVATE API
export const Google = () => <GoogleIcon className={styles.icon} />

// PRIVATE API
export const Keycloak = () => <KeycloakIcon className={styles.icon} />

// PRIVATE API
export const GitHub = () => <GitHubIcon className={styles.icon} />

// PRIVATE API
export const Discord = () => <DiscordIcon className={styles.icon} />

// PRIVATE API
export const Slack = () => <SlackIcon className={styles.icon} />

// PRIVATE API
export const Microsoft = () => <MicrosoftIcon className={styles.icon} />
