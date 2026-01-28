const commonMessageStyles = {
  borderRadius: '.5rem',
  padding: '1rem',
}

const errorMessageStyles = {
  ...commonMessageStyles,
  borderColor: 'rgb(240 82 82)',
  backgroundColor: 'rgb(253 232 232)',
  color: 'rgb(200 30 30)',
}

const loadingMessageStyles = {
  ...commonMessageStyles,
  borderColor: 'rgb(107 114 128)',
  backgroundColor: 'rgb(243 244 246)',
  color: 'rgb(55 65 81)',
}

const titleStyles = {
  display: 'flex',
  alignItems: 'center',
  gap: '.5rem',
}

const subtitleStyles = {
  marginLeft: '1.75rem',
}

export function MessageError({
  children,
  subtitle,
}: {
  children: React.ReactNode;
  subtitle?: React.ReactNode;
}) {
  return (
    <div style={errorMessageStyles}>
      <div>
        <div style={titleStyles}>
          <MessageIcon /> {children}
        </div>
        {subtitle && <div style={subtitleStyles}>{subtitle}</div>}
      </div>
    </div>
  )
}

export function MessageLoading({
  children,
  subtitle,
}: {
  children: React.ReactNode;
  subtitle?: React.ReactNode;
}) {
  return (
    <div style={loadingMessageStyles}>
      <div>
        <div style={titleStyles}>
          <MessageIcon /> {children}
        </div>
        {subtitle && <div style={subtitleStyles}>{subtitle}</div>}
      </div>
    </div>
  )
}

const MessageIcon = () => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width="1.25rem"
    height="1.25rem"
    fill="currentColor"
    stroke="currentColor"
    strokeWidth={0}
    aria-hidden="true"
    viewBox="0 0 20 20"
  >
    <path
      fillRule="evenodd"
      stroke="none"
      d="M18 10a8 8 0 1 1-16 0 8 8 0 0 1 16 0zm-7-4a1 1 0 1 1-2 0 1 1 0 0 1 2 0zM9 9a1 1 0 0 0 0 2v3a1 1 0 0 0 1 1h1a1 1 0 1 0 0-2v-3a1 1 0 0 0-1-1H9z"
      clipRule="evenodd"
    />
  </svg>
)
