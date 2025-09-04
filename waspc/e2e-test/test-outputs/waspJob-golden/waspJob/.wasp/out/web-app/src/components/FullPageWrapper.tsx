const wrapperStyles = {
  display: 'flex',
  minHeight: '80vh',
  justifyContent: 'center',
  alignItems: 'center',
}

export function FullPageWrapper({
  children,
  className,
}: {
  children: React.ReactNode;
  className?: string;
}) {
  const classNameWithDefaults = ['wasp-full-page-wrapper', className].filter(Boolean).join(' ');
  return (
    <div className={classNameWithDefaults} style={wrapperStyles}>
      {children}
    </div>
  );
}
