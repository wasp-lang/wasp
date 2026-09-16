const wrapperStyles = {
    display: 'flex',
    minHeight: '80vh',
    justifyContent: 'center',
    alignItems: 'center',
};
export function FullPageWrapper({ children, className, }) {
    const classNameWithDefaults = ['wasp-full-page-wrapper', className].filter(Boolean).join(' ');
    return (<div className={classNameWithDefaults} style={wrapperStyles}>
      {children}
    </div>);
}
//# sourceMappingURL=FullPageWrapper.jsx.map