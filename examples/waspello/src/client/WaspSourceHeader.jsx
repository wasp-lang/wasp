import waspLogo from './waspLogo.png'

const WaspSourceHeader = (props) => {
  const divStyle = {
    position: "sticky",
    top: 0,
    backgroundColor: "#212121",
    color: "#fff",
    padding: "8px 24px",
    display: "flex",
    alignItems: "center",
    justifyContent: "center",
    minHeight: "40px",
    height: "100%",
    filter: "drop-shadow(0 0 0.25rem gray)"
  }

  return (
    <div style={divStyle}>
      <span style={{ marginRight: "5px" }}>
        <img alt="Wasp" src={waspLogo} className='h-8' />
      </span>
      <span>
        This is an example <a href="https://wasp-lang.dev" target="_blank" rel="noreferrer" style={{ color: "#fc0" }}>Wasp</a> application{props.name && ` called ${props.name}`}.
        To see the source, please visit our <a href="https://github.com/wasp-lang/wasp/tree/main/examples" target="_blank" rel="noreferrer" style={{ color: "#fc0" }}>GitHub repo</a>.
      </span>
      <span style={{ marginLeft: "5px" }}>🚀</span>
    </div>
  )
}

export default WaspSourceHeader
