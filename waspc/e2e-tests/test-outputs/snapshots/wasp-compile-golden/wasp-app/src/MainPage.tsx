import Logo from "./assets/wasp-logo-rounded.svg";
import "./Main.css";

export function MainPage() {
  return (
    <main className="container">
      <img className="logo" src={Logo} alt="Wasp logo" />

      <h1 className="title">
        Welcome to <span className="highlight">Wasp</span>
      </h1>

      <p className="content">
        This is page <code>MainPage</code> located at route <code>/</code>.
        <br />
        Open <code>src/MainPage.tsx</code> to edit it.
      </p>

      <div className="buttons">
        <a
          className="button button-primary"
          href="https://wasp.sh/docs/tutorial/create"
          target="_blank"
          rel="noreferrer noopener"
        >
          Take the Tutorial
        </a>
        <a
          className="button button-secondary"
          href="https://discord.com/invite/rzdnErX"
          target="_blank"
          rel="noreferrer noopener"
        >
          Chat on Discord
        </a>
      </div>
    </main>
  );
}
