import Logo from "./assets/wasp-logo-rounded.svg";
import "./Main.css";

export function MainPage() {
  return (
    <>
      <header>
        <h1>
          <img src={Logo} alt="Wasp logo" />
          My Wasp App
        </h1>
      </header>
      <main className="card">
        <h2>Welcome to Wasp!</h2>

        <p>
          This is page <code>MainPage</code> located at route <code>/</code>.
          Open <code>src/MainPage.tsx</code> to edit it.
        </p>

        <div className="links">
          <a
            className="button"
            href="https://wasp.sh/docs/tutorial/create"
            target="_blank"
            rel="noreferrer noopener"
          >
            Take the Tutorial
          </a>
          <a
            href="https://discord.com/invite/rzdnErX"
            target="_blank"
            rel="noreferrer noopener"
          >
            Chat on Discord
          </a>
        </div>
      </main>
    </>
  );
}
