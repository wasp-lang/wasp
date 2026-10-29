/*
  This template gets rendered when you visit the root route on the server (i.e.
  http://localhost:3001/) in development mode.

  Some of the data passed to this template is known at runtime and not compile
  time, so we need to craft the string at runtime with a JS template string. We
  explicitly do not use Wasp mustaches here so we don't have to reason about two
  different templating systems.

  For now, we don't serve any other static files in our routes, so all the
  resources needed (CSS, images, etc.) must be inlined into the file.
*/

// The /* HTML */ comment is a hint to `prettier` to format this string as HTML.
export const makeWrongPortPage = ({
  appName,
  frontendUrl,
}: {
  appName: string;
  frontendUrl: string;
}): string => /* HTML */ `
  <!doctype html>
  <html lang="en">
    <head>
      <meta charset="UTF-8" />
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <title>${appName} API Server</title>

      <style>
        :root {
          --yellow: #f5c842;
          --yellow-dark: #d4a930;
          --black: #111;
          --cream: #f7f5f0;
          --white: #fafafa;
          --font-mono:
            ui-monospace, "SF Mono", Menlo, Consolas, "Liberation Mono",
            monospace;
          --border: 2px solid var(--black);
        }

        * {
          box-sizing: border-box;
        }

        body {
          margin: 0;
          background: var(--cream);
          color: var(--black);
          font-family: var(--font-mono);
          line-height: 1.5;
        }

        .page {
          display: grid;
          grid-template-rows: auto 1fr;
          min-height: 100dvh;
        }

        header {
          display: flex;
          align-items: center;
          justify-content: space-between;
          padding: 0.75rem 1.5rem;
          border-bottom: var(--border);
          background: var(--white);
        }

        main {
          display: flex;
          flex-direction: column;
          gap: 1rem;
          align-self: center;
          width: min(32rem, 100% - 2rem);
          margin: 3rem auto;
        }

        h1 {
          display: flex;
          align-items: center;
          gap: 0.5rem;
          margin: 0;
          font-size: 1.25rem;
        }

        h1 svg {
          height: 2.5rem;
          width: 2.5rem;
        }

        h2 {
          margin: 0;
          font-size: 1.5rem;
          text-transform: uppercase;
        }

        p {
          margin: 0;
        }

        a {
          color: var(--black);
        }

        .card {
          border: var(--border);
          background: var(--white);
          box-shadow: 6px 6px 0 var(--yellow);
          padding: 1rem 2rem 2rem;
        }

        :focus-visible {
          outline: 2px solid var(--black);
          outline-offset: 2px;
        }

        .button {
          align-self: flex-start;
          padding: 0.75rem 1.25rem;
          border: var(--border);
          background: var(--yellow);
          color: var(--black);
          font-weight: 700;
          text-decoration: none;
          word-break: break-all;
        }

        .button:hover {
          background: var(--yellow-dark);
        }
      </style>
    </head>
    <body>
      <div class="page">
        <header>
          <h1>
            <svg viewBox="0 0 1520 1520" aria-label="Wasp logo" role="img">
              <rect width="1520" height="1520" rx="300" fill="#f5c842" />
              <g fill="#111">
                <path
                  d="m1007 300-156.98 2e-3 0.499 142.51h85.857c0.0239 2e-3 18.763 1.566 30.461 7.9108 13.537 7.3425 14.626 10.763 21.704 20.417 3.0674 4.1835 4.9651 10.687 6.0494 16.123 1.0827 5.4284 1.3754 9.7642 1.3762 9.7762v144.33l2e-3 0.0252s0.583 7.1093 1.796 15.918c1.213 8.809 3.0448 19.305 5.6079 26.205 4.3676 11.759 13.62 26.659 21.633 36.363 9.3866 11.367 22.228 20.124 34.969 27.568 11.294 6.5984 32.628 11.542 33.548 11.754v2.1847c-0.9202 0.21233-22.254 5.1555-33.548 11.754-12.741 7.4439-25.583 16.201-34.97 27.568-8.0134 9.7041-17.266 24.604-21.633 36.363-2.5631 6.9007-4.3948 17.396-5.6078 26.205-1.213 8.809-1.796 15.918-1.796 15.918l-2e-3 0.0271v144.33c-8.2e-4 0.012-0.29348 4.3478-1.3762 9.7762-1.0842 5.4359-2.982 11.94-6.0494 16.123-7.0789 9.6547-8.1671 13.075-21.704 20.418-11.698 6.3448-30.437 7.9107-30.461 7.9107h-85.857l-0.499 142.51h156.99s7.5461-0.1872 16.483-0.629c8.9371-0.4418 19.238-1.1296 24.889-2.1653 7.1809-1.3165 20.102-3.1616 33.53-9.0719 13.329-5.8668 16.299-7.3647 22.443-10.939 11.758-6.8408 24.169-19.146 34.441-32.311s18.389-27.167 21.463-37.524c6.1235-20.632 6.382-47.126 6.382-47.126v-195.36c0-0.0207 0.145-2.5801 0.505-5.7396 0.3614-3.1722 0.956-6.9416 1.7742-9.1512 0.9504-2.5664 2.8911-7.2999 5.3504-11.905 2.4592-4.6049 5.481-9.0841 8.3701-11.115 5.643-3.9674 12.809-9.5822 20.29-10.61 5.9133-0.81252 13.182-1.4397 13.202-1.4417h53.892v-149.83h-53.892c-0.019-2e-3 -7.2884-0.62915-13.202-1.4417-7.4814-1.028-14.648-6.6429-20.29-10.61-2.8891-2.0312-5.9109-6.5104-8.3701-11.115-2.4593-4.6049-4.4-9.3384-5.3504-11.905-0.8182-2.2095-1.4128-5.9789-1.7742-9.1512-0.36-3.1595-0.5038-5.7188-0.505-5.7396v-195.36s-0.2585-26.494-6.382-47.126c-3.0739-10.357-11.191-24.359-21.463-37.524s-22.683-25.47-34.441-32.311c-6.1438-3.5745-9.1141-5.0723-22.443-10.939-13.427-5.9102-26.349-7.7554-33.53-9.0718-5.6503-1.0358-15.952-1.7236-24.889-2.1654-8.9366-0.4418-16.483-0.62892-16.483-0.62892z"
                />
                <rect x="250" y="830" width="500" height="160" />
                <rect x="250" y="530" width="500" height="160" />
              </g>
            </svg>
            ${appName}
          </h1>
        </header>

        <main class="card">
          <h2>API Server</h2>
          <p>
            The server is up and running. This is the backend part of your Wasp
            application.
          </p>
          <p>To visit your frontend application, go to:</p>
          <a href="${frontendUrl}" class="button">${frontendUrl}</a>
          <p>
            <small>
              This message is shown because you are running the server in
              development mode. In production, this route would not show
              anything.
            </small>
          </p>
        </main>
      </div>
    </body>
  </html>
`;
