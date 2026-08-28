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
          --page-background: #f0f0f0;
          --wrapper-background: white;
          --wasp-yellow: #f5cc05;
          --main-link-color: #1a73e8;
        }

        .wrapper {
          font-family: system-ui, sans-serif;
          width: 90%;
          max-width: 600px;
          margin: 2em auto;
        }

        h1,
        h2 {
          margin: 0;
        }

        .main-link {
          text-align: center;
          font-size: 1.5em;
          font-weight: bold;
          font-family: ui-monospace, monospace;
        }

        .icon {
          width: 1em;
          height: 1em;
        }

        .wasp-title {
          margin: 0.5em 0;
          display: flex;
          align-items: center;
          gap: 0.2em;
        }

        body {
          background-color: var(--page-background);
        }

        main {
          background-color: var(--wrapper-background);
          padding: 1.5em;
          border-radius: 10px;
        }

        a,
        a:visited {
          color: var(--main-link-color);
        }
      </style>
    </head>
    <body>
      <div class="wrapper">
        <header>
          <h2 class="wasp-title">
            <svg viewBox="0 0 161 161" class="icon" alt="Wasp Logo">
              <circle cx="80.5" cy="80.5" r="79" fill="var(--wasp-yellow)" />
              <path
                d="M88.67 114.33h2.91q6 0 7.87-1.89c1.22-1.25 1.83-3.9 1.83-7.93V93.89c0-4.46.65-7.7 1.93-9.73s3.51-3.43 6.67-4.2q-4.69-1.08-6.65-4.12c-1.3-2-2-5.28-2-9.77V55.44q0-6-1.83-7.93t-7.87-1.88h-2.86V39.5h2.65q10.65 0 14.24 3.15t3.59 12.62v10.29c0 4.28.77 7.24 2.29 8.87s4.3 2.44 8.32 2.44h2.74V83h-2.74q-6 0-8.32 2.49c-1.52 1.65-2.29 4.64-2.29 9v10.25q0 9.47-3.59 12.64t-14.24 3.12h-2.65Z"
              />
              <path d="M38.5 85.15h37.33v7.58H38.5Zm0-17.88h37.33v7.49H38.5Z" />
            </svg>
            Wasp
          </h2>
        </header>

        <main>
          <h1>${appName} API Server</h1>
          <p>
            The server is up and running. This is the backend part of your Wasp
            application.
          </p>
          <p>
            If you want to visit your frontend application, go to this URL in
            your browser:
          </p>
          <a href="${frontendUrl}" class="main-link">
            <p>${frontendUrl}</p>
          </a>
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
