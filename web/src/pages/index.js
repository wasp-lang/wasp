import React, { useState, useEffect } from 'react';
import clsx from 'clsx';
import Layout from '@theme/Layout';
import CodeBlock from '@theme/CodeBlock';
import CodeBlockWithTitle from '../components/CodeBlockWithTitle'
import EmailSignupForm from '../components/EmailSignupForm/index.js'
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';
import Head from '@docusaurus/Head';
import styles from './styles.module.css';
import Modal from 'react-modal';

const features = [
  {
    title: 'Quick start',
    //imageUrl: 'img/undraw_docusaurus_mountain.svg',
    description: (
      <>
        No more endless configuration files. Create a production-ready web app
        with just a few lines of code - we will set you up with all the best defaults.
      </>
    ),
  },
  {
    title: 'Speed & Power',
    //imageUrl: 'img/undraw_docusaurus_tree.svg',
    description: (
      <>
        Move fast using Wasp's declarative language, but also
        drop down to <code>js/html/css...</code> when you require more control.
      </>
    ),
  },
  {
    title: 'No lock-in',
    //imageUrl: 'img/undraw_docusaurus_react.svg',
    description: (
      <>
        If Wasp becomes too limiting for you, simply eject and continue with the human-readable
        source code following industry best-practices.
      </>
    ),
  },
];

function Feature({imageUrl, title, description}) {
  const imgUrl = useBaseUrl(imageUrl);
  return (
    <div className={clsx('col col--4', styles.feature)}>
      {imgUrl && (
        <div className="text--center">
          <img className={styles.featureImage} src={imgUrl} alt={title} />
        </div>
      )}
      <h2>{title}</h2>
      <p>{description}</p>
    </div>
  );
}

function PageBreakWithLogo() {
  return (
    <section className={'section-lg'}>
      <div className="container"
           style={{ textAlign: 'center' }}>
        <img className="logo" src="img/eqpar-separator.png"/>
      </div>
    </section>
  )
}

function WaspLatestVersion() {
  const [latestRelease, setLatestRelease] = useState(null)

  useEffect(() => {
    const fetchRelease = async () => {
      const response = await fetch(
        'https://api.github.com/repos/wasp-lang/wasp/releases'
      )
      console.log(response)
      const releases = await response.json()
      if (releases) {
        setLatestRelease(releases[0])
      }
    }
    fetchRelease()
  }, [])

  if (latestRelease) {
    return (
      <h4 className={styles.waspVersion}>
        <a href={latestRelease.html_url}>
          { latestRelease.name }
        </a>
      </h4>
    )
  } else {
    return null
  }
}

function ProductHuntBadge() {
  return (
    <a href="https://www.producthunt.com/posts/wasp-lang-alpha?utm_source=badge-top-post-badge&utm_medium=badge&utm_souce=badge-wasp-lang-alpha" target="_blank">
      <img 
        src="https://api.producthunt.com/widgets/embed-image/v1/top-post-badge.svg?post_id=277135&theme=light&period=daily"
        alt="Wasp-lang Alpha - Develop web apps in React & Node.js with no boilerplate | Product Hunt"
        style={{width: '250px', height: '54px'}} width="250" height="54"
      />
  </a>
  )
}

function HeroCodeExample() {
  // NOTE: There is an image in static/img/hero-code-shot.png of this code,
  //   used as the main image of the web app (specified via <meta>) when being
  // parsed by external sites (Facebook, Twitter, Reddit, ...).
  // Therefore, if this example changes, you should also update that image
  // (just take a screenshot).

  const createAppWaspCode =
`app todoApp {
  title: "ToDo App", /* visible in tab */

  auth: { /* full-stack auth out-of-the-box */
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      usernameAndPassword: {},
      google: {}
    }
  }
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  /* import your React code */
  component: import Main from "@ext/Main.js"
}
`
  return (
    <CodeBlockWithTitle title="todoApp.wasp" language="css">
      { createAppWaspCode }
    </CodeBlockWithTitle>
  )
}

function CodeExamples() {
  const CodeExample = Object.freeze({
    NEW_APP: 'Create a new app',
    DEFINE_ENTITY: 'Define and query a data model',
    ADD_AUTH: 'Add authentication'
  })

  const getButtonTextForCodeExample = (codeExample) => CodeExample[codeExample]

  const [currentCodeExample, setCodeExample] = useState(CodeExample.NEW_APP)

  function CurrentCodeExample() {
    if (currentCodeExample === CodeExample.NEW_APP) {
    const createAppWaspCode =
`/* global app settings */
app todoApp {
  title: "ToDo App" /* browser tab title */
}

/* routing */
route RootRoute { path: "/", to: MainPage }
page MainPage {
  component: import Main from "@ext/Main" /* import your React code */
}
`

    const createAppMainComponentCode =
`import React from 'react'

export default () => <span> Hello World! </span>
`
      return (
        <div className="codeExampleFiles">
          <CodeBlockWithTitle title="todoApp.wasp" language="css">
            { createAppWaspCode }
          </CodeBlockWithTitle>

          <CodeBlockWithTitle
            title="ext/Main.js | External React code, imported above"
            language="jsx">
            { createAppMainComponentCode }
          </CodeBlockWithTitle>

          <div>
            That's it, this is the whole app! Run <code>wasp start</code> and check it out
            at <code>http://localhost:3000</code>!
          </div>
        </div>
      )
    } else if (currentCodeExample === CodeExample.ADD_AUTH) {
    const exampleCode =
`app todoApp {
  /* ... */

  /* full-stack auth out-of-the-box */
  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {}
    }
    onAuthFailedRedirectTo: "/login"
  }
}

/* ... */

/* username & password required because of the auth method above */
entity User {=psl
    id          Int     @id @default(autoincrement())
    username    String  @unique
    password    String
psl=}

page MainPage {
  authRequired: true, /* available only to logged in users */
  component: import Main from "@ext/Main"
}
`

      const mainUsingAuthCode =
`import React from 'react'
import Todo from './Todo.js'

/* Because of authRequired property in todoApp.wasp, this page is
 * available only to logged in users and prop 'user' is automatically provided by wasp. */
export default ({ user }) => {
  return <Todo/>
}
`
      return (
        <div className="codeExampleFiles">
          <CodeBlockWithTitle title="todoApp.wasp" language="css">
            { exampleCode }
          </CodeBlockWithTitle>
          <CodeBlockWithTitle title="ext/Main.js | Checking if user is logged in" language="jsx">
            { mainUsingAuthCode }
          </CodeBlockWithTitle>

          <div>
            To learn more about authentication & authorization in Wasp, check
            the <Link to={useBaseUrl('/docs/language/features#authentication--authorization')}>docs</Link>.
          </div>
        </div>
      )
    } else if (currentCodeExample === CodeExample.DEFINE_ENTITY) {
      const defineEntityWaspCode =
`/* ... */

/* Data model is defined via Prisma Schema Language (PSL) */
entity Task {=psl
    id          Int     @id @default(autoincrement())
    description String
    isDone      Boolean @default(false)
psl=}

query getTasks {
  fn: import { getTasks } from "@ext/queries.js", /* import Node.js function */
  /* A list of entities this query uses - useful for automatic invalidation and refetching */
  entities: [Task]
}
`
      const getTasksQueryCode =
`export const getTasks = async (args, context) => {
  // Since we above declared this query is using Task, it is automatically injected in the
  // context.
  return context.entities.Task.findMany()
}
`
      const todoUsingGetTasksCode =
`import React from 'react'
import { useQuery } from '@wasp/queries'
import getTasks from '@wasp/queries/getTasks.js'

export default () => {
  // Standard useQuery syntax, Wasp provides a thin wrapper around it.
  const { data: tasks } = useQuery(getTasks)
  return <Tasks tasks={tasks}/>
}
`
      return (
        <div className="codeExampleFiles">
          <CodeBlockWithTitle title="todoApp.wasp" language="css">
            { defineEntityWaspCode }
          </CodeBlockWithTitle>
          <CodeBlockWithTitle
            title="ext/queries.js | Node.js function imported in a query above"
            language="jsx"
          >
            { getTasksQueryCode }
          </CodeBlockWithTitle>
          <CodeBlockWithTitle title="ext/Todo.js | Invoking query from React code" language="jsx">
            { todoUsingGetTasksCode }
          </CodeBlockWithTitle>

          <div>
            To learn more about working with data in Wasp, check
            the <Link to={useBaseUrl('/docs/language/language/features#entity')}>docs</Link>.
          </div>
        </div>
      )
    } else {
      console.log('this should never happen.')
      return null
    }
  }

  function Buttons() {

    function Button({ codeExampleKey }) {
      return (
        <button
          className={clsx('button',
            'info', 
            currentCodeExample === CodeExample[codeExampleKey] && 'is-active',
            'button--secondary'
          )}
          onClick={() => setCodeExample(CodeExample[codeExampleKey])}
        >
          { getButtonTextForCodeExample(codeExampleKey) }
        </button>
      )
    }

    return (
      Object.keys(CodeExample).map((k, idx) => <Button codeExampleKey={k} key={idx} />)
    )
  }

  return (
    <div className="row CodeExamples">
      <div className="ButtonTabs col col--3">
        <div>
          <Buttons/>
        </div>
      </div>

      <div className="col col--9">
        <CurrentCodeExample/>
      </div>
    </div>
  )
}

function WaspGhStarsCount() {
  return (
    <iframe
        src="https://ghbtns.com/github-btn.html?user=wasp-lang&repo=wasp&type=star&count=true&size=large" 
        frameBorder="0"
        scrolling="0"
        width="160px" height="30px">
    </iframe>
  )
}

function WaspDiscordBadge() {
  return (
    <a href="https://discord.gg/rzdnErX">
      <img alt="discord" src="https://img.shields.io/discord/686873244791210014?label=chat%20@%20discord" height="29px" />
    </a>
  )
}

function EmailCta() {
  return (
    <section className={clsx('section-lg', 'emailCtaTop')} id="signup-atf">
      <div className="container">

        <div className={clsx('row', styles.responsiveCentered)}>
          <div className="col col--10 col--offset-1">
          </div>
        </div>

        <div className={clsx('row', styles.responsiveCentered)} style={{ paddingTop: '1rem' }}>
          <div className="col col--8 col--offset-2">
            <EmailSignupForm placeholder="Enter your email to receive updates"/>
          </div>
        </div>

      </div>
    </section>
  )
}

function EmailAndGithubCta() {
  return (
    <section className={'section-lg bg-diff'} id="signup">
      <div className="container">

        <div className={clsx('row', styles.responsiveCentered)}>
          <div className="col col--10 col--offset-1">
            <h2>Stay up to date</h2>
            <h3>
              <p>
                Join our mailing list and be the first to know when we ship new features
                and updates!
              </p>
            </h3>
          </div>
        </div>

        <div className={clsx('row', styles.responsiveCentered)} style={{ paddingTop: '1rem' }}>
          <div className="col col--8 col--offset-2">
            <EmailSignupForm/>
          </div>
        </div>

        <div className={clsx('row', styles.responsiveCentered, 'section-text')}>
          <div className="col col--10 col--offset-1">
            <h3>
              <p>
                Also, make sure to check
                out <Link to={'https://github.com/wasp-lang/wasp'}>Wasp repo
                on Github</Link> and express your support by
                giving us a star!
              </p>
            </h3>
          </div>
        </div>

        <div className={clsx('row', styles.responsiveCentered)} style={{ paddingTop: '1rem' }}>
          <div className="col">
            <WaspGhStarsCount />
            <WaspDiscordBadge />
          </div>
        </div>

      </div>
    </section>
  )
}

function SocialProofSection() {
  return (
    <section className={clsx('section-lg', 'bg-diff', styles.socialProofSection)}>
      <div className="container">
        <div className={clsx('row', styles.responsiveCentered)}>

          <div className="col col--10 col--offset-1">
            <div className={clsx(styles.socialProof)}>
              <div className={clsx(styles.backedByYC)}>
                <img className={clsx(styles.ycLogo)} src="img/ycombinator-logo.png" />
                <span>backed by <strong>Y Combinator</strong></span>
              </div>
              <ProductHuntBadge />
            </div>
          </div>

        </div> {/* End of row */}
      </div>
    </section>
  )
}

function Home() {
  const context = useDocusaurusContext();
  const {siteConfig = {}} = context;

  const [modalIsOpen, setModalIsOpen] = React.useState(false)

  const todoTutorialUrl = useBaseUrl('docs/tutorials/todo-app');
  const waspRwaDemoUrl = 'https://wasp-rwa.netlify.app';

  const modalStyles = {
    content : {
      top                   : '40%',
      left                  : '40%',
      transform             : 'translate(-30%, -30%)',
      paddingTop            : '4rem',
      background            : 'var(--ifm-hero-background-color)',
      /*
      right                 : 'auto',
      bottom                : 'auto',
      marginRight           : '-50%',
      */
    }
  };

  function openModal() {
    setModalIsOpen(true);
  }

  function closeModal() {
    setModalIsOpen(false);
  }

  return (
    <Layout
      title={`${siteConfig.title}`}
      description={siteConfig.tagline}
    >
      <Head>
        <meta property="og:image" content={siteConfig.url + useBaseUrl('img/hero-code-shot.png')} />
      </Head>

      <header className={clsx('hero', styles.heroBanner)}>
        <div className="container">

          <div className="row hero-row">
            <div className="col col--7">

              <div className="hero-text-col">

                <h2 className="hero-subtitle">{siteConfig.tagline}</h2>

                <div className="hero-works-with">
                  <h3 className="works-with-text">
                    Describe high-level features with Wasp DSL and write the rest of your logic
                    in React, Node.js and Prisma.
                  </h3>
                  <div className="hero-works-with-icons">
                    <img src="img/react-logo.svg" />
                    <img src="img/nodejs-logo.svg" />
                    <img src="img/prisma-logo.svg" />
                  </div>
                </div>
              </div>
            </div> {/* End of col. */}

            <div className="col col--5">
              <HeroCodeExample/>

            </div> {/* End of col. */}

          </div> {/* End of row. */}

          <div className={clsx('row', styles.responsiveCentered, styles.tryWaspRow)}>
            <div className="col col--10 col--offset-1">

              <div className={clsx(styles.tryWaspContainer)}>
                <div className={clsx(styles.startCliCmd)}>
                  <span><code>curl -sSL https://get.wasp-lang.dev/installer.sh | sh</code></span>
                </div>

                {/* TODO: Martin commented tihs out because both buttons started showing up on desktop after upgrading to newer docusaurus. This happens when built for deployment, not during serving for development. Instead, he replaced it with just one version of button, below, instead of having one button for mobile and one for desktop.
                <div className={clsx(styles.startButtonAndVersion, styles.visibleOnDesktopOnly)}>
                  <button
                    className={clsx(
                      'button button--primary button--huge',
                      styles.heroButton,
                    )}
                    onClick={openModal}
                  >
                      Try Wasp in 5 minutes →
                  </button>
                </div>

                <div className={clsx(styles.startButtonAndVersion, styles.visibleOnMobileOnly)}>
                  <Link
                    className={clsx(
                      'button button--primary button--huge',
                      styles.heroButton,
                    )}
                    to={useBaseUrl('/docs')}
                  >
                      Try Wasp in 5 minutes →
                  </Link>
                </div>
                */}

                <div className={clsx(styles.startButtonAndVersion)}>
                  <Link
                    className={clsx(
                      'button button--primary button--huge',
                      styles.heroButton,
                    )}
                    to={useBaseUrl('/docs')}
                  >
                    Try Wasp in 5 minutes →
                  </Link>
                </div>

              </div>

              <div className={clsx(styles.usingWindows)}>
                Using Windows? Check the instructions <Link to={useBaseUrl("/docs/#2-installation")}>here</Link>.
              </div>

              <Modal
                isOpen={modalIsOpen}
                style={modalStyles}
                onRequestClose={closeModal}
                shouldCloseOnOverlayClick={true}
              >
                <div className="container">
                  <div className={clsx('row')}>
                    <div className="col col--10">


                      <h2 className="modal-step-title">1. Open your terminal and run:</h2>
                      <div className={clsx(styles.startCliCmd)} style={{ height: '40px' }}>
                        <span><code>curl -sSL https://get.wasp-lang.dev/installer.sh | sh</code></span>
                      </div>

                      <h2 className="modal-step-title "style={{marginTop: '4rem'}}>2. Create a new project:</h2>
                      <div className={clsx(styles.startCliCmd)} style={{ height: '40px' }}>
                        <span><code>wasp new MyFirstApp</code></span>
                      </div>

                      <h2 className="modal-step-title" style={{marginTop: '4rem'}}>3. Run your first app:</h2>
                      <div className={clsx(styles.startCliCmd)} style={{ height: '40px', marginRight: '10px' }}>
                        <span><code>cd MyFirstApp && wasp start</code></span>
                      </div>
                      <span>
                        That's it!
                        Open <Link to='http://localhost:3000/'>http://localhost:3000</Link> and see how it looks like!
                      </span>

                      <div style={{marginTop: '4rem'}}>
                        <span>
                          You ran into problems or want more details? Refer to the <Link to={useBaseUrl('/docs')}>docs</Link>.
                        </span>
                      </div>

                    </div>
                  </div>
                </div>


              </Modal>


              <div className={clsx('row', styles.responsiveCentered)} style={{ paddingTop: '1rem' }}>
                <div className="col">
                  <WaspGhStarsCount />
                  <WaspDiscordBadge />
                </div>
              </div>

            </div>
          </div> {/* End of row. */}

        </div>
      </header>

      <main>

        {/* Social proof */}
        <SocialProofSection />

        {/* One-line explanation */}
        <section className={'section-lg'}>
          <div className="container">
            <div className={clsx('row', styles.responsiveCentered)}>
              <div className="col col--12">
                <h3 className={'title'}>
                  Wasp is an open source, declarative DSL for devs who want to <span className="title-strong">use modern web dev stack</span>
                  &nbsp;
                  <span style={{ whiteSpace: 'nowrap' }}>
                    (React <img src="img/react-logo.png" height="25px" />,
                    Node.js <img src="img/node-logo.png" height="25px" />,
                    Prisma <img src="img/prisma-logo.png" height="25px" />,
                    ...)
                  </span>
                  &nbsp;
                  <span className="title-strong">without writing boilerplate</span>.
                </h3>
                <h3>
                  <p>Frontend, backend and deployment - all unified with one concise language.</p>
                  <p>Zero configuration, all best practices.</p>
                </h3>
              </div>
            </div>
          </div>
        </section>

        <PageBreakWithLogo/>

        {/* Wasp compilation */}
        <section className={'section-lg'} id="how-it-works">
          <div className="container">

            <div className={clsx('row', styles.responsiveCentered)}>
              <div className="col col--10 col--offset-1">
                <h2>How it works</h2>
                <h3>
                  <p>
                    Given <code>.wasp</code> + <code>.js</code>, <code>.css</code>, <code>...</code> files as an input, Wasp compiler behind the scene&nbsp;
                    <span className="title-strong">generates the full
                    source code of your web app</span> - front-end, back-end and deployment.
                  </p>
                </h3>
              </div>
            </div>

            <div className={clsx('row', styles.responsiveCentered)} style={{ paddingTop: '2rem' }}>
              <div className="col">
                <img
                  className={'wasp-diagram'}
                  src="img/wasp-compilation.png"
                  alt="Wasp compilation"
                />
              </div>
            </div>

          </div>
        </section>
         
        <PageBreakWithLogo/>

        {/* Features */}
        {features && features.length > 0 && (
          <section className={clsx(styles.features, 'bg-diff')}>
            <div className="container">
              <div className="row">
                {features.map((props, idx) => (
                  <Feature key={idx} {...props} />
                ))}
              </div>
            </div>
          </section>
        )}

        <PageBreakWithLogo/>

        {/* Quick to start, easy to scale */}
        <section className={'section-lg'} id="fast-and-scalable">
          <div className="container">
            <div className={clsx('row', styles.responsiveCentered)}>
              <div className="col col--10 col--offset-1">
                <h2>Quick to start, easy to scale</h2>
                <h3>
                  <p>
                    Wasp aims to be at least as flexible as the traditional web frameworks like Ruby on Rails.
                    <br/>
                    Start your project quickly with the best defaults and customize and scale it as it grows.
                  </p>
                </h3>
                <h3>
                  <p>As an example, we used Wasp to implement a copy of Medium:</p>
                </h3>
              </div>
            </div> {/* End of row */}

            <div className="row">
              <div className="col col--10 col--offset-1">
                <a href={waspRwaDemoUrl} target="_blank">
                  <img
                    className="rwa"
                    src="img/rwa-screenshot.png"
                    alt="RealWorldApp in Wasp"
                  />
                </a>
              </div>
            </div>

            <div className={clsx('row', styles.responsiveCentered)}>
              <div className="col col--10 col--offset-1">
                <h3>You can try out the deployed app <a href={waspRwaDemoUrl}>here</a> or check out the source code <a href="https://github.com/wasp-lang/wasp/tree/main/examples/realworld">here</a>.</h3>
              </div>
            </div>

          </div>
        </section>

        <PageBreakWithLogo/>

        {/* What can Wasp do */}
        <section className={'section-lg'} id="what-can-do">
          <div className="container">

            <div className={clsx(styles.featuresAndRoadmap, 'row', styles.responsiveCentered)}>
              <div className="col col--10 col--offset-1">
                <h2>Features & Roadmap</h2>
              </div>
            </div>

            <div className={clsx('row')}>
              <div className="col col--6">
                <h3 className={styles.featureListTitle}>Alpha</h3>
                <ul className={clsx(styles.featuresList, styles.featuresListDone)}>
                  <li> full-stack auth (username & password, Google) </li>
                  <li> pages & routing </li>
                  <li> blurs the line between client & server - define your server actions and queries and call them directly in your client code (RPC)! </li>
                  <li> smart caching of server actions and queries (automatic cache invalidation) </li>
                  <li> entity (data model) definition with Prisma.io </li>
                  <li> ACL on frontend </li>
                  <li> importing NPM dependencies </li>
                  <li> background and scheduled jobs </li>
                </ul>
              </div>
              <div className="col col--6">
                <h3 className={styles.featureListTitle}>Coming next</h3>
                <ul className={clsx(styles.featuresList, styles.featuresListComing)}>
                  <li> ACL on backend </li>
                  <li> one-click deployment </li>
                  <li> more auth methods (Facebook, LinkedIn, ...) </li>
                  <li> tighter integration of entities with other features </li>
                  <li> themes and layouts </li>
                  <li> support for explicitly defined server API </li>
                  <li> inline JS - ability to mix JS code with Wasp code! </li>
                  <li> Typescript support </li>
                  <li> server-side rendering </li>
                  <li> Visual Editor </li>
                  <li> support for different languages on backend </li>
                  <li> richer wasp language with better tooling </li>
                </ul>
              </div>
            </div>
          </div>
        </section>

        <PageBreakWithLogo/>

        {/* The language */}
        <section className={'section-lg'} id="the-language">
          <div className="container">

            <div className={clsx('row', styles.responsiveCentered)}>
              <div className="col col--10 col--offset-1">
                <h2>The Language</h2>
                <h3>
                  <p>
                    Concepts such as <em>app</em>, <em>page</em>, <em>route</em>, <em>auth</em>&nbsp;
                    etc. are baked into Wasp, providing the
                    higher level of expressiveness.
                  </p>
                </h3>
              </div>
            </div>

            <CodeExamples/>

          </div>
        </section>

        {/* Take the tutorial */}
        <section className={'section-lg'}>
          <div className="container">

            <div className={clsx('row', styles.responsiveCentered)}>
              <div className="col col--10 col--offset-1">
                <h2>Take the tutorial</h2>
                <h3>
                  <p>
                    Take the <Link to={todoTutorialUrl}> Todo App tutorial </Link> and build a full-fledged Todo app in Wasp!
                  </p>
                </h3>
              </div>
            </div>

            <div className={clsx('row', styles.responsiveCentered)} style={{ paddingTop: '2rem' }}>
              <div className="col">
                <img alt="How Todo App will work once it is done"
                     src={useBaseUrl('img/todo-app-tutorial-intro.gif')}
                     style={{ border: "1px solid black" }}
                />
              </div>
            </div>

          </div>
        </section>

      <div id="join-the-list">
        <EmailAndGithubCta />
      </div>

      </main>
    </Layout>
  );
}

export default Home;
