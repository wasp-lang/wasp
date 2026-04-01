import { useState, useEffect, useRef } from "react";
import Link from "@docusaurus/Link";
import { Layout, Eye, Code, RotateCcw, DollarSign, Globe, ArrowRight, Briefcase, Cpu } from "react-feather";
import { vc, CrosshairCard, VCSection } from "./vcVariant";
import CodeHighlight from "../CodeHighlight";

const benefits = [
  {
    Icon: Layout,
    title: "Smart decisions made upfront",
    v3Title: "Your AI has a plan",
    description:
      "Wasp is a full-stack, opinionated framework. When your AI needs to add a feature, it doesn't negotiate patterns or debate file structure. It already knows where client code goes, how server operations work, and how data flows. It just writes the logic.",
    v3Description:
      "Wasp gives your agent a clear structure to follow: where code goes, how things connect, and what patterns to use. No guessing, no decision fatigue.",
    url: "/docs",
  },
  {
    Icon: Cpu,
    title: "It's got skills",
    v3Title: "It's got skills",
    description:
      "Plugins and Agent Skills for your coding tool of choice turn your agent into a Wasp expert.",
    v3Description:
      "Plugins and Agent Skills for your coding tool of choice turn your agent into a Wasp expert.",

    url: "docs/wasp-ai/coding-agent-plugin",
  },
  {
    Icon: Code,
    title: "AI writes features, not plumbing",
    v3Title: "Focus on the fun stuff",
    description:
      "Auth? Declared in config, not coded. CRUD operations? Auto-generated from your data model. Email? Configured, not built. Deployment? One CLI command. Your AI spends its time on what makes your app unique — the actual business logic.",
    v3Description:
      "Login, database, email, background jobs, etc. Wasp handles it all so your agents can focus on you app's unique features.",
    url: "/docs/auth/overview",
  },
  {
    Icon: DollarSign,
    title: "Save Tokens",
    v3Title: "Save Tokens and Money",
    description:
      "Wasp reduces the amount of code your AI needs to read and write, so you spend less on LLM tokens.",
    v3Description:
      "Wasp reduces the amount of code you and your agent need to read and write, so you spend less on LLM tokens.",
    url: "/blog/2026/03/26/nextjs-vs-wasp-40-percent-less-tokens-same-app",
  },
  {
    Icon: Globe,
    title: "Deploy to any platform",
    v3Title: "Deploy to any platform",
    description:
      "Deploy to Railway, Fly.io, or self-host on your own server. No vendor lock-in.",
    v3Description:
      "Other frameworks tie you to their expensive platforms and pricing. Wasp lets you deploy anywhere easily.",
    url: "/docs/deployment/intro",
  },
  {
    Icon: Globe,
    title: "Everything. Integrated.",
    v3Title: "Everything. Integrated.",
    description:
      "Wasp is truly full-stack, from front-end to database and deployments.",
    v3Description:
      "Wasp is truly full-stack, from front-end to database and deployments. No fumbling through multiple tools and services.",
    url: "/docs",
  },
];

// --- Interactive demo for v3 ---

const TYPING_SPEED = 50;
const RESPONSE_TYPING_SPEED = 20;
const RESPONSE_LINE_PAUSE = 600;
const PAUSE_BETWEEN = 2300;
const DEPLOY_DURATION = 2000;
const LIVE_DURATION = 2000;
const REPLAY_DELAY = 1000;
const HIGHLIGHT_DURATION = 2500;

const conversation = [
  { prompt: "claude", response: ["\u23FA How can I help?"] },
  { prompt: "I want to build a SaaS app", response: ["\u23FA Bash(wasp new -t saas)", "\u23FA Your SaaS app template is ready."], event: "showFullCode" },
  { prompt: "Now add Slack login", response: ["\u23FA Done. Slack auth added!"], event: "addSlack" },
  { prompt: "Help me deploy my app to Railway", response: ["\u23FA Bash(wasp deploy railway launch)"], deploy: true },
];

const codeEmpty = `const app = new App()`;

const codeBase = `const app = new App('mySaasApp', {
  title: 'My SaaS App',
})

app.auth({
  userEntity: 'User',
  methods: { google: {} }
})

app.route('DashboardRoute', {
  path: '/dashboard', to: dashboard
})

app.query('getTasks', {
  fn: { import: 'getTasks', from: '@src/queries' },
  entities: ['Task']
})

app.api('payments', {
  fn: { import: 'paymentStripe', from: '@src/apis' },
  httpRoute: (POST, '/payment/stripe')
});`;

const codeWithSlack = `const app = new App('mySaasApp', {
  title: 'My SaaS App',
})

app.auth({
  userEntity: 'User',
  methods: { google: {}, slack: {} }
})

app.route('DashboardRoute', {
  path: '/dashboard', to: dashboard
})

app.query('getTasks', {
  fn: { import: 'getTasks', from: '@src/queries' },
  entities: ['Task']
})

app.api('payments', {
  fn: { import: 'paymentStripe', from: '@src/apis' },
  httpRoute: (POST, '/payment/stripe')
});`;

const useInteractiveDemo = () => {
  // State machine: which conversation step, typing progress, phase within step
  const [stepIndex, setStepIndex] = useState(0);
  const [charIndex, setCharIndex] = useState(0);
  const [phase, setPhase] = useState("waiting"); // waiting | typing | responding | responded | deploying | live | done
  const [history, setHistory] = useState([]); // completed exchanges
  const [responseLineIndex, setResponseLineIndex] = useState(0); // which response line we're streaming
  const [responseCharIndex, setResponseCharIndex] = useState(0); // char position within current response line
  const [showFullCode, setShowFullCode] = useState(false);
  const [fullCodeFlash, setFullCodeFlash] = useState(false);
  const [showSlack, setShowSlack] = useState(false);
  const [slackFlash, setSlackFlash] = useState(false);
  const [showReplay, setShowReplay] = useState(false);
  const [dotCount, setDotCount] = useState(0);
  const containerRef = useRef(null);
  const timeoutRef = useRef(null);
  const deployTimerRef = useRef(null);

  const step = conversation[stepIndex];

  // Start animation when scrolled into view
  useEffect(() => {
    if (phase !== "waiting") return;
    const el = containerRef.current;
    if (!el) return;

    const observer = new IntersectionObserver(
      ([entry]) => {
        if (entry.isIntersecting) {
          setPhase("typing");
          observer.disconnect();
        }
      },
      { threshold: 0.95 }
    );
    observer.observe(el);
    return () => observer.disconnect();
  }, [phase]);

  const triggerEvent = (evt) => {
    if (evt === "showFullCode") {
      setShowFullCode(true);
      setFullCodeFlash(true);
      setTimeout(() => setFullCodeFlash(false), HIGHLIGHT_DURATION);
    } else if (evt === "addSlack") {
      setShowSlack(true);
      setSlackFlash(true);
      setTimeout(() => setSlackFlash(false), HIGHLIGHT_DURATION);
    }
  };

  const handleReplay = () => {
    clearTimeout(deployTimerRef.current);
    setHistory([]);
    setStepIndex(0);
    setCharIndex(0);
    setResponseLineIndex(0);
    setResponseCharIndex(0);
    setDotCount(0);
    setPhase("typing");
    setShowFullCode(false);
    setShowSlack(false);
    setShowReplay(false);
  };

  useEffect(() => {
    const prompt = step?.prompt;
    if (!step || phase === "done" || phase === "waiting") return;

    switch (phase) {
      case "typing":
        if (charIndex < prompt.length) {
          timeoutRef.current = setTimeout(() => {
            setCharIndex(charIndex + 1);
          }, TYPING_SPEED);
        } else {
          timeoutRef.current = setTimeout(() => {
            setResponseLineIndex(0);
            setResponseCharIndex(0);
            setPhase("responding");
          }, RESPONSE_LINE_PAUSE);
        }
        break;

      case "responding": {
        const currentLine = step.response[responseLineIndex];
        if (responseCharIndex < currentLine.length) {
          timeoutRef.current = setTimeout(() => {
            setResponseCharIndex(responseCharIndex + 1);
          }, RESPONSE_TYPING_SPEED);
        } else if (responseLineIndex < step.response.length - 1) {
          timeoutRef.current = setTimeout(() => {
            setResponseLineIndex(responseLineIndex + 1);
            setResponseCharIndex(0);
          }, RESPONSE_LINE_PAUSE);
        } else {
          if (step.event) triggerEvent(step.event);
          setPhase("responded");
        }
        break;
      }

      case "responded":
        if (step.deploy) {
          timeoutRef.current = setTimeout(() => {
            setDotCount(1);
            setPhase("deploying");
            deployTimerRef.current = setTimeout(() => {
              setPhase("live");
            }, DEPLOY_DURATION);
          }, RESPONSE_LINE_PAUSE);
        } else {
          timeoutRef.current = setTimeout(() => {
            setHistory((h) => [
              ...h,
              { prompt: step.prompt, response: step.response },
            ]);
            setCharIndex(0);
            setResponseLineIndex(0);
            setResponseCharIndex(0);
            setPhase("typing");
            setStepIndex(stepIndex + 1);
          }, PAUSE_BETWEEN);
        }
        break;

      case "deploying":
        timeoutRef.current = setTimeout(() => {
          setDotCount((d) => (d % 3) + 1);
        }, 400);
        break;

      case "live":
        timeoutRef.current = setTimeout(() => {
          setPhase("done");
          setTimeout(() => setShowReplay(true), REPLAY_DELAY);
        }, LIVE_DURATION);
        break;
    }

    return () => clearTimeout(timeoutRef.current);
  }, [phase, charIndex, stepIndex, dotCount, responseLineIndex, responseCharIndex]);

  const terminal = (
      <div ref={containerRef} className="relative flex flex-col overflow-hidden border border-neutral-300">
        <div className="flex items-center gap-2 bg-neutral-800 px-3 py-1.5">
          <div className="h-2 w-2 rounded-full bg-red-400" />
          <div className="h-2 w-2 rounded-full bg-yellow-400" />
          <div className="h-2 w-2 rounded-full bg-green-400" />
          <span className="ml-2 text-xs text-neutral-500">terminal</span>
        </div>
        <div className="h-[400px] overflow-hidden bg-neutral-900 px-4 py-3 font-mono text-md leading-relaxed text-neutral-100">
          {/* History */}
          {history.map((h, i) => (
            <div key={i} className="mb-2">
              <div>
                <span className="text-yellow-400">{i === 0 ? "$" : ">"}</span>{" "}
                {h.prompt}
              </div>
              {h.response && h.response.map((line, j) => (
                <div key={j} className="text-neutral-400">{line}</div>
              ))}
            </div>
          ))}
          {/* Current */}
          {step && (
            <div className="mb-2">
              <div>
                <span className="text-yellow-400">
                  {stepIndex === 0 ? "$" : ">"}
                </span>{" "}
                {step.prompt.slice(0, charIndex)}
                {phase === "typing" && (
                  <span className="animate-pulse text-yellow-400">|</span>
                )}
              </div>
              {(phase === "responding" ||
                phase === "responded" ||
                phase === "deploying" ||
                phase === "live" ||
                phase === "done") &&
                step.response && step.response.map((line, j) => {
                  if (phase === "responding") {
                    if (j < responseLineIndex) return <div key={j} className="text-neutral-400">{line}</div>;
                    if (j === responseLineIndex) return <div key={j} className="text-neutral-400">{line.slice(0, responseCharIndex)}</div>;
                    return null;
                  }
                  return <div key={j} className="text-neutral-400">{line}</div>;
                })}
              {(phase === "deploying" || phase === "live" || phase === "done") && (
                <div className="text-neutral-400">
                  {"\u23FA Deploying" + ".".repeat(phase === "deploying" ? dotCount : 3)}
                </div>
              )}
              {(phase === "live" || phase === "done") && (
                <div>
                  <br/>
                  {"\u23FA 🚀 Your app is live at "}
                  <a
                    href="https://opensaas.sh"
                    target="_blank"
                    rel="noreferrer"
                    className="text-yellow-400 underline hover:text-yellow-300"
                  >
                    OpenSaaS.sh
                  </a>
                </div>
              )}
            </div>
          )}
        </div>
        {showReplay && (
          <button
            onClick={handleReplay}
            className="absolute bottom-3 right-3 inline-flex items-center gap-1.5 bg-neutral-700 px-3 py-1 text-xs text-neutral-300 transition-colors hover:bg-neutral-600 hover:text-white"
          >
            <RotateCcw size={10} />
            Replay
          </button>
        )}
      </div>

  );

  const blueprint = (
      <div className="flex flex-col overflow-hidden border border-neutral-200">
        <div className="flex items-center gap-2 bg-neutral-50 px-3 py-1.5">
          <div className="h-2 w-2 rounded-full bg-red-400" />
          <div className="h-2 w-2 rounded-full bg-yellow-400" />
          <div className="h-2 w-2 rounded-full bg-green-400" />
          <span className="ml-2 text-xs font-medium text-neutral-400">
            main.wasp.ts
          </span>
        </div>
        <div className="relative h-[400px] overflow-hidden text-sm">
          {fullCodeFlash && (
            <div
              className="pointer-events-none absolute left-0 right-0 bg-yellow-200/30 transition-opacity duration-1000"
              style={{ top: "1.4em", bottom: 0 }}
            />
          )}
          {slackFlash && (
            <div
              className="pointer-events-none absolute left-0 right-0 bg-yellow-200/30 transition-opacity duration-1000"
              style={{ top: "8.2em", height: "1.4em" }}
            />
          )}
          <CodeHighlight
            language="typescript"
            source={
              showSlack ? codeWithSlack : showFullCode ? codeBase : codeEmpty
            }
          />
        </div>
      </div>
  );

  return { terminal, blueprint };
};

// --- Main component ---

const VCWhyWasp = ({ variant }) => {
  const cardClass = vc(variant, {
    base: "h-full rounded-md border border-yellow-500/25 bg-yellow-500/5 p-6",
    v1: "h-full rounded-none border border-yellow-500/25 bg-yellow-500/5 p-6",
    v2: "h-full rounded-none border border-neutral-300 bg-yellow-500/5 p-6",
    v3: "h-full rounded-none border border-neutral-300 bg-yellow-500/5 p-6",
  });

  const iconClass = vc(variant, {
    base: "mb-4 inline-flex h-10 w-10 items-center justify-center rounded-md bg-neutral-700 text-yellow-500",
    v1: "mb-4 inline-flex h-10 w-10 items-center justify-center rounded-none bg-neutral-700 text-yellow-500",
    v2: "mb-4 inline-flex h-10 w-10 items-center justify-center rounded-none bg-neutral-700 text-yellow-500",
    v3: "mb-4 inline-flex h-10 w-10 items-center justify-center rounded-none border border-neutral-600 bg-neutral-700 text-yellow-500",
  });

  const Wrapper = variant === "v2" || variant === "v3" ? CrosshairCard : ({ children }) => <>{children}</>;

  const { terminal, blueprint } = useInteractiveDemo();

  if (variant === "v3") {
    return (
      <VCSection variant={variant} id="how-wasp-works">
        <div className="mx-auto max-w-3xl text-center">
          <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
            The Framework for the{" "}
            <span className="underline decoration-yellow-500">
              Agentic Era
            </span>
          </h2>
        </div>

        {/* Top: terminal left, blueprint right */}
        <div className="mt-10 grid grid-cols-1 gap-3 lg:grid-cols-2">
          {terminal}
          {blueprint}
        </div>

        {/* Bottom: benefit cards */}
        <div className="mt-8 grid grid-cols-1 gap-3 md:grid-cols-2 lg:grid-cols-3">
          {benefits.map((benefit, idx) => (
            <div key={idx} className="border border-neutral-300 bg-yellow-500/5 p-4">
              <div className="flex items-start gap-3">
                <div className="inline-flex h-8 w-8 shrink-0 items-center justify-center rounded-none border border-neutral-600 bg-neutral-700 text-yellow-500">
                  <benefit.Icon size={16} />
                </div>
                <div>
                  <h3 className="text-sm font-semibold text-neutral-700">
                    {benefit.v3Title}
                  </h3>
                  <p className="mt-1 text-sm text-neutral-500">
                    {benefit.v3Description}
                  </p>
                  {benefit.url && (
                    <Link to={benefit.url}>
                      <span className="group mt-2 inline-flex items-center gap-1 text-xs text-neutral-400 hover:text-neutral-600">
                        Learn more
                        <ArrowRight size={10} className="transition-transform group-hover:translate-x-0.5" />
                      </span>
                    </Link>
                  )}
                </div>
              </div>
            </div>
          ))}
        </div>
      </VCSection>
    );
  }

  return (
    <VCSection variant={variant} id="how-wasp-works">
      <div className="mx-auto max-w-3xl text-center">
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          Declarative config. Zero architectural debates.{" "}
          <span className="underline decoration-yellow-500">
            All business logic.
          </span>
        </h2>
      </div>

      <div className="mt-16 grid grid-cols-1 gap-8 md:grid-cols-2">
        {benefits.map((benefit, idx) => (
          <Wrapper key={idx} className="h-full">
            <div className={cardClass}>
              <div className={iconClass}>
                <benefit.Icon size={24} />
              </div>
              <h3 className="mb-2 text-lg font-semibold text-neutral-700">
                {benefit.title}
              </h3>
              <p className="text-neutral-500">{benefit.description}</p>
            </div>
          </Wrapper>
        ))}
      </div>
    </VCSection>
  );
};

export default VCWhyWasp;
