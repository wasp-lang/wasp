import { useState } from "react";
import SectionContainer from "./Layouts/SectionContainer";

const properties = [
  {
    title: "High Level",
    body: "A uniquely powerful config layer serves as a backbone that connects all parts of the stack (FE, BE, DB, deployment, ...).",
  },
  {
    title: "Batteries Included",
    body: "Like Rails and Laravel, focus on your app, not your framework. Auth, jobs, email, deploy and much more built in.",
  },
  {
    title: "Fully Yours",
    body: "Open source, no lock-in. No provider or cloud platform dependency. Host anywhere.",
  },
];

/* ─────────── Badge styles to cycle through ─────────── */

const badgeBase =
  "inline-block bg-wasp-yellow px-4 py-1.5 font-mono text-xs font-bold uppercase tracking-widest text-wasp-black";

const badgeStyles = [
  {
    name: "Solid 2px black border (button-like)",
    render: (text) => (
      <div className={`${badgeBase} border-2 border-wasp-black`}>{text}</div>
    ),
  },
  {
    name: "Dashed 2px black border",
    render: (text) => (
      <div className={`${badgeBase} border-2 border-dashed border-wasp-black`}>
        {text}
      </div>
    ),
  },
  {
    name: "Corner marks (schematic ticks)",
    render: (text) => (
      <div className="relative inline-block">
        <div className={badgeBase}>{text}</div>
        {/* 4 corner L-marks */}
        <span className="pointer-events-none absolute -left-1 -top-1 h-2 w-2 border-l-2 border-t-2 border-wasp-black" />
        <span className="pointer-events-none absolute -right-1 -top-1 h-2 w-2 border-r-2 border-t-2 border-wasp-black" />
        <span className="pointer-events-none absolute -bottom-1 -left-1 h-2 w-2 border-b-2 border-l-2 border-wasp-black" />
        <span className="pointer-events-none absolute -bottom-1 -right-1 h-2 w-2 border-b-2 border-r-2 border-wasp-black" />
      </div>
    ),
  },
  {
    name: "Outline only (no fill)",
    render: (text) => (
      <div
        className={`inline-block border-2 border-wasp-yellow-dark bg-transparent px-4 py-1.5 font-mono text-xs font-bold uppercase tracking-widest text-wasp-yellow-dark`}
      >
        {text}
      </div>
    ),
  },
  {
    name: "Brackets only (no border, no fill)",
    render: (text) => (
      <div className="inline-block px-2 py-1.5 font-mono text-xs font-bold uppercase tracking-widest text-wasp-black">
        [ <span className="text-wasp-g5">{text}</span> ]
      </div>
    ),
  },
  {
    name: "Borderless (yellow fill only)",
    render: (text) => <div className={badgeBase}>{text}</div>,
  },
];

const BadgePicker = ({ text, defaultIdx = 0 }) => {
  const [idx, setIdx] = useState(defaultIdx);
  const [hover, setHover] = useState(false);

  const cycle = (delta) => setIdx((i) => (i + delta + badgeStyles.length) % badgeStyles.length);

  return (
    <div
      onMouseEnter={() => setHover(true)}
      onMouseLeave={() => setHover(false)}
      className="relative mb-6 inline-block"
    >
      {/* Arrow buttons hug the badge (w-10 = 40px, touching left/right edges)
          so there's no gap that triggers mouseleave when moving toward them. */}
      {hover && (
        <>
          <button
            type="button"
            onClick={() => cycle(-1)}
            className="absolute -left-10 top-0 flex h-full w-10 items-center justify-center border-0 bg-transparent p-0 font-mono text-sm text-wasp-g6 hover:text-wasp-black"
            aria-label="Previous badge style"
          >
            ←
          </button>
          <button
            type="button"
            onClick={() => cycle(1)}
            className="absolute -right-10 top-0 flex h-full w-10 items-center justify-center border-0 bg-transparent p-0 font-mono text-sm text-wasp-g6 hover:text-wasp-black"
            aria-label="Next badge style"
          >
            →
          </button>
          <div className="absolute -bottom-5 left-0 whitespace-nowrap font-mono text-[10px] uppercase tracking-wider text-wasp-g5">
            {badgeStyles[idx].name}
          </div>
        </>
      )}
      {badgeStyles[idx].render(text)}
    </div>
  );
};

/* ─────────── Variant 1 — Bordered cards ─────────── */
const Variant1 = () => (
  <SectionContainer className="pt-12 pb-12">
    <BadgePicker text="// Properties" defaultIdx={5} />
    <div className="grid grid-cols-1 gap-6 lg:grid-cols-3">
      {properties.map((p) => (
        <div key={p.title} className="border border-wasp-g3 p-6">
          <h3 className="mb-3 flex items-center gap-2 font-mono text-lg font-bold uppercase text-wasp-black">
            <span className="inline-block h-3 w-3 border border-wasp-black bg-wasp-yellow" />
            {p.title}
          </h3>
          <p className="font-mono text-sm leading-relaxed text-wasp-g6">{p.body}</p>
        </div>
      ))}
    </div>
  </SectionContainer>
);

/* ─────────── Variant 2 — Borderless cards (dividers) ─────────── */
const Variant2 = () => (
  <SectionContainer className="pt-12 pb-12">
    <BadgePicker text="// Properties" defaultIdx={5} />
    <div className="grid grid-cols-1 gap-8 lg:grid-cols-3 lg:divide-x lg:divide-wasp-g3">
      {properties.map((p) => (
        <div key={p.title} className="lg:px-6 lg:first:pl-0 lg:last:pr-0">
          <h3 className="mb-3 flex items-center gap-2 font-mono text-lg font-bold uppercase text-wasp-black">
            <span className="inline-block h-3 w-3 border border-wasp-black bg-wasp-yellow" />
            {p.title}
          </h3>
          <p className="font-mono text-sm leading-relaxed text-wasp-g6">{p.body}</p>
        </div>
      ))}
    </div>
  </SectionContainer>
);

const Properties = () => (
  <>
    <div className="border-t border-wasp-g2">
      <div className="mx-auto max-w-[1400px] px-6 pt-6 font-mono text-xs uppercase tracking-[2px] text-wasp-g5">
        Variant 1 — Bordered cards
      </div>
      <Variant1 />
    </div>
    <div className="border-t border-wasp-g2">
      <div className="mx-auto max-w-[1400px] px-6 pt-6 font-mono text-xs uppercase tracking-[2px] text-wasp-g5">
        Variant 2 — Borderless cards (dividers)
      </div>
      <Variant2 />
    </div>
  </>
);

export default Properties;
