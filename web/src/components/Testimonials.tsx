import Link from "@docusaurus/Link";
import useBrokenLinks from "@docusaurus/useBrokenLinks";
import classNames from "classnames";
import { ReactNode } from "react";

import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";

const TESTIMONIALS_ID = "testimonials";

interface Testimonial {
  text: ReactNode;
  name: string;
  role?: string;
  img?: string;
  url?: string;
}

/* Wrap a slice of testimonial text to give it subtle bold emphasis. */
const Highlight = ({ children }: { children: ReactNode }) => (
  <span className="font-bold text-wasp-black">{children}</span>
);

const Testimonials = () => {
  // Register anchor so Docusaurus's broken-link checker sees it at build time.
  useBrokenLinks().collectAnchor(TESTIMONIALS_ID);

  return (
    <div className="bg-wasp-yellow-light">
      <SectionContainer id={TESTIMONIALS_ID}>
        <SectionLabel text="testimonials" />
        <TestimonialsGrid />
      </SectionContainer>
    </div>
  );
};

/* Testimonials grid: landscape featured testimonial center + 2 side columns of
   2 testimonials each, separated by 1px solid dividers. */
const TestimonialsGrid = () => (
  <div className="grid grid-cols-1 items-stretch gap-10 lg:grid-cols-12 lg:gap-8">
    <SideColumn side="left" t1={testimonials[0]} t2={testimonials[1]} />
    <div className="lg:col-span-6 lg:self-center">
      <FeaturedTestimonial />
    </div>
    <SideColumn side="right" t1={testimonials[2]} t2={testimonials[3]} />
  </div>
);

/* Side column with two testimonials, separated by a horizontal divider that
   sits at the column's 50% mark. Both columns stretch to the same height (via
   grid `items-stretch`), so the dividers line up across the section. */
const SideColumn = ({
  side,
  t1,
  t2,
}: {
  side: "left" | "right";
  t1: Testimonial;
  t2: Testimonial;
}) => {
  const isLeft = side === "left";
  const innerPadding = isLeft ? "lg:pr-8" : "lg:pl-8";
  const sideBorder = isLeft ? "lg:border-r" : "lg:border-l";
  return (
    <div
      className={classNames(
        "relative flex flex-col gap-10 border-wasp-g3 lg:col-span-3 lg:h-full lg:justify-between lg:gap-0",
        sideBorder,
      )}
    >
      <div className={innerPadding}>
        <SideTestimonial testimonial={t1} />
      </div>
      <div className={innerPadding}>
        <SideTestimonial testimonial={t2} />
      </div>
      <div className="pointer-events-none absolute left-0 right-0 top-1/2 hidden border-t border-wasp-g3 lg:block" />
    </div>
  );
};

/* ─────────── Card components ─────────── */

const Avatar = ({
  img,
  name,
  size = "h-10 w-10",
}: {
  img?: string;
  name: string;
  size?: string;
}) =>
  img ? (
    <img
      src={img}
      alt={name}
      className={`${size} flex-shrink-0 rounded-full object-cover`}
    />
  ) : (
    <div
      className={`${size} flex flex-shrink-0 items-center justify-center rounded-full bg-wasp-white font-mono text-sm font-bold text-wasp-g7`}
    >
      {name.charAt(0)}
    </div>
  );

const SideTestimonial = ({ testimonial }: { testimonial: Testimonial }) => {
  const { name, role, img, text, url } = testimonial;
  const Inner = (
    <article>
      <p className="text-sm leading-relaxed text-wasp-g7">“{text}”</p>
      <div className="mt-4 flex items-center gap-3">
        <Avatar img={img} name={name} />
        <div className="min-w-0 flex-1">
          <div className="font-mono text-xs font-bold text-wasp-black">
            {name}
          </div>
          {role && (
            <div className="font-mono text-[11px] text-wasp-g5">{role}</div>
          )}
        </div>
      </div>
    </article>
  );
  return url ? (
    <Link
      to={url}
      className="block text-wasp-g7 no-underline hover:text-wasp-g7 hover:no-underline"
    >
      {Inner}
    </Link>
  ) : (
    Inner
  );
};

const FeaturedTestimonial = () => (
  <article className="border border-[#1E1F22] bg-[#313338] p-8 lg:py-12">
    <div className="mb-4 flex items-center gap-1.5">
      <span className="font-sans text-2xl font-light leading-none text-[#80848E]">
        #
      </span>
      <span className="text-lg leading-none">🏠</span>
      <span className="font-sans text-base font-bold leading-none text-[#F2F3F5]">
        made-with-wasp
      </span>
    </div>
    <p className="font-sans text-base leading-relaxed text-[#DBDEE1] lg:text-lg">
      Wasp is as{" "}
      <span className="bg-wasp-yellow px-1 text-wasp-black">
        game-changing for me as React has been many years back
      </span>
      . Its simplicity, and how well [it] captures most full-stack engineering
      tasks is{" "}
      <span className="bg-wasp-yellow px-1 text-wasp-black">pure genius</span>.
      I believe Wasp will become{" "}
      <span className="bg-wasp-yellow px-1 text-wasp-black">
        the #1 web technology in just a couple years
      </span>
      . It has everything that most web devs are looking for.
    </p>
    <div className="mt-6 flex items-center gap-3">
      <div className="flex h-12 w-12 flex-shrink-0 items-center justify-center rounded-full bg-[#5865F2] font-sans text-base font-bold text-white">
        W
      </div>
      <div>
        <div className="font-sans text-sm font-bold text-[#F2F3F5]">
          Wasp builder
        </div>
        <div className="font-sans text-xs text-[#949BA4]">via Discord</div>
      </div>
    </div>
  </article>
);

// Order matters: [leftColTop, leftColBottom, rightColTop, rightColBottom]
const testimonials: Testimonial[] = [
  {
    text: (
      <>
        Wasp is <Highlight>by far the best for AI coding</Highlight>. The
        high-level Wasp file keeps me on top of everything.
      </>
    ),
    name: "Marcel Coetzee",
    role: "Founder @ Hireveld",
    img: "https://github.com/Pipboyguy.png",
    url: "/blog/2026/03/29/hireveld-from-10-stacks-to-production-with-wasp",
  },
  {
    text: (
      <>
        If you start with Wasp,{" "}
        <Highlight>80% of the pains of vibe coding are taken care of</Highlight>{" "}
        for you already.
      </>
    ),
    name: "Kenny Rogers",
    role: "Dev Rel & AI-First Educator",
    img: "https://github.com/kenrogers.png",
    url: "https://x.com/KenTheRogers",
  },
  {
    text: (
      <>
        <Highlight>Everything just works on the first try</Highlight>, minimal
        intervention needed for anything.
      </>
    ),
    name: "Hrvoje Pavlinovic",
    role: "Senior Engineer @ Memoato",
    img: "https://github.com/hrvojepavlinovic.png",
    url: "https://memoato.com/",
  },
  {
    text: (
      <>
        Just using AI would make it harder to sleep at night. With Wasp{" "}
        <Highlight>I feel secure</Highlight>, like I'm{" "}
        <Highlight>not cutting any corners</Highlight>.
      </>
    ),
    name: "Robbie Artress",
    role: "Founder @ PeakMastering",
    img: "https://pbs.twimg.com/profile_images/1938395157109342208/PtnrrFe7_400x400.jpg",
    url: "https://peakmastering.com",
  },
];

export default Testimonials;
