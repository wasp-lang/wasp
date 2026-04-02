import Link from "@docusaurus/Link";
import { VCSection } from "./vcWrappers";

const testimonials = [
  {
    text: "Constraints are the feature. Wasp tells me how to do it, so the agent doesn't try to do 50 things. I tried 10 frameworks and Wasp is the only one that really stands out!",
    name: "Marcel Coetzee",
    role: "Data Analyst & Founder @ MoonCoon & Hireveld",
    img: "https://github.com/Pipboyguy.png",
    url: "blog/2026/03/23/hireveld-from-10-stacks-to-production-with-wasp",
    aiTools: ["Claude Code"],
  },
  {
    text: "If you start with Wasp, 80% of the pains of vibe coding are taken care of for you already.",
    name: "Kenny Rogers",
    role: "Dev Rel & AI-First Educator",
    img: "https://github.com/kenrogers.png",
    url: "https://x.com/KenTheRogers",
    aiTools: ["Claude Code"],
  },
  {
    text: "Wasp is very LLM friendly.",
    name: "Anshula Chowdhury",
    role: "Founder @ SREDwise \u2014 Built with Wasp and acquired for $100k",
    img: "https://media.licdn.com/dms/image/v2/C5603AQG7DLgxpU-CBg/profile-displayphoto-shrink_800_800/profile-displayphoto-shrink_800_800/0/1594820855370?e=1776902400&v=beta&t=ISYFy4Oib6f6Bb_aL89yihKb-Ebx5HAnXIdafNLDdXY",
    url: "https://www.linkedin.com/in/anchowdhury/",
    aiTools: ["Claude Code"],
  },
  {
    text: "With Next.js App Router, I was constantly fighting the LLM to get the syntax right. With Wasp, you can ask Claude... and it actually knows.",
    name: "TK Garrett",
    role: "Founder @ PlotTree",
    img: "https://plottree.ai/hand-with-quill.svg",
    url: "https://plottree.ai",
    aiTools: ["Claude", "Cursor"],
  },
  {
    text: "Everything just works on the first try \u2014 minimal intervention needed for anything. When you open the config file, you immediately see the full picture \u2014 app, route, page, that's it.",
    name: "Hrvoje Pavlinovic",
    role: "Senior Software Engineer & Agency Owner",
    img: "https://github.com/hrvojepavlinovic.png",
    url: "https://hrvoje.pavlinovic.com",
    aiTools: ["Claude Code", "Codex"],
  },
  {
    text: "Wasp made me feel secure, like I am not cutting any corners. Just using AI without it would make it harder to sleep at night.",
    name: "Robbie Artress",
    role: "Audio Engineer & Founder of PeakMastering.com",
    img: "https://pbs.twimg.com/profile_images/1938395157109342208/PtnrrFe7_400x400.jpg",
    url: "https://peakmastering.com",
    aiTools: ["Claude Code", "Codex"],
  },
  {
    text: "Two years ago I couldn\u2019t imagine building an application. So Wasp is like... kind of magic for me.",
    name: "Leo Golubyov",
    role: "Founder @ messync.com",
    img: "https://github.com/NeroxTGC.png",
    url: "https://messync.com",
    aiTools: ["Cursor"],
  },
];

const TestimonialCard = ({ url, text, name, role, img, aiTools }) => {
  const cardClass = "rounded-none border-l-2 border-yellow-500 bg-white p-6";

  return (
    <Link to={url} className="flex h-full">
      <div className={`${cardClass} flex h-full w-full flex-col`}>
        <div className="flex">
          {img ? (
            <img
              className="h-[45px] w-[45px] rounded-full object-cover"
              src={img}
              width={45}
              height={45}
              alt={name}
            />
          ) : (
            <div className="flex h-[45px] w-[45px] items-center justify-center rounded-full bg-yellow-100 text-lg font-semibold text-yellow-600">
              {name.charAt(0)}
            </div>
          )}
          <div className="flex w-full justify-between pl-3">
            <div>
              <h6 className="text-md font-semibold text-neutral-700">{name}</h6>
              <p className="text-sm text-neutral-500">{role}</p>
            </div>
            {aiTools && aiTools.length > 0 && (
              <div className="flex items-start gap-1">
                {aiTools.map((tool) => (
                  <span
                    key={tool}
                    className="whitespace-nowrap rounded bg-neutral-100 px-1.5 py-0.5 text-xs text-neutral-500"
                  >
                    {tool}
                  </span>
                ))}
              </div>
            )}
          </div>
        </div>
        <div className="mt-3 flex-1 whitespace-pre-wrap text-neutral-700">
          &ldquo;{text}&rdquo;
        </div>
      </div>
    </Link>
  );
};

const VCTestimonials = () => {
  return (
    <VCSection className="space-y-16">
      <div className="grid grid-cols-12">
        <div className="col-span-12 text-center">
          <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
            What Vibe Coders Are Saying
          </h2>
          <p className="text-neutral-500">
            Real users building real products with Wasp + AI coding tools.{" "}
          </p>
        </div>
      </div>

      <div className="grid grid-cols-1 gap-4 md:grid-cols-2">
        {testimonials.map((t, idx) => (
          <TestimonialCard key={idx} {...t} />
        ))}
        <Link to="https://discord.gg/rzdnErX" className="flex h-full">
          <div className="flex h-full flex-1 flex-col items-center justify-center rounded-none border-l-2 border-yellow-500 bg-white p-6">
            <img
              src="img/discord-logo.webp"
              alt="Discord"
              className="mb-3 h-10 w-10"
            />
            <p className="text-md italic text-neutral-500">
              Join our active community of 4.5k+ builders
            </p>
          </div>
        </Link>
      </div>

      <div className="flex flex-wrap items-center justify-center gap-6">
        <a
          href="https://github.com/wasp-lang/wasp"
          target="_blank"
          rel="noreferrer"
          className="text-sm text-neutral-500 hover:text-neutral-400"
        >
          13k+ GitHub Stars
        </a>
        <span className="h-4 border-l border-neutral-300" />
        <span className="flex items-center text-sm text-neutral-500">
          Backed by{" "}
          <img
            className="ml-2 w-20"
            src="img/lp/yc-logo-rounded.webp"
            alt="Y Combinator"
          />
        </span>
        <span className="h-4 border-l border-neutral-300" />
        <a
          href="https://discord.gg/rzdnErX"
          target="_blank"
          rel="noreferrer"
          className="text-sm text-neutral-500 hover:text-neutral-400"
        >
          Active Discord Community of 4.5k+ builders
        </a>
      </div>
    </VCSection>
  );
};

export default VCTestimonials;
