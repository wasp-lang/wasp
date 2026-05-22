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

const Badge = ({ text }) => (
  <div className="mb-3 inline-block font-mono text-base font-semibold text-wasp-black">
    <span className="bg-wasp-yellow px-1.5 py-0.5">// {text}</span>
  </div>
);

const Properties = () => (
  <SectionContainer className="pb-12 pt-4 lg:pt-16">
    <Badge text="properties" />
    <div className="grid grid-cols-1 gap-6 lg:grid-cols-3">
      {properties.map((p, i) => (
        <div
          key={p.title}
          className="border-2 border-wasp-black bg-wasp-yellow-light p-6"
        >
          <h3 className="mb-3 flex items-center gap-2 font-mono text-lg font-bold uppercase text-wasp-black">
            <span className="bg-wasp-yellow px-1.5 text-wasp-black">
              {String(i + 1).padStart(2, "0")}
            </span>
            {p.title}
          </h3>
          <p className="font-mono text-sm leading-relaxed text-wasp-g6">
            {p.body}
          </p>
        </div>
      ))}
    </div>
  </SectionContainer>
);

export default Properties;
