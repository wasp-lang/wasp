import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";

const properties = [
  {
    title: "High Level",
    body: (
      <>
        A uniquely powerful config layer serves as a backbone that connects all
        parts of the stack -{" "}
        <strong>frontend, backend, database, and deployment</strong>.
      </>
    ),
  },
  {
    title: "Batteries: Included",
    body: (
      <>
        <strong>Like Rails and Laravel</strong>, focus on your app, not your
        framework. Auth, jobs, email, deploy and much more built in.
      </>
    ),
  },
  {
    title: "Fully Yours",
    body: (
      <>
        Open source, no lock-in. No third-party provider or cloud platform
        dependency. <strong>Host anywhere</strong>.
      </>
    ),
  },
];

const Properties = () => (
  <SectionContainer>
    <SectionLabel text="properties" />
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
