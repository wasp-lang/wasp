import Tilt from "react-parallax-tilt";

export function ExampleCard({ idea, useIdea }) {
  return (
    <Tilt
      tiltMaxAngleX={10}
      tiltMaxAngleY={10}
      perspective={1000}
      transitionSpeed={1000}
      scale={1.05}
    >
      <div
        className="mt-2 flex cursor-pointer flex-col items-center rounded-xl bg-slate-50 p-8 transition-all hover:shadow-lg"
        onClick={() => useIdea(idea)}
      >
        <div className="idea w-full">
          <div className="mb-4 flex items-center justify-between">
            <h4 className="mb-1 text-xl font-semibold text-slate-700">
              <span
                className="mr-2 inline-block h-4 w-4 rounded-full"
                style={{ backgroundColor: idea.color.color }}
              ></span>
              {idea.name}
            </h4>
            <button className="button sm gray">Use this idea</button>
          </div>
          <div className="line-clamp-10 text-base leading-relaxed text-slate-500">
            {idea.description.split("\n").map((str) => (
              <p key={str}>{str}</p>
            ))}
          </div>
        </div>
      </div>
    </Tilt>
  );
}
