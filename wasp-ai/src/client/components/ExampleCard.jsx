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
        className="bg-slate-50 p-8 rounded-xl mt-2 flex flex-col items-center cursor-pointer hover:shadow-lg transition-all"
        onClick={() => useIdea(idea)}
      >
        <div className="idea w-full">
          <div className="flex justify-between items-center mb-4">
            <h4 className="text-xl font-semibold text-slate-700 mb-1">
              <span
                className="inline-block w-4 h-4 rounded-full mr-2"
                style={{ backgroundColor: idea.color.color }}
              ></span>
              {idea.name}
            </h4>
            <button className="button sm gray">Use this idea</button>
          </div>
          <div className="text-base leading-relaxed text-slate-500 line-clamp-[10]">
            {idea.description.split('\n').map(str => <p>{str}</p>)}
          </div>
        </div>
      </div>
    </Tilt>
  );
}
