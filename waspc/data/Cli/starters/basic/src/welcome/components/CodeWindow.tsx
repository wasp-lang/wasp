// A file or terminal preview. Lines starting with `$ ` are shown as commands.
export function CodeWindow({ title, code }: { title: string; code: string }) {
  return (
    <figure className="border-line bg-canvas w-full overflow-hidden rounded-md border">
      <figcaption className="border-line text-gray border-b px-4 py-2 font-mono text-xs">
        {title}
      </figcaption>
      <pre className="overflow-x-auto p-4 text-xs leading-relaxed [overflow-wrap:normal]">
        {code.split("\n").map((line, index) => (
          <div key={index}>
            {line.startsWith("$ ") ? (
              <>
                <span className="text-gray select-none">$ </span>
                {line.slice(2)}
              </>
            ) : (
              line || " "
            )}
          </div>
        ))}
      </pre>
    </figure>
  );
}
