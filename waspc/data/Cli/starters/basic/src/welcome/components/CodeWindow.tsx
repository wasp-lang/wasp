// A file or terminal preview. Lines starting with `$ ` are shown as commands.
export function CodeWindow({ title, code }: { title: string; code: string }) {
  return (
    <figure className="w-full overflow-hidden border border-neutral-900 bg-white">
      <figcaption className="border-b border-neutral-900 px-4 py-2 font-mono text-xs text-neutral-600">
        [<span className="bg-primary-200 px-1 text-neutral-900">{title}</span>]
      </figcaption>
      <pre className="overflow-x-auto p-4 font-mono text-xs leading-relaxed text-neutral-800">
        {code.split("\n").map((line, index) => (
          <div key={index}>
            {line.startsWith("$ ") ? (
              <>
                <span className="text-primary-600 select-none">$ </span>
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
