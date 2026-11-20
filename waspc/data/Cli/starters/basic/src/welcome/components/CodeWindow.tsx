// A file or terminal preview. Lines starting with `$ ` are shown as commands.
export function CodeWindow({ title, code }: { title: string; code: string }) {
  return (
    <figure className="w-full overflow-hidden border-2 border-black bg-white">
      <figcaption className="border-b-2 border-black px-4 py-2 text-xs text-gray">
        [<span className="bg-yellow px-1 text-black">{title}</span>]
      </figcaption>
      <pre className="overflow-x-auto p-4 text-xs leading-relaxed">
        {code.split("\n").map((line, index) => (
          <div key={index}>
            {line.startsWith("$ ") ? (
              <>
                <span className="text-yellow-dark select-none">$ </span>
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
