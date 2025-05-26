import { Link } from "wasp/client/router";

export function HomePage() {
  return (
    <div className="flex flex-col items-center justify-center min-h-80">
      <div className="space-y-2 text-center">
        <h1 className="text-2xl font-bold mb-4">Kitchen sink app</h1>
        <div>
          <Link to="/tasks">Tasks</Link>
        </div>
        <div>
          <Link to="/serialization">Serialization Test</Link>
        </div>
      </div>
    </div>
  );
}
