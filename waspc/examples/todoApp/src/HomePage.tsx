import { Link, type Routes } from "wasp/client/router";

export function HomePage() {
  return (
    <div className="container mx-auto px-4 py-12">
      <div className="max-w-4xl mx-auto">
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6 mb-8">
          <FeatureCard
            to="/tasks"
            title="Operations"
            description="Task management and operations"
            isAuthRequired={true}
            icon="⚙️"
          />
          <FeatureCard
            to="/custom-signup"
            title="Custom Signup"
            description="User registration and authentication"
            icon="👤"
          />
          <FeatureCard
            to="/apis"
            title="Custom APIs"
            description="REST endpoints and data handling"
            icon="🔌"
          />
          <FeatureCard
            to="/chat"
            title="Websockets"
            description="Real-time communication"
            isAuthRequired={true}
            icon="💬"
          />
          <FeatureCard
            to="/crud"
            title="Automatic CRUD"
            description="Database operations made simple"
            isAuthRequired={true}
            icon="📊"
          />
          <FeatureCard
            to="/streaming"
            title="Streaming"
            description="Real-time data streaming"
            isAuthRequired={true}
            icon="📡"
          />
          <FeatureCard
            to="/serialization"
            title="Serialization Test"
            description="Data serialization examples"
            icon="🔄"
          />
        </div>
      </div>
    </div>
  );
}

type FeatureCardProps = {
  title: string;
  description?: string;
  icon?: string;
  isAuthRequired?: boolean;
  to: string;
} & Routes;

function FeatureCard({
  title,
  description,
  icon,
  isAuthRequired,
  ...routeProps
}: FeatureCardProps) {
  return (
    <Link
      {...routeProps}
      className="group relative bg-white rounded-xl border border-gray-200 p-6 shadow-sm hover:shadow-lg transition-all duration-200 hover:-translate-y-1 hover:border-indigo-300 no-underline"
    >
      {icon && <div className="text-3xl mb-4">{icon}</div>}

      <div className="space-y-2">
        <h3 className="text-xl font-semibold text-gray-900 group-hover:text-indigo-600 transition-colors">
          {title} {isAuthRequired && <RequiresAuth />}
        </h3>
        {description && (
          <p className="text-gray-600 text-sm leading-relaxed">{description}</p>
        )}
      </div>

      <div className="absolute top-4 right-4 opacity-0 group-hover:opacity-100 transition-opacity">
        <svg
          className="w-5 h-5 text-indigo-400"
          fill="none"
          stroke="currentColor"
          viewBox="0 0 24 24"
        >
          <path
            strokeLinecap="round"
            strokeLinejoin="round"
            strokeWidth={2}
            d="M9 5l7 7-7 7"
          />
        </svg>
      </div>
    </Link>
  );
}

function RequiresAuth() {
  return (
    <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-amber-100 text-amber-800 border border-amber-200">
      <svg
        className="w-3 h-3 mr-1"
        fill="none"
        stroke="currentColor"
        viewBox="0 0 24 24"
      >
        <path
          strokeLinecap="round"
          strokeLinejoin="round"
          strokeWidth={2}
          d="M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z"
        />
      </svg>
      Auth
    </span>
  );
}
