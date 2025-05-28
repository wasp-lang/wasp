import { Link, type Routes } from "wasp/client/router";
import { SimplePageContainer } from "../components/SimplePageContainer";

export function HomePage() {
  return (
    <SimplePageContainer>
      <div className="max-w-4xl mx-auto">
        <div className="grid grid-cols-1 gap-4 mb-8">
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
    </SimplePageContainer>
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
      className="group relative card hover:shadow-lg hover:-translate-y-1 hover:border-indigo-300 flex gap-4"
    >
      {icon && <div className="text-3xl mb-4">{icon}</div>}

      <div className="space-y-2">
        <h3 className="text-xl font-semibold text-gray-900 group-hover:text-indigo-600 transition-colors">
          {title} {!isAuthRequired && <PublicFeature />}
        </h3>
        {description && (
          <p className="text-gray-600 text-sm leading-relaxed">{description}</p>
        )}
      </div>
    </Link>
  );
}

function PublicFeature() {
  return (
    <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 text-green-800 border border-green-200">
      Public
    </span>
  );
}
