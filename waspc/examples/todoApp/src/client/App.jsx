import logout from "@wasp/auth/logout.js";
import useAuth from "@wasp/auth/useAuth.js";

import "./Main.css";

export function App({ children }) {
    const { data: user } = useAuth();

    return (
        <div className="app border-spacing-2 p-4">
            <header className="flex justify-between">
                <h1 className="font-bold text-4xl mb-5">ToDo Example App</h1>
                {user && (
                    <div>
                        <button className="btn btn-blue" onClick={logout}>
                            Logout
                        </button>
                    </div>
                )}
            </header>
            <main>{children}</main>
            <footer className="mt-8 text-center">Created with Wasp</footer>
        </div>
    );
}
