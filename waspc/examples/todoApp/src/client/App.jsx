export function App({ children }) {
    return (
        <div className="p-6">
            <header className="mb-6">
                <h1 className="text-3xl font-bold">ToDo App</h1>
            </header>
            {children}
        </div>
    );
}
