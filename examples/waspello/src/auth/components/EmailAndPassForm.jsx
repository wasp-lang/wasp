const inputFieldClasses = `
  box-border w-full border-2 border-neutral-300
  bg-neutral-100/50
  hover:bg-neutral-200
  focus:bg-neutral-100/50 focus:outline-hidden
  focus:border-yellow-500
  transition ease-out duration-200
  h-10 px-2 text-sm placeholder:text-neutral-500
`;
const EmailAndPassForm = (props) => {
  const {
    title,
    submitButtonLabel,
    userField,
    passField,
    setUser,
    setPass,
    handleSignup,
    errorMessage,
  } = props;

  return (
    <div className="w-full text-center">
      <h2 className="text-base font-bold text-neutral-600">{title}</h2>
      <form onSubmit={handleSignup} className="block flex flex-col">
        <input
          className={inputFieldClasses + " mt-5"}
          type="text"
          placeholder="Enter email address"
          value={userField}
          onChange={(e) => setUser(e.target.value)}
        />
        <input
          className={inputFieldClasses + " mt-4"}
          type="password"
          placeholder="Enter password"
          value={passField}
          onChange={(e) => setPass(e.target.value)}
        />
        {errorMessage && (
          <div className="mt-4 w-full rounded-sm border border-red-500 bg-red-100 px-2 py-1 text-center text-red-700">
            {errorMessage}
          </div>
        )}
        <input
          className={`mt-3 h-10 bg-yellow-500 text-sm font-bold text-white hover:bg-yellow-400`}
          type="submit"
          value={submitButtonLabel}
        />
      </form>
    </div>
  );
};

export default EmailAndPassForm;
