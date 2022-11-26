export const errorMessage = (e) => {
  return (
    <span>
      Error: {e.message}
      {e.data?.message && <span style={{display: 'block'}}>Details: {e.data.message}</span>}
    </span>
  );
};
