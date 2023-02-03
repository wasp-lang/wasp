import './Main.css';
import React from 'react';
import getTotalWork from '@wasp/queries/getTotalWork';
import { useQuery } from '@wasp/queries';
import Chart from './Chart';

const MainPage = () => {
  const { data: work, isError, error } = useQuery(getTotalWork);

  if (isError) return <p>Error: {error.message}</p>;

  if (work)
    return (
      <main>
        <div>
          <Chart work={work} />
        </div>
      </main>
    );

  return <p>Loading...</p>;
};
export default MainPage;
