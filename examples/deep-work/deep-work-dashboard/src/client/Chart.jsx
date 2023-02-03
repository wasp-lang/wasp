import React from 'react';
import waspLogo from './waspLogo.png';
import { useQuery } from '@wasp/queries';
import getWorkByYear from '@wasp/queries/getWorkByYear';
import getTotalHours from '@wasp/queries/getTotalHours';
import { BarChart, Bar, LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, Label } from 'recharts';

const months = [
  'Total',
  'January',
  'February',
  'March',
  'April',
  'May',
  'June',
  'July',
  'August',
  'September',
  'October',
  'November',
  'December',
];

function getUniqueYears(work) {
  const years = work.map((w) => {
    const date = new Date(w.timeStarted);
    return date.getFullYear();
  });
  const uniqueYears = [...new Set(years)];
  return uniqueYears;
}

function getWeekNumber(date) {
  const start = new Date(date.getFullYear(), 0, 0);
  const diff = date - start;
  const oneDay = 1000 * 60 * 60 * 24;
  const day = Math.floor(diff / oneDay);
  return Math.floor(day / 7) + 1;
}

function reducedWeeks(work, monthToView) {
  const reducedWeeks = work.reduce((acc, x) => {
    const date = new Date(x.timeStarted);
    const week = getWeekNumber(date);
    const month = date.getMonth();

    if (monthToView !== 0 && monthToView !== month + 1) {
      return acc;
    }

    let weekObject = acc.find((item) => item.week === week);

    if (!weekObject) {
      weekObject = { week: week, month: month, minutes: 0 };
      acc.push(weekObject);
    }
    weekObject.minutes += Number(x.minutes);
    weekObject.month = month;
    return acc;
  }, []);
  if (monthToView !== 0) {
    return reducedWeeks.filter((week) => {
      return week.month + 1 === monthToView;
    });
  }
  return reducedWeeks;
}

function Chart({ work }) {
  const [isDataSet, setIsDataSet] = React.useState(false);
  const [uniqueYears, setUniqueYears] = React.useState([]);
  const [parsedDays, setparsedDays] = React.useState([]);
  const [parsedWeeks, setParsedWeeks] = React.useState([]);
  const [yearToView, setYearToView] = React.useState(new Date().getFullYear());
  const [monthToView, setMonthToView] = React.useState(new Date().getMonth() + 1);

  const { data: workByYear } = useQuery(getWorkByYear, { year: yearToView });
  const { data: totalMinutes } = useQuery(getTotalHours);

  React.useMemo(() => {
    setUniqueYears(getUniqueYears(work));
  }, [work]);

  React.useEffect(() => {
    if (work && workByYear) {
      setIsDataSet(false);

      if (yearToView === 0) {
        setMonthToView(0);
      }

      /** MIN PER DAY */
      if (monthToView !== 0 && yearToView !== 0) {
        const daysPerMonth = work.filter((day) => {
          const timestamp = Date.parse(day.timeStarted);
          const date = new Date(timestamp);
          const month = date.getMonth();

          return month + 1 === monthToView;
        });
        if (daysPerMonth?.length > 0) {
          setparsedDays(daysPerMonth);
        } else {
          setparsedDays([]);
        }
      } else {
        setparsedDays(work);
      }

      /** MIN PER WEEK */
      const weeksPerMonth = reducedWeeks(work, monthToView);
      if (weeksPerMonth?.length > 0) {
        setParsedWeeks(weeksPerMonth);
      } else {
        setParsedWeeks([]);
      }

      setIsDataSet(true);
    }
  }, [work, workByYear, monthToView, yearToView]);

  return (
    <>
      <div style={{ display: 'flex', justifyContent: 'center', alignItems: 'center' }}>
        <h1>Total Deep Work Hours: {totalMinutes && (totalMinutes / 60).toFixed(2)}</h1>
        <img src={waspLogo} alt='Wasp Logo' height={'40px'} style={{ marginLeft: '15px' }}/>
      </div>

      <select
        value={monthToView}
        disabled={yearToView === 0}
        onChange={(e) => {
          setMonthToView(Number(e.target.value));
        }}
      >
        <option value={0}>Total</option>
        <option value={1}>January</option>
        <option value={2}>February</option>
        <option value={3}>March</option>
        <option value={4}>April</option>
        <option value={5}>May</option>
        <option value={6}>June</option>
        <option value={7}>July</option>
        <option value={8}>August</option>
        <option value={9}>September</option>
        <option value={10}>October</option>
        <option value={11}>November</option>
        <option value={12}>December</option>
      </select>

      <select value={yearToView} onChange={(e) => setYearToView(Number(e.target.value))}>
        <option value={0}>Total</option>
        {uniqueYears.map((year) => (
          <option key={year} value={year}>
            {year}
          </option>
        ))}
      </select>

      {isDataSet && (
        <>
          <h2>Total Minutes per Day {`(${months[monthToView]})`}</h2>
          <LineChart width={1000} height={700} data={parsedDays} margin={{ top: 5, right: 30, left: 20, bottom: 25 }}>
            <CartesianGrid strokeDasharray='3 3' />
            <XAxis dataKey='timeStarted'>
              <Label value='Date' offset={10} position='bottom' />
            </XAxis>

            <YAxis dataKey='minutes'>
              <Label value='Minutes' angle={-90} position='insideLeft' />
            </YAxis>
            <Tooltip />
            {/* <Legend /> */}
            <Line dataKey='minutes' stroke='#8884d8' />
          </LineChart>
          <br />

          <h2>Total Minutes per Week {`(${months[monthToView]})`}</h2>
          <BarChart width={1000} height={700} data={parsedWeeks} margin={{ top: 5, right: 30, left: 20, bottom: 25 }}>
            <CartesianGrid strokeDasharray='3 3' />
            <XAxis dataKey='week'>
              <Label value='# of Week (per Year) ' offset={10} position='bottom' />
            </XAxis>
            <YAxis dataKey='minutes'>
              <Label value='Minutes' angle={-90} position='insideLeft' />
            </YAxis>
            <Tooltip />
            {/* <Legend /> */}
            <Bar dataKey='minutes' fill='#8884d8' />
          </BarChart>
          <h2>Raw Data</h2>
          {work.map((x) => (
            <p>
              {JSON.stringify(x)}
              <br />
            </p>
          ))}
        </>
      )}
    </>
  );
}

export default Chart;
