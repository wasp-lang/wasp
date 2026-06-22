import { ApexOptions } from "apexcharts";
import { useEffect, useMemo, useState } from "react";
import ReactApexChart from "react-apexcharts";
import { type DailyStatsProps } from "../../../analytics/stats";

const options: ApexOptions = {
  legend: {
    show: false,
    position: "top",
    horizontalAlign: "left",
  },
  colors: ["#3C50E0", "#80CAEE"],
  chart: {
    fontFamily: "system-ui, sans-serif",
    height: 335,
    type: "area",
    dropShadow: {
      enabled: true,
      color: "#623CEA14",
      top: 10,
      blur: 4,
      left: 0,
      opacity: 0.1,
    },

    toolbar: {
      show: false,
    },
  },
  responsive: [
    {
      breakpoint: 1024,
      options: {
        chart: {
          height: 300,
        },
      },
    },
    {
      breakpoint: 1366,
      options: {
        chart: {
          height: 350,
        },
      },
    },
  ],
  stroke: {
    width: [2, 2],
    curve: "straight",
  },
  // labels: {
  //   show: false,
  //   position: "top",
  // },
  grid: {
    xaxis: {
      lines: {
        show: true,
      },
    },
    yaxis: {
      lines: {
        show: true,
      },
    },
  },
  dataLabels: {
    enabled: false,
  },
  markers: {
    size: 4,
    colors: "#fff",
    strokeColors: ["#3056D3", "#80CAEE"],
    strokeWidth: 3,
    strokeOpacity: 0.9,
    strokeDashArray: 0,
    fillOpacity: 1,
    discrete: [],
    hover: {
      size: undefined,
      sizeOffset: 5,
    },
  },
  xaxis: {
    type: "category",
    axisBorder: {
      show: false,
    },
    axisTicks: {
      show: false,
    },
  },
  yaxis: {
    title: {
      style: {
        fontSize: "0px",
      },
    },
    min: 0,
    max: 100,
  },
};

interface ChartOneState {
  series: {
    name: string;
    data: number[];
  }[];
}

export function RevenueAndProfitChart({ weeklyStats }: DailyStatsProps) {
  const dailyRevenueArray = useMemo(() => {
    if (!!weeklyStats && weeklyStats?.length > 0) {
      const sortedWeeks = weeklyStats?.sort((a, b) => {
        return new Date(a.date).getTime() - new Date(b.date).getTime();
      });
      return sortedWeeks.map((stat) => stat.totalRevenue);
    }
  }, [weeklyStats]);

  const daysOfWeekArr = useMemo(() => {
    if (!!weeklyStats && weeklyStats?.length > 0) {
      const datesArr = weeklyStats?.map((stat) => {
        // get day of week, month, and day of month
        const dateArr = stat.date.toString().split(" ");
        return dateArr.slice(0, 3).join(" ");
      });
      return datesArr;
    }
  }, [weeklyStats]);

  const [state, setState] = useState<ChartOneState>({
    series: [
      {
        name: "Profit",
        data: [4, 7, 10, 11, 13, 14, 17],
      },
    ],
  });
  const [chartOptions, setChartOptions] = useState<ApexOptions>(options);

  useEffect(() => {
    if (dailyRevenueArray && dailyRevenueArray.length > 0) {
      // eslint-disable-next-line react-hooks/set-state-in-effect
      setState((prevState) => {
        // Check if a "Revenue" series already exists
        const existingSeriesIndex = prevState.series.findIndex(
          (series) => series.name === "Revenue",
        );

        if (existingSeriesIndex >= 0) {
          // Update existing "Revenue" series data
          return {
            ...prevState,
            series: prevState.series.map((serie, index) => {
              if (index === existingSeriesIndex) {
                return { ...serie, data: dailyRevenueArray };
              }
              return serie;
            }),
          };
        } else {
          // Add "Revenue" series as it does not exist yet
          return {
            ...prevState,
            series: [
              ...prevState.series,
              {
                name: "Revenue",
                data: dailyRevenueArray,
              },
            ],
          };
        }
      });
    }
  }, [dailyRevenueArray]);

  useEffect(() => {
    if (
      !!daysOfWeekArr &&
      daysOfWeekArr?.length > 0 &&
      !!dailyRevenueArray &&
      dailyRevenueArray?.length > 0
    ) {
      // eslint-disable-next-line react-hooks/set-state-in-effect
      setChartOptions({
        ...options,
        xaxis: {
          ...options.xaxis,
          categories: daysOfWeekArr,
        },
        yaxis: {
          ...options.yaxis,
          // get the min & max values to the neareast hundred
          max: Math.ceil(Math.max(...dailyRevenueArray) / 100) * 100,
          min: Math.floor(Math.min(...dailyRevenueArray) / 100) * 100,
        },
      });
    }
  }, [daysOfWeekArr, dailyRevenueArray]);

  return (
    <div className="border-border bg-card shadow-default pt-7.5 sm:px-7.5 col-span-12 rounded-sm border px-5 pb-5 xl:col-span-8">
      <div className="flex flex-wrap items-start justify-between gap-3 sm:flex-nowrap">
        <div className="flex w-full flex-wrap gap-3 sm:gap-5">
          <div className="min-w-47.5 flex">
            <span className="border-primary mr-2 mt-1 flex h-4 w-full max-w-4 items-center justify-center rounded-full border">
              <span className="bg-primary block h-2.5 w-full max-w-2.5 rounded-full"></span>
            </span>
            <div className="w-full">
              <p className="text-primary font-semibold">Total Profit</p>
              <p className="text-muted-foreground text-sm font-medium">
                Last 7 Days
              </p>
            </div>
          </div>
          <div className="min-w-47.5 flex">
            <span className="border-secondary mr-2 mt-1 flex h-4 w-full max-w-4 items-center justify-center rounded-full border">
              <span className="bg-secondary block h-2.5 w-full max-w-2.5 rounded-full"></span>
            </span>
            <div className="w-full">
              <p className="text-secondary font-semibold">Total Revenue</p>
              <p className="text-muted-foreground text-sm font-medium">
                Last 7 Days
              </p>
            </div>
          </div>
        </div>
        <div className="max-w-45 flex w-full justify-end">
          <div className="bg-muted inline-flex items-center rounded-md p-1.5">
            <button className="bg-background text-foreground shadow-card hover:bg-background hover:shadow-card rounded px-3 py-1 text-xs font-medium">
              Day
            </button>
            <button className="text-muted-foreground hover:bg-background hover:shadow-card rounded px-3 py-1 text-xs font-medium">
              Week
            </button>
            <button className="text-muted-foreground hover:bg-background hover:shadow-card rounded px-3 py-1 text-xs font-medium">
              Month
            </button>
          </div>
        </div>
      </div>

      <div>
        <div id="chartOne" className="-ml-5">
          <ReactApexChart
            options={chartOptions}
            series={state.series}
            type="area"
            height={350}
          />
        </div>
      </div>
    </div>
  );
}
