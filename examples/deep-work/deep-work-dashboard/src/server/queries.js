import HttpError from '@wasp/core/HttpError.js';

/** HELPER FUNCTIONS */

async function getAllWork(context) {
  if (!context.user) {
    throw new HttpError(401);
  }

  return context.entities.Work.findMany({
    where: {
      user: {
        userId: context.user.userId,
      },
    },
    orderBy: {
      id: 'asc',
    },
  });
}

function addMissingDaysAndSort(work) {
  const daysOfWeek = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'];

  const addMissingDays = (work) => {
    const modifiedWork = [];

    for (let i = 0; i < work.length; i++) {
      const currentWork = work[i];
      const previousWork = i > 0 ? work[i - 1] : null;

      if (previousWork) {
        const currentTimeStarted = new Date(currentWork.timeStarted);
        const previousTimeStarted = new Date(previousWork.timeStarted);

        if (currentTimeStarted > previousTimeStarted) {
          const oneDay = 24 * 60 * 60 * 1000; // hours*minutes*seconds*milliseconds
          const numDays = Math.round(Math.abs((currentTimeStarted - previousTimeStarted) / oneDay));
          for (let j = 1; j < numDays; j++) {
            const missingTimeStarted = new Date(previousTimeStarted.getTime() + oneDay * j);
            const missingWork = {
              timeStarted: missingTimeStarted.toISOString(),
              minutes: 0,
            };
            modifiedWork.push(missingWork);
          }
        }
      }

      modifiedWork.push(currentWork);
    }

    return modifiedWork;
  };

  const modifiedWork = addMissingDays(work);

  const reducedDays = modifiedWork.reduce((acc, x) => {
    const dateObj = new Date(x.timeStarted).toISOString();
    const timestamp = Date.parse(x.timeStarted);
    const date = new Date(timestamp);
    const dayOfWeek = daysOfWeek[date.getDay()];

    let dayObject = acc.find((item) => {
      return item.timeStarted === dateObj.split('T')[0];
    });

    if (!dayObject) {
      dayObject = { dayOfWeek: dayOfWeek, timeStarted: dateObj.split('T')[0], minutes: 0 };
      acc.push(dayObject);
    }

    dayObject.minutes += Number(x.minutes);
    return acc;
  }, []);

  return reducedDays;
}

/** QUERIES */

/** 
 model Work {
  id Int @id @default(autoincrement())
  username String
  timeStarted DateTime
  minutes String
  user User @relation(fields: [userId], references: [userId])
  userId String
}
 */

export const getTotalWork = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401);
  }

  const work = await getAllWork(context);

  return addMissingDaysAndSort(work);
};

export const getTotalHours = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401);
  }

  const work = await getAllWork(context);

  let totalMinutes = 0;
  work.forEach((w) => {
    totalMinutes += parseInt(w.minutes);
  });

  return totalMinutes;
};

export const getWorkByYear = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401);
  }

  const work = await context.entities.Work.findMany({
    where: {
      user: {
        userId: context.user.userId,
      },
      timeStarted: {
        gte: new Date(args.year, 0, 1),
        lte: new Date(args.year, 12, 31),
      },
    },
    orderBy: {
      id: 'asc',
    },
  });

  return addMissingDaysAndSort(work);
};
