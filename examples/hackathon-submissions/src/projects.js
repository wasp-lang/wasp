export const submitProject = async (project, context) => {
  const newProject = context.entities.Submission.create({
    data: {
      ...project,
      approved: process.env.HEADLESS_TESTING ? true : false,
    },
  });

  return newProject;
};

export const getProjects = async (_args, context) => {
  return context.entities.Submission.findMany({
    where: {
      approved: true,
    },
    orderBy: {
      createdAt: "desc",
    },
  });
};
