export const submitProject = async (project, context) => {

  const newProject = context.entities.Submission.create({
    data: project,
  });

  return newProject;
};
