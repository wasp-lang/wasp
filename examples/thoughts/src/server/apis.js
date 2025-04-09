
export const getCurrentTime = async (req, res, context) => {
  res.json({
    time: (new Date()).toUTCString(),
  })
};

export const apiMiddleware = (config) => {
  return config;
};
