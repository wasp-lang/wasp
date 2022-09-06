import axios from 'axios';

export const getExcuse = async () => {
  return axios
    .get('https://api.devexcus.es/')
    .then(res => {
      return res.data;
    })
    .catch(error => {
      console.error(error);
    });
}

export const getAllSavedExcuses = async (_args, context) => {
  return context.entities.Excuse.findMany()
}