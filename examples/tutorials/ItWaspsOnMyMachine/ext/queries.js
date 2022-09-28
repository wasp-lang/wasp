import axios from 'axios';

export const getExcuse = async () => {
  const response = await axios.get('https://api.devexcus.es/')
  return response.data
}

export const getAllExcuses = async (_args, context) => {
  return context.entities.Excuse.findMany()
}