export const RAILWAY_SERVICE_NAME = 'Railway';
export const RAILWAY_INSTALL_CLI_URL = 'https://docs.railway.app/develop/cli';
export const RAILWAY_CONFIG_FILE_NAME = 'wasp.deployrc.railway.json';

// Docker file that will be used to copy to the client side of Wasp
export const REACT_DOCKER_TEMPLATE = `FROM node:18-alpine AS builder

ARG PORT
ARG WASP_WEB_CLIENT_URL
ARG REACT_APP_API_URL
ARG API_URL

ENV REACT_APP_PORT=$PORT
ENV REACT_APP_WASP_WEB_CLIENT_URL=$WASP_WEB_CLIENT_URL
ENV REACT_APP_API_URL=$REACT_APP_API_URL

# Add a work directory
WORKDIR /app

COPY package.json .

RUN npm install

COPY . /app/

RUN npm run build

FROM nginx:1.19.10-alpine

# Set working directory to nginx asset directory
WORKDIR /usr/share/nginx/html

# Remove default nginx static assets
RUN rm -rf ./*

COPY --from=builder /app/build .

COPY .nginx/nginx.conf /etc/nginx/conf.d/default.conf

EXPOSE 3000

ENTRYPOINT ["nginx", "-g", "daemon off;"]
`;

export const NGINX_CONFIG_TEMPLATE = `server {
   listen       8080;
   server_name  localhost;

   location / {
       root   /usr/share/nginx/html;
       index  index.html;
       try_files $uri $uri/ /index.html;
   }
}
`;
