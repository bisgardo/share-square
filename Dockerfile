# syntax=docker/dockerfile:1

FROM node:14-alpine as build
WORKDIR /build
COPY ./package*.json ./
RUN npm install
COPY . .
RUN npm run build

FROM nginx:stable-alpine
COPY ./index.html /usr/share/nginx/html/index.html
COPY --from=build /build/dist/app.js /usr/share/nginx/html/app.js
