# syntax=docker/dockerfile:1

FROM node:14-alpine as build
WORKDIR /build
COPY ./package*.json ./
RUN npm install
COPY . .
RUN npm run build

FROM nginx:stable-alpine
COPY --from=build /build/dist /usr/share/nginx/html
