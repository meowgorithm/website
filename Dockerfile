FROM haskell:9.4.8 AS server-builder
RUN cabal update

RUN cabal install cabal-cache

WORKDIR /usr/src/app
COPY . .
RUN cabal install --only-dependencies --overwrite-policy=always --keep-going
RUN cabal build --builddir ./build
RUN cp `find ./build -type f -name "webserver"` /webserver

# FROM node:23-alpine AS asset-builder
# WORKDIR /usr/src/app
# COPY package.json .
# COPY package-lock.json .
# RUN npm install
# COPY . .
# RUN npm run build
# COPY ./static /static

# FROM alpine:latest
# COPY --from=server-builder /webserver /app/webserver
# COPY --from=asset-builder /static /app/static
# CMD ["/app/webserver"]
