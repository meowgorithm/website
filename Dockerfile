FROM haskell:9.10.3 AS server-builder
RUN cabal update
RUN cabal install cabal-cache
WORKDIR /usr/src/app
COPY . .
RUN cabal install --only-dependencies --overwrite-policy=always --keep-going
ENV CABALOPTS="-static -optl-static -optl-pthread -fPIC"
RUN cabal build --enable-executable-static --builddir ./build
RUN cp `find ./build -type f -name "webserver"` /webserver


FROM node:22-alpine AS asset-builder
WORKDIR /usr/src/app
COPY package.json .
COPY package-lock.json .
RUN npm ci
COPY . .
RUN npm run build
RUN cp -r static /static


FROM alpine:3.21
COPY --from=server-builder /webserver /app/webserver
COPY --from=asset-builder /static /app/static
WORKDIR /app
CMD ["/app/webserver"]
