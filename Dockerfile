FROM haskell:8
MAINTAINER Boris Buliga <boris@d12frosted.io>
WORKDIR /opt/server
COPY ./stack.yaml /opt/server/stack.yaml
COPY ./package.yaml /opt/server/package.yaml
RUN stack setup
RUN stack build --only-dependencies
COPY ./src /opt/server/src
COPY ./app /opt/server/app
COPY ./test /opt/server/test
COPY ./README.org /opt/server/README.org
COPY ./ChangeLog.md /opt/server/ChangeLog.md
RUN stack build

ENV address=34.67.67.45
ENV port=8080
ENV user=
ENV code=

CMD stack exec bomberman-bot -- --address $address --port $port --user $user --code $code
