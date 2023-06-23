FROM alpine:3.18

# we need to be root to tell the system to trust our CA
USER root

# copy the CA we want to trust into a location where alpine expects it to be
COPY keys/rootCA.pem /usr/local/share/ca-certificates/

# tell alpine to trust the CA file we added
RUN apk --no-cache add ca-certificates curl && \
  rm -rf /var/cache/apk/* && \
  update-ca-certificates
