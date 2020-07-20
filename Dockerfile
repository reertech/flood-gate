FROM ubuntu:16.04 AS build

RUN apt-get update && \
    apt-get install -y libgmp10-dev libpq-dev zlib1g-dev curl libtinfo-dev

RUN curl -sSL https://get.haskellstack.org/ | sh

COPY app/ /app/

RUN cd /app && \
    stack build && \
    stack install

RUN apt-get install -y pax-utils && \
    echo "Required libraries:" && \
    scanelf --needed --nobanner --recursive /root/.local/bin | \
      awk '{ gsub(/,/, "\n", $2); print $2 }' | \
      grep -v '/root/.local/bin' | sort -u | \
      xargs -r dpkg -S | sed -e 's/: .*//g' -e 's/:amd64$//' | \
      grep -v libc6 | sort -u | tr '\n' ' ' | tee /root/libraries && \
    echo ""




FROM ubuntu:16.04 AS production

COPY --from=build /root/libraries /tmp/libraries
RUN apt-get update && \
    apt-get install -y ca-certificates sudo `cat /tmp/libraries` && \
    rm -f /tmp/libraries

RUN mkdir -p /app && \
    useradd -d /app app
COPY --from=build /root/.local/bin/flood-gate-exe /app/flood-gate
COPY docker/ /
RUN chown -fR app:app /app && \
    chmod a+rx /app/flood-gate

USER app

ENTRYPOINT ["/app/flood-gate"]
