FROM erlang
MAINTAINER Peter Morgan <peter.james.morgan@gmail.com>

RUN apt-get update && apt-get install -y \
    make

RUN make

EXPOSE 3535

ENTRYPOINT ["_rel/haystack/bin/haystack"]
CMD ["foreground"]
