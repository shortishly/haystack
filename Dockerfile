FROM erlang
MAINTAINER Peter Morgan <peter.james.morgan@gmail.com>

ADD . /haystack
WORKDIR /haystack
RUN make

EXPOSE 3535

ENTRYPOINT ["_rel/haystack/bin/haystack"]
CMD ["foreground"]
