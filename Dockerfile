FROM erlang
MAINTAINER Peter Morgan <peter.james.morgan@gmail.com>

ENV TZ=GMT
ENV CODE_LOADING_MODE=interactive

ADD . /haystack
WORKDIR /haystack
RUN make

EXPOSE 3535

ENTRYPOINT ["_rel/haystack/bin/haystack"]
CMD ["foreground"]
