FROM erlang
MAINTAINER Peter Morgan <peter.james.morgan@gmail.com>
RUN make
ENTRYPOINT ["_rel/haystack/bin/haystack"]
CMD ["console"]
