FROM erlang:latest

RUN mkdir /rpn
COPY . /rpn

WORKDIR /rpn
RUN rebar3 as prod release

FROM erlang:latest
RUN mkdir /rpn
WORKDIR /rpn
COPY --from=0 /rpn/_build/prod/rel/rpn .
#WORKDIR /rpn

ENTRYPOINT ["bin/rpn"]
CMD ["foreground"]
