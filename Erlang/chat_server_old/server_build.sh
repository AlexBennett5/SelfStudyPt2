erlc src/server.erl
erl -noshell -s server start
rm server.beam
rm erl_crash.dump
