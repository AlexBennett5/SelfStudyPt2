erlc src/server_handler.erl
erlc src/server_dict.erl
erl -noshell -s server_dict start
rm server_dict.beam
rm server_handler.beam
rm erl_crash.dump
