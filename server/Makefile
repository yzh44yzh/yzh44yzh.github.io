compile:
	erlc -o ebin src/*.erl

run:
	erl +pc unicode -pa ebin -pa deps/*/ebin -config server -boot start_sasl -s server start
