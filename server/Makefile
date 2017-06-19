compile:
	erlc -o ebin src/*.erl

run:
	erl -pa ebin -pa _build/default/lib/*/ebin -config server -boot start_sasl -s server start
