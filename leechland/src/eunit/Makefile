all: eunit env extra_test clean
	echo "FULL REGRESSION IS DONE :)"

eunit:
	./rebar -C rebar.config.rel compile
	cp src/*.app ./ebin
	erl -noshell -sname leechland_testnode -pa ./ebin -eval 'leechland_regression:start(),halt().'

env:
	python ./src/leechland_setup &

extra_test:
	erl -noshell -sname leechland_testnode1 -pa ./ebin -eval 'integration_regression:start(),halt().'

clean:
	rm -f ebin/*.beam
	sh ./src/kill_testleechland
