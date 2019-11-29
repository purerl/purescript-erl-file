.PHONY: ps erl all test

all: ps erl

ps:
	psc-package sources | xargs purs compile 'src/**/*.purs' 'test/**/*.purs'

test: ps erl
	erl -pa ebin -noshell -eval '(test_main@ps:main())()' -eval 'init:stop()'

erl:
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl
