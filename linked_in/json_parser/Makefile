.PHONY: compile c_compile shell clean_shell

compile: c_compile
	erlc -o ebin/ src/*.erl

c_compile:
	gcc -o priv/jp_driver.so -fpic -bundle -flat_namespace -undefined suppress \
	-I`which erl`/../../erts-7.2.1/include \
	-I`which erl`/../../lib/erl_interface-3.8.1/include \
	-Illoyd-yajl-9c15d72/build/yajl-1.0.9/include \
	-L`which erl`/../../lib/erl_interface-3.8.1/lib \
	-Llloyd-yajl-9c15d72/build/yajl-1.0.9/lib \
	c_src/jp_driver.c \
	-lei_st \
	-lyajl

clean_shel:
	erl -pa ../json_parser/ebin

shell: compile
	erl -pa ../json_parser/ebin/ -s json_parser
