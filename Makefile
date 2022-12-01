APP_NAME := tweeter
CACHE_APP_NAME := cache
TWEETER_SERVER_APP_NAME := tweeter
TWEETER_CLIENT_SIMULATOR_APP_NAME := client
COMPILER_OPTIONS := -pa ebin/ -o ebin/ -DPROD

all: compile_tweeter_server compile_tweeter_client_simulator compile_common

compile_common: clean
	erlc $(COMPILER_OPTIONS) src/common/*.erl

compile_tweeter_server: clean
	cp src/$(TWEETER_SERVER_APP_NAME).app ebin/
	erlc $(COMPILER_OPTIONS) src/$(TWEETER_SERVER_APP_NAME)/*.erl

compile_tweeter_client_simulator: clean
	cp src/$(TWEETER_CLIENT_SIMULATOR_APP_NAME).app ebin/
	erlc $(COMPILER_OPTIONS) src/${TWEETER_CLIENT_SIMULATOR_APP_NAME}/*.erl

run:
	erl -pa ebin/
	
# run:
# 	erl -pa ebin/ -sname $(APP_NAME) -s $(APP_NAME) -config config/production

# run_dev:
# 	erl -pa ebin/ deps/**/ebin/ -sname $(APP_NAME) -s $(APP_NAME) -config config/development

clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump