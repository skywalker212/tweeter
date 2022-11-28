APP_NAME := tweeter
CACHE_APP_NAME := cache
TWEETER_SERVER_APP_NAME := tweeter

all: compile_tweeter_server compile_tweeter_client
	
compile_tweeter_server: clean
	cp src/$(TWEETER_SERVER_APP_NAME).app ebin/
	erlc -pa ebin/ -o ebin/ src/$(TWEETER_SERVER_APP_NAME)/*.erl

compile_tweeter_client: clean
	erlc -pa ebin/ -o ebin/ src/tweeter_client/*.erl

run:
	erl -pa ebin/
	
# run:
# 	erl -pa ebin/ -sname $(APP_NAME) -s $(APP_NAME) -config config/production

# run_dev:
# 	erl -pa ebin/ deps/**/ebin/ -sname $(APP_NAME) -s $(APP_NAME) -config config/development

clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump