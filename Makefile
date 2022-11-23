APP_NAME := tweeter
CACHE_APP_NAME := cache

all: compile_cache
	
compile_cache: clean
	cp src/$(CACHE_APP_NAME).app ebin/
	erlc -pa ebin/ -o ebin/ src/$(CACHE_APP_NAME)/*.erl

run:
	erl -pa ebin/
	
# run:
# 	erl -pa ebin/ -sname $(APP_NAME) -s $(APP_NAME) -config config/production

# run_dev:
# 	erl -pa ebin/ deps/**/ebin/ -sname $(APP_NAME) -s $(APP_NAME) -config config/development

clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump