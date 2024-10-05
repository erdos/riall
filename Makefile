.PHONY: test uber native build

test:
	clojure -A:test

uber:
	clojure -T:build uber

native:
	clojure -T:build uber

build: uber