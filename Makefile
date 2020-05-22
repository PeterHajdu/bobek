format:
	ormolu -m inplace `find src lib test -iname "*.hs"`

lint:
	hlint -g

build:
	cabal new-build

test: build
	cabal new-configure --enable-test --enable-coverage
	cabal new-test

start-env:
	docker build test/rabbitmq-container/ -t bobek-rabbitmq
	docker run --rm -d -p 5672:5672 -p 15672:15672 bobek-rabbitmq

smoke-test:
	echo "aroutingkey something" | cabal new-run bobek -- --stdin -d amqp://guest:guest@localhost:5672 -e ""
	cabal new-run bobek -- -s amqp://guest:guest@localhost:5672 -q aroutingkey --stdout | grep something

.PHONY: format lint test smoke-test start-env build
