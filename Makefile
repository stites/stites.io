stack-build:
	stack build

clean: stack-build
	stack exec site -- clean

build: stack-build clean
	stack exec site -- build

watch: stack-build clean
	stack exec site -- watch --host 0.0.0.0

.PHONY: hakyll clean build watch stack-build
