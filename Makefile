push:
	source secrets && s3_website push --config-dir=_config/

build:
	jekyll build --config=_config/_config.yml

publish: build push

