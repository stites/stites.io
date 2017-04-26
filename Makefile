push:
	source secrets && s3_website push --config-dir=_config/

jekyll:
	jekyll build --config=_config/jekyll.yml

publish: jekyll push

