push:
	source secrets && s3_website push --config-dir=./configs/

hakyll:
	stack build && stack exec site build

hakyll-watch:
	stack build && stack exec site clean && stack exec site watch

publish: hakyll push

