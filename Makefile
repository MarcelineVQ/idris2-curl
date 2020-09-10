# literally just convenience

.PHONY: build

build:
	idris2 --build curl.ipkg

clean:
	@find . -type f -name '*.ttc' -exec rm -f {} \;
	@find . -type f -name '*.ttm' -exec rm -f {} \;
	@find . -type f -name '*.ibc' -exec rm -f {} \;
