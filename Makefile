# literally just convenience

PKGNAME = curl

.PHONY: build

build:
	idris2 --build ${PKGNAME}.ipkg

install:
	idris2 --install ${PKGNAME}.ipkg

clean:
	@find . -type f -name '*.ttc' -exec rm -f {} \;
	@find . -type f -name '*.ttm' -exec rm -f {} \;
	@find . -type f -name '*.ibc' -exec rm -f {} \;
