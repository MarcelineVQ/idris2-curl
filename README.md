Idris2 Curl Bindings
=====

I really don't want to write my own HTTP lib, so here's curl.  
This will provide the Easy interface of direct all-at-once actions and the Multi interface which exposes the full power of curl.

Further this will provide methods to use curl as a replacement for Idris' `network` package when used for internet connections.

Installation
------------
You can install via idris2 directly:  
`idris2 --install package.ipkg`  
Or via the Makefile:  
`make install`  
Or via the [sae tool](https://github.com/DoctorRyner/sae):
`sae-linux install`

Version
-------

This package follows [Haskell PVP](https://pvp.haskell.org/) which is distinct from [SEMVER](https://semver.org/) in that when examining `1.2.3`, `1.2`  is the Major Version rather than `1`.
