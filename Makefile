S=src
M=maps
T=test

.PHONY : server test

server:
	racket ${S}/server.rkt ${M}/pentagon.osm

test :
	racket ${T}/all-tests.rkt

clean :
	rm -rf *~ *# ${S}/*~ ${S}/*# ${S}/compiled
