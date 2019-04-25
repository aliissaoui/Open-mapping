S=src
M=maps
T=test

.PHONY : server test

build : server 

server :
	racket ${S}/server.rkt ${M}/pentagon.osm

test :
	racket ${T}/all-tests.rkt

clean :
	rm -rf *~ *# ${S}/*~ ${S}/*# ${S}/compiled
