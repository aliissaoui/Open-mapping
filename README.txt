How to use?
    Default:
	in root folder apply: make server
	   activates automatically the server with pentagon.osm
    Specific map:
    	racket src/server maps/map.osm 

Links:

Simple map:
	  http://localhost:9000

Itinerary:
	  http://localhost:9000/route?start=<id>&end=<id>

Distance:
	  http://localhost:9000/distance?start=<id>&end=<id>

cycle:
	  http://localhost:9000/cycle?nodes=<id,...,id>

Examples:
	Itinerary connected vertexes: ( works also with distance )
		    http://localhost:9000/route?start=392014874&end=392015296

	Itinerary deconnected universe:  ( works also with distance )
		    http://localhost:9000/route?start=645727111&end=645727111

	cycle:
		    http://localhost:9000/cycle?nodes=5297614183,645731557,3520831242,569691668

Tests:
	make test to execute the tests

clean:

	make clean to clean the directories
