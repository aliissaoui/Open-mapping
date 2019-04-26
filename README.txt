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

Examples:
	Itinerary connected vertexes:
		    http://localhost:9000/route?start=392014874&end=392015296

	Itinerary deconnected universe:
		    http://localhost:9000/route?start=645727111&end=645727111

Tests:
	make test to execute the tests

clean:

	make clean to clean the directories


Questions:

	- are we going to show the distance
	- idea: if no itinerary return (v v) , if (car == cadr) disconnect.
