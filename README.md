Open Trails Anchorage
=====================

Converts existing Anchorage trail data to geojson format.

The files for this project are publicly available at the Municipality of Anchorage
and Alaska Department of Natural Resources websites at:

- http://www.muni.org/Departments/it/GIS2/Pages/MOAGISData.aspx (Shapefile)
- http://dnr.alaska.gov/parks/aktrails/explore/astgglearthmap.htm (KMZ)
- additionally the Matanuska/Susitna trails is in the /data subfolder (shapefile) 

We are adopting the geojson open format for this project in order to complete the
Open Trails System specification
(http://www.codeforamerica.org/specifications/trails/spec.html).

This specification requires five files:
- `trailsegements.json`
- `named_trails.csv`
- `trailheads.json`
- `stewards.csv`
- `areas.geojson` (optional and not included at this time)

As the project develops, you can see these files in Anchorage_Trails.zip. 

Cleaning the Data
-----------------------------

Go to the issues to look at what needs to be cleaned up. Please Contribulate!


Getting Started (development)
-----------------------------

From the project directory, run:

    docker build -t codeforanchorage/ota:latest .

This will set up a Docker-based development environment image. To run an
interactive session in the environment:

    docker -v /path/to/project:/project -i -t codeforanchorage/ota /bin/bash

