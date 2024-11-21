---
format: md
---

## Workflow

The following workflow demonstrates a conservation area networking analysis using conservation areas built by `beaconsbuilder`. Comments in the code indicate points where users could instead use polygons of other conservation areas such as the existing protected areas network.
<center><img src="pics/workflow.png" width="400"></center>

## Overview

The Overview section provides a description of the app, its functionality, and the demo datasets. You can start using the app by turning on and off the various layers in the map legend.

<center><img src="pics/app.png" width="600"><br>Figure 1. Shiny-based disturbance explorer app.</center>

## Select study area

Click on "Select study area" to change the area of interest dataset. You have two choices:

  - Select an existing fundamental drainage area (FDA). This will include all the layers needed to run the app. [*Currently not functional*]
  - Upload a user-defined study area as a geopackage ('.gpkg'). In this case, the onus is on the user to ensure that all the required layers are included, and that the projection is sest to EPSG:3578 (NAD83 / Yukon Albers). See the **Datasets** tab for more details about required layers.

<center><img src="pics/slide1.png" width="600"><br>Figure 1. Shiny-based disturbance explorer app.</center>

## Buffer features

Click on the "Buffer sizes" tab to apply buffers of influence around linear and areal (polygonal) feature types. Simply edit the values in the "BUFFER_SIZE" column.

In the left sidebar, you can also set a minimum patch size of intactness areas after the buffering on the disturbance features are applied. This function will remove every patch in the study area that are considered not sufficiently large to maintain key ecological processes
  
<center><img src="pics/slide1.png" width="600"><br>Figure 1. Shiny-based disturbance explorer app.</center>

### View map

After a few seconds to a minute, two new layers will appear in the map and legend: a "footprint" layer that shows the human influence on the landscape and an "intactness" layer which shows the distribution of intact areas in the landscape.

<center><img src="pics/slide2.png" width="600"><br>Figure 1. Shiny-based disturbance explorer app.</center>

### View statistics

If you click on the "Statistic" tab, you will be able to compare regional estimates of intactness to those provided by Intact Forest Landscapes for the years 2000 and 2020. The areal extent of burned areas are also shown.

<center><img src="pics/slide4.png" width="600"><br>Figure 1. Shiny-based disturbance explorer app.</center>

### Download data

Finally, you can click on "Download data" in the left sidebar to save the footprint and intactness layers to a GeoPackage format which can be further viewed and analysed in a GIS e.g., QGIS.

<center><img src="pics/slide5.png" width="600"><br>Figure 1. Shiny-based disturbance explorer app.</center>
