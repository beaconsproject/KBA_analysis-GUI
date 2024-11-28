

## Upload spatial dataset

This section aims to upload the desire shapefiles and TIFF files in the environment in order to  run the analysis. A shapefile consists of multiple files with the same name but different extensions. 
The mandatory files include .shp, .shx, .dbf, .prj. 
  
### Using the app

Required shapefiles and tiff files are: 

- **Catchment** : A shapefile representing a set of watershed catchments created by BEACONs Project. 
- **Stream** : A shapefile of linear features representing the stream netwrok. 
- **Plannning region** : A single polygon outlining the boundary of the planning area where KBAs will be generated
- **CMI** : A TIFF representing Climate moisture index (continuous)
- **GPP** : A TIFF Gross primary productivity (continuous)
- **LED** : A TIFF Lake-edge density (continuous)
- **LCC** : A TIFF Land cover map of Canada 2020 (categorical- 19 classes)

### Projections
All layers need to be projected to `NAD 1983 Albers` to match the catchments projection prior to run the analysis.

In the **catchments** layer, catchment intactness is provided using decimal (0-1). 


### Output

No output table are created at this stage.  
