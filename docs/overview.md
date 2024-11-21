
## KBA Analysis for the Northern Canadian Shield Taiga Ecoregion
> Using beaconstools to Design KBAs

Key biodiversity area (KBA) planning often involves the selection of optimal conservation area scenarios from a suite of options. In the case of KBAs produced by the partner package `beaconsbuilder`, there could be 100's to 1000's of conservation area options for a given planning region. 
When combined into networks of multiple KBAs, the number of options can often be in the millions.

The `beaconstools` package provides a range of functions for building polygons of KBA and KBA networks, and adding ecological attributes to those polygons to allow 
options to be ranked. Functions in the package perform the following tasks:

- **Create KBA polygons** - creates conservation area polygons using output from the `beaconsbuilder` package; combines `beaconsbuilder` conservation areas with polygons defining other conservation areas such as the existing protected area network; combines conservation areas into networks of multiple conservation areas; filters conservation areas and networks to remove spatial overlap and redundancy.
- **Hydrology and upstream threats** - For a given conservation area or polygon, identifies all watershed catchments upstream (or downstream) and calculates their area and intactness values.
- **Dendritic connectivity** - Calculates hydrological connectivity within each conservation area or network.
- **Assess representation using dissimilarity metrics** - An alternate representation metric using dissimilarity statistics to compare raster distributions between a conservation area and a planning region.
- **Build network** - Generates KBA networks  .

### Network naming conventions

Individual KBA network are defined as a single feature with an associated geometry, stored in a simple features object that typically contains multiple KBA
and their associated geometries and attributes. Each network should have a unique name which is typically stored in a column named `network`. For consistency, 
even features representing single conservation areas have their names stored in the `network` column, you can think of these as networks made up of just one conservation area. 
Simple and standardized conservation area names are encouraged and must not include spaces. 

For networks of multiple conservation areas, the individual KBA names are combined using the separator `__`. 
So a network named `KBA_0001__KBA_0002` would be the combined geometries of the individual conservation areas `KBA_0001` and `KBA_0002`. 


### Functionality

The app demonstrates a KBA networking analysis using KBAs built by `beaconsbuilder`. Comments in the code indicate points where users could instead use polygons of other conservation areas such as the existing protected areas network.
    
### Output

Multiples files are saved locally by the app. A destination folder is set at the **Set input parameters"** stage. This destination folder will hold 
Builder_input, Builder_output and others related output. Spatial layers are saved in a geopackage named KBA_analysis.gpkg in the output folder. If you are pointing to a directory that was use previously to run the analysis, the app won't overwrite the existing files. On the contrary, the app will use the data already generated, thus acting as a cache. 
