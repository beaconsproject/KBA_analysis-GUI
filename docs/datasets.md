
> Dataset

The following data are required:
- catchments
- stream network
- planning region boundary defines the region within which the KBAs are identified
- reference area that specifies the areaa to be represented by the KBA(s)
- CMI rasters
- LED rasters
- LCC landcover rasters
- GPP rasters 

### Catchments
The BEACONs catchment dataset is a set of approximate drainage areas for segments of the stream network. The dataset has attributes that describe the direction of water flow as well as catchment land and water area, intactness, watershed association, and stream length. 

KBA Explorer uses the following functions in `beaconstools` that rely on a catchment dataset:

- `dissolve_catchments_from_table()` and `extract_catchments_from_table()`: converts catchment lists (e.g. from `beaconsbuilder`) to extracted catchments or dissolved polygons.
- `get_upstream_catchments()`: calculates the area upstream of a given polygon using catchments and stream flow attributes.
- `criteria_to_catchments()`: sums area of raster values in an intersecting catchments dataset. Used in one of the representation analysis workflows.
- `evaluate_targets_using_catchments()`: evaluate representation targets in conservation areas using catchments. Used in one of the representation analysis workflows.

All other functions operate on polygons and do not require a catchments dataset.

The catchment dataset must have the following hardcoded attribute names: "FDA_M", "CATCHNUM", "ORDER1", "ORDER2", "ORDER3", "BASIN", "SKELUID", "length_m", "Area_land", "Area_water", "Area_total", and "Isolated".

The catchment dataset much also have a "Zone" attribute that the user will be asked to point to. There are no restrictions on the name of this attribute. Zones specifies subregions within the planning region that KBAs must stay with. In other workds, the App will not build KBAs that cross zones.  A zone may be a watershed such as an Ocean Drainage Area, for example. 

### Stream Network

The stream dataset must have the following attributes: "BASIN" and "SKELUID".
