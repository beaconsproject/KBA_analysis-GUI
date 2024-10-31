
> Dataset

The following data are required:
- catchments
- streams network
- planning region boundary or catchments that make up the planning region. This region serves as the reference area for assessing representation
- CMI rasters
- LED rasters
- LCC landcover rasters
- GPP rasters 

### Catchments
The BEACONs catchments dataset includes water catchment polygons with associated stream flow attributes as well as additional attributes describing catchment area and intactness. More info on the catchments can be found in `vignette("catchments")`.

The following functions in `beaconstools` rely on a catchments dataset:

- `dissolve_catchments_from_table()` and `extract_catchments_from_table()`: converts catchment lists (e.g. from `beaconsbuilder`) to extracted catchments or dissolved polygons.
- `get_upstream_catchments()`: calculates the area upstream of a given polygon using catchments and stream flow attributes.
- `criteria_to_catchments()`: sums area of raster values in an intersecting catchments dataset. Used in one of the representation analysis workflows.
- `evaluate_targets_using_catchments()`: evaluate representation targets in conservation areas using catchments. Used in one of the representation analysis workflows.

All other functions operate on polygons and do not require a catchments dataset.
