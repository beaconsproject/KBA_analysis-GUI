

## Calculate hydrology metrics on KBAs

Here, area, intactness, and hydrology metrics are added to the KBA polygon attribute table: 

**Network** is the unique identifier for the KBA and is created by Builder.

**Area_PB** is the area of the KBA calculated by Builder in m2.

**AWI** is calculated by Builder. It is the mean area-weighted catchment intactness of the KBA reported as a proportion, ranging from 0 (0% intact) to 1 (100% intact).

**Area_km2** is the area of the KBA converted to km2.

**up_km2** is calculated by Builder. It is the total area upstream of the KBA in km2.

**up_AWI** is calculated by Builder. It is the mean area-weighted catchment intactness of the area upstream of the KBA reported as a proportion, ranging from 0 (0% intact) to 1 (100% intact).

**dci** is calculated by the App. The Dendritic Connectivity Index (DCI) quantifies the “longitudinal connectivity of river networks based on the expected probability of an organism being able to move freely between two random points of the network” (Cote  et  al.  2009). The index ranges  from  0 (low connectivity) to 1 (high connectivity).  

Cote, D., Kehler, D.G., Bourne, C. et al. A new measure of longitudinal connectivity for stream networks. Landscape Ecol 24, 101–113 (2009). https://doi.org/10.1007/s10980-008-9283-y


### Output

Two spatial layers are added to the KBA_analysis geopackage (KBA_analysis.gpkg) in the "output" subfolder. 

1. **KBAs_dci** - KBA polygons with the attributes listed above.  

2. **KBAs_upstream** - polygons of the upstream area for each KBA. The "Network" attribute is the unique identifier for the KBA.