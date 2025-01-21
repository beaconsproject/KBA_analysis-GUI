## Create Networks

Individual KBAs and/or PAs may not be sufficiently representative of the reference area and a network may be required to achieve representation objectives. In those cases, networks of more than one KBA and PAs can be created and assessed. 

The following network options are available:

1. Networks comprised of only KBAs to the specified number of KBAs per network (≥ 2). All combinations are evaluated.
2. Networks comprised of KBAs with the specified number KBAs per network (≥ 1) with the PA network forced into all networks. All combinations of KBAs are evaluated.

The KBAs used to create the networks can be restricted to filtered KBAs identified in the previous step **Upload reference area and assess representation** based on dissimilarity metrics. 

### Using the app

First, specify the number of KBAs in the network. 

**Set number of KBAs per network**

Specify the number of KBAs per network by entering a number 

**Apply KBA filtering in the network**

Check the box if the KBAs used to create the network must ...

**Force PAs in the network**

Check the box if **

Click on the orange button **Build network** to launch the representation analysis. Depending on the number of KBAs/PAs and the resolution of the indicators, this step can take a while to finish. Once completed, a spatial layer of the networks will be added to the KBA_analysis geopackage in the folder called "output". The attributes added to this spatial layer are listed and described below. Density and bar plots are also created for each network. 

Once the analysis is complete, the networks will appear in the map. The table in the upper right provides a count of the networks in the analysis. The attributes of each network can be explored by selecting the network from the dropdown menu. When selected, the network and its upstream area will be highlighted in the map. 

**Table Attributes:**
- Area km2: total area of the network in km2   
- AWI: mean catchment area-weighted intactness of the network (%)
- Upstream Area km2: area upstream of the network in km2 
- Upstream AWI: mean catchment area-weighted intactness of the upstream area (%)
- DCI: Dendritic Connectivity Index
- CMI: KS statistic for Climate Moisture Index
- GPP: KS statistic for Gross Primary Productivity
- LED: KS statistic for lake-edge density
- LCC: BC statistic for landcover

**Filter networks based on dissimilarity metrics (DMs) and upstream area**

To explore the results, use the sliders to set maximum DM values for each indicator and the maximum upstream area for the network. Click on the orange **Apply filtering** button. The table on the upper right will update and the networks available for exploration, and displayed on the map, will be restricted to the filtered networks. No new spatial layers are created.

**Interacting with the Map**

Click on the icon in the top right corner of the map to view the full list of spatial layers available to view on the map.

## Output

The following naming convention is used for the network spatial layers added to the KBA_analysis geopackage: **net_kba"n"_force"FALSE or TRUE"_includePAs**

to If KBAs are include in the representation analysis, a spatial layer called "KBAs_att" is added to the "KBA_analysis" geopackage (KBA_analysis.gpkg) in the "output" subfolder. 

**KBAs_att** - KBA polygons with the following attributes:

- **Network** is the unique identifier for the KBA.  
- **AWI** is the mean area-weighted catchment intactness of the KBA reported as a proportion, ranging from 0 (0% intact) to 1 (100% intact).  
- **area_km2** is the area of the KBA in km2.  
- **up_km2** is the total area upstream of the KBA in km2.  
- **up_AWI** is the mean area-weighted catchment intactness of the area upstream of the KBA reported as a proportion, ranging from 0 (0% intact) to 1 (100% intact).  
- **dci** is the Dendritic Connectivity Index (DCI) of the KBA.
- **cmi** is the KS statistic measuring the KBA's representation of CMI.
- **led** is the KS statistic measuring the KBA's representation of LED.
- **gpp** is the KS statistic measuring the KBA's representation of GPP.
- **lcc** is the KS statistic measuring the KBA's representation of landcover. 

If PAs are include in the representation analysis, a spatial layer called "PAs_att" is added to the "KBA_analysis" geopackage (KBA_analysis.gpkg) in the "output" subfolder. 

**PAs_att** - PA polygons with the following attributes:

- **Network** is the unique identifier for the PA.  
- **AWI** is the mean area-weighted catchment intactness of the KBA reported as a proportion, ranging from 0 (0% intact) to 1 (100% intact).  
- **area_km2** is the area of the KBA in km2.  
- **up_km2** is the total area upstream of the KBA in km2.  
- **up_AWI** is the mean area-weighted catchment intactness of the area upstream of the KBA reported as a proportion, ranging from 0 (0% intact) to 1 (100% intact).  
- **dci** is the Dendritic Connectivity Index (DCI) of the KBA.
- **cmi** is the KS statistic measuring the KBA's representation of CMI.
- **led** is the KS statistic measuring the KBA's representation of LED.
- **gpp** is the KS statistic measuring the KBA's representation of GPP.
- **lcc** is the KS statistic measuring the KBA's representation of landcover. 
