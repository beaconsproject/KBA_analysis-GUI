## Create Networks

Individual KBAs and/or PAs may not be sufficiently representative of the reference area, and a network may be required to achieve representation objectives. In those cases, networks of more than one KBA and the PAs network can be created and assessed. 

The following network options are available:

1. Networks comprised of only KBAs with the specified number of KBAs per network (≥ 2). All combinations of KBAs are evaluated.
2. Networks comprised of KBAs with the specified number KBAs per network (≥ 1) plus the PA network forced into all networks. All combinations of KBAs are evaluated.

The KBAs used to create the networks can be restricted to filtered KBAs identified in the previous step **Upload reference area and assess representation**.

### Using the app

**Set number of KBAs per network** - Specify the number of KBAs in the network. 

**Apply KBA filtering in the network** - Check the box if the KBAs used to create the network must ...

**Force PAs in the network** - (Change to "Include PAs in the network")

Click on the orange button **Build network** to launch the representation analysis. Depending on the number of KBAs, the size of the PA network, and the resolution of the indicators, this step can take a while to finish. Once completed, a spatial layer of the networks will be added to the KBA_analysis geopackage in the folder called "output". The attributes added to this spatial layer are listed and described below. Density and bar plots are also created for each network. 

When the analysis is complete, the networks will appear in the map. The table in the upper right provides a count of the networks in the analysis. The attributes of each network can be explored by selecting the network from the dropdown menu. When selected, the network and its upstream area will be highlighted in the map. 

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

To explore the results, use the sliders to set maximum DM values for each indicator and the maximum upstream area for the network. Click on the orange **Apply filtering** button. The table on the upper right will update and the networks available for exploration, and displayed on the map, will be restricted to the filtered networks. A spatial layer of networks based on the last filter applied can be downloaded in the next step of the App: **Download results**.

**Interacting with the Map**

Click on the icon in the top right corner of the map to view the full list of spatial layers available to view on the map.

## Output

**Spatial Layers**

Spatial layers are created for the networks and the areas upstream of the networks. 

The following naming convention is used for the network spatial layers added to the KBA_analysis geopackage: **net_** + *number of KBAs e.g., KBA2_* + *force filter - True or False e.g., forceFALSE or forceTRUE* + *P **force*FALSE or TRUE*_*includePA***

(**net_** + *number of KBAs e.g., KBA2_* + *apply KBA filtering: True or False e.g., filterFALSE or filterTRUE* + *force PAs in the network e.g., _includePAs* )

Example 1, if number of KBAs per network = 2, KBA filtering is not applied (FALSE), and PAs are forced in the network, the name of the spatial layer is **net_KBA2_filterFALSE_includePAs**

Example 2, if number of KBAs per network = 2, KBA filtering is applied (TRUE), and PAs are not forced in the network, the name of the spatial layer is **net_KBA2_filterTRUE**

The network spatial layer has the following attributes:
- **Network**: unique identifier for the network 
- **AWI**: mean area-weighted catchment intactness of the network reported as a proportion, ranging from 0 (0% intact) to 1 (100% intact) 
- **area_km2**: total area of the network in km2  
- **up_km2**: total area upstream of the networkin km2  
- **up_AWI**: mean area-weighted catchment intactness of the area upstream of the network reported as a proportion, ranging from 0 (0% intact) to 1 (100% intact)  
- **dci**: Dendritic Connectivity Index (DCI) of the network (changes coming to this attribute)
- **cmi**: KS statistic measuring the network's representation of CMI
- **led**: KS statistic measuring the network's representation of LED
- **gpp**: KS statistic measuring the network's representation of GPP
- **lcc**: KS statistic measuring the network's representation of landcover 

The upstream spatial layer has the following attribute: 
- **Network**: unique identifier for the network 

**Density and Bar Plots**

Plots for the networks are saved to a folder with a name that includes the spatial layer name e.g., **plotnet_KBA2_filterFALSE_includePA**.
