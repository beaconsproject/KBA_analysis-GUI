## Create Networks

Individual KBAs and/or PAs may not be sufficiently representative of the reference area. In those cases, networks of more than one KBA/PA can be created and assessed. The following network options are available:

1. Networks comprised of only KBAs to the specified number of KBAs per network. All combinations will be evaluated.
3. Networks comprised of only PAs to the specified number of PAs per network. All combinations will be evaluated.
4. Networks comprised of all combinations of KBAs and PAs.

### Using the app

First, specify the number of KBAs in the network. 

**Upload reference area shapefile**

To upload the shapefile, Browse to the location of the file and select all files associated with the shapefile (.shp, .shx, .dbf, .prj, etc.) and click "Open".

Second, specify if KBAs and/or PAs are to be assessed. For PAs to be included, the PAs must first be evaluated under the **Evaluate PAs (optional)** step. 

**Assess representation using:**
- **Only KBAs** - select this option if only KBAs are to be assessed.
- **Only PAs** - select this option if only PAs are to be assessed. **See note above regarding PAs.**  
- **Both KBAs and PAs** - select this option if both KBAs and PAs are to be assessed. **See note above regarding PAs.** 

Click on the orange **Run representation analysis** button to launch the representation analysis. Depending on the number of KBAs/PAs and the resolution of the indicators, this step can take a while to finish. Once completed, a spatial layer called "KBA_att" and/or "PA_att" will be added to the KBA_analysis geopackage in the folder called "output". The attributes added to this spatial layer are listed and described below.

Once the analysis is complete, the KBAs/PAs will appear in the map. The table in the upper right provides a count of the KBAs and protected areas (PAs) in the analysis. The attributes of each KBA/PA can be explored by selecting the KBA/PA from the dropdown menu. When selected, the KBA/PA and its upstream area will be highlighted in the map. 

**Table Attributes:**
- Area km2: area of KBA/PA in km2   
- AWI: mean catchment area-weighted intactness of the KBA/PA (%)
- Upstream Area km2: area upstream of the KBA/PA in km2 
- Upstream AWI: mean catchment area-weighted intactness of the upstream area (%)
- DCI: Dendritic Connectivity Index
- CMI: KS statistic for Climate Moisture Index
- GPP: KS statistic for Gross Primary Productivity
- LED: KS statistic for lake-edge density
- LCC: BC statistic for landcover

**Filter KBAs and/or PAs based on dissimilarity metrics (DMs) and upstream area**

To explore the results, use the sliders to set maximum DM values for each indicator and the maximum upstream area for the KBA/PA. Click on the orange **Apply filtering** button. The table on the upper right will update and the KBAs/PAs available for exploration, and displayed on the map, will be restricted to the filtered KBAs/PAs. No new spatial layers are created.

**Interacting with the Map**

Click on the icon in the top right corner of the map to view the full list of spatial layers available to turn on and off on the map.

## Output

If KBAs are include in the representation analysis, a spatial layer called "KBAs_att" is added to the "KBA_analysis" geopackage (KBA_analysis.gpkg) in the "output" subfolder. 

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
