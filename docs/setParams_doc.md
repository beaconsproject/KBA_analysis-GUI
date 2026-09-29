# Set input parameters

Setting input parameters requires to: 

### 1. Select the output directory and project
1. Select an output directory:
   - Navigate to the directory where BenchmarkBuilder_cmd.exe is located.
   - Click Select (bottom right), then confirm your selection. 
2. Select the project type:
   - **Use an existing project**: pick a project (subfolder) from the list. Previously uploaded layers and results are reloaded automatically, so you can go to step 3.
   - **Create a new project**: enter a name. The app creates the project folder with the subfolders `data`, `output`, `Builder_input` and `Builder_output`.

&#x1F4CC; **Note:** Directory and project names must not contain spaces.

<br>

### 2. Upload spatial datasets
All spatial datasets for building KBAs are uploaded here, as well as the planning region boundary, protected areas, reference area for representation analysis, and environmental criteria for assessing representation (CMI, LCC, LED, and GPP). This includes shapefiles and TIF files. 

A shapefile consists of multiple files with the same name but different extensions. All files associated with the shapefile must be uploaded and must include .shp, .shx, .dbf, .prj. 

A tif is a single .tif file. 

All spatial datasets must have the same projectiong e.g., `NAD 1983 Albers`.

The following spatial datasets are required:   
- **Catchments**: A shapefile representing a set of watershed catchments created by BEACONs Project. 
- **Streams**: A shapefile of linear features representing the stream network. 
- **Planning region**: A single polygon outlining the boundary of the planning area where KBAs will be generated
- **LCC**: A TIF representing Land Cover map of Canada 2020 (categorical- 19 classes)  
- **LED**: A TIF representing Lake-Edge Density (continuous)  
- **CMI**: A TIF representing Climate Moisture Index (continuous)
- **GPP**: A TIF representing Gross Primary Productivity (continuous)

Optional spatial dataset: 
- **Protected areas**: A shapefile representing existing protected areas.
- **Reference area**: A polygon defining the area against which KBA representation will be assessed.
- **Custom criteria**: One additional spatial dataset for the representation analysis can be uploaded e.g., climate-projected CMI. The name of file will appear in the map legend, naming of output files, and as an attribute name in shapefile tables. As such, a short name is recommended e.g., projcmi. 


There are two options for uploading the spatial datasets: (1) use csv with file pathways and (2) upload individual layers. 

**OPTION 1: Use CSV with file pathways**

The spatial datasets can be uploaded using a csv file created in a text editor (e.g., Notepad). The csv file must have the following structure:

 Layer,Path  
 catchments,C:/data/catchments.shp  
 stream,C:/data/streams.shp  
 planning region,C:/data/planning_region.shp  
 protected areas,C:/data/protected_areas.shp **This dataset is optional. Delete this line if a protected areas dataset is not included.*  
 reference area,C:/data/reference_area.shp **This dataset is optional. Delete this line if a reference area dataset is not included.*  
 CMI,C:/data/cmi.tif  
 LCC,C:/data/lcc.tif  
 LED,C:/data/led.tif  
 GPP,C:/data/gpp.tif  
 PROJCMI,C:/data/projcmi.tif **Adding custom criteria is optional. Delete this line if a custom dataset is not included.* 

The column headings (Layer,Path) must not change. Layer names under the "Layer" column (e.g., catchments, streams, planning region, etc.) must not change except for the custom criteria (e.g., PROJCMI). See note above about naming the custom criteria under "Optional spatial dataset".

Template can be downloaded [here](./accessPath.csv)

**OPTION 2: Upload individual layers.**

Datasets are uploaded by navigating to the shapefile or tif, select the dataset, and click open. A shapefile consists of multiple files with the same name but different extensions. All files associated with the shapefile must be selected before clicking "Open".  

<br>

### 3. Specify the intactness attribute

Select the catchment attribute giving the proportion of each catchment that is intact (0 to 1, where 1 = 100% intact). **This is required before using any other section.**

<br>

### 4. Protected areas (optional)
Once the intactness attribute is set, upload the protected areas here if they were not in the CSV. The app then automatically calculates, for each PA:

- **area_km2**: area of the PA
- **AWI**: area-weighted intactness of the PA (0–1)
- **up_km2** and **up_AWI**: area and intactness of the land upstream of the PA, using the same method as BUILDER
- **dci**: Dendritic Connectivity Index, from 0 (fragmented) to 1 (fully connected stream network within the PA) (Cote et al. 2009)

The results are shown on the map and in the **Protected areas statistics** table below it. Click a PA on the map to highlight its row. A `NAME` attribute is used for display. If it is missing, the table shows empty names.

This step is required to include PAs in **Assess representation** and in KBA networks.


<br>

### Output
Uploaded spatial datasets are copied to the project's data folder. When a CSV file is used, layer_paths.csv is also saved in this folder. 
This allows the project to be reopened later with the same input datasets.
 