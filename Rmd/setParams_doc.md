# Set input parameters

There actions are completed here: 
(1) select an output directory and identify project subfolder, 
(2) upload spatial datasets in the App, and 
(3) specify catchment attribute that describes the propotion of area intact or undisturbed within the catchment.

### Select output Directory

Navigate to the directory where outputs produced by the App will be written, highlight the directory, and click Select (bottom right). 

Next, click the orange "Confirm" button.

### Identify Project

The user has two options: 

Option 1 - Select **Use an existing project** to revisit an existing project. The project is a subfolder within the Output directory. Use the dropdown menu to select the project and click the orange "Confirm" button. 

**If this option is selected, the App will automatically recognized data previously uploaded into the App, and the steps below are not required.**

Option 2 - Select **Create a new project** to create a new project. Enter the name for the project, and click the orange "Confirm" button. A subfolder with this name will be created in the Output directory as well as three project subfolders: Builder_input, Builder_output, and output. 

### Source spatial datasets

All spatial datasets for building KBAs are uploaded here, as well as the planning region boundary, protected areas, reference area for representation analyisis, and environmental criteria for assessing representation (CMI, LCC, LED, and GPP). This includes shapefiles and TIF files. 

A shapefile consists of multiple files with the same name but different extensions. All files associated with the shapefile must be uploaded and must include .shp, .shx, .dbf, .prj. 

A tif is a single file. 

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
- **Custom criteria**: One additional spatial dataset for the representation analysis can be uploaded e.g., climate-projected CMI. The name of file will appear in the map legend, naming of output files, and as an attribute name in shapefile tables. As such, a short name is recommended e.g., projcmi. 

There are two options for uploading the spatial datasets: (1) use csv file with pathways and (2) upload individual layers. 

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

### Using the app: Scenario 2 - Add to Previous Analysis



Spatial datasets will still need to be uploaded. See **Upload spatial datasets** above. If tif files of spatial datasets for the representation analysis (e.g., kba_cmi.tif) exist in the "output" subfolder, these datasets do not need to be uploaded again.

### Output

No output is created at this stage.  
