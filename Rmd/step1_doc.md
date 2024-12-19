## Set input parameters

Here, the user must complete two actions: (1) select an output directory and (2) upload spatial datasets.

### Using the app: Scenario 1 - New Analysis

**Select output Directory**

Navigate to and/or create the directory where outputs produced by the App will be written, highlight the directory, and click Select (bottom right). 

Next, click the orange "Confirm" button.

Three subfolders will be created in the output Directory: Builder_input, Builder_output, and output. 

If the selected directory contains output from a prior analysis, a message will display.

**Upload spatial datasets**

All spatial datasets for buildings KBAs are uploaded here, as well as the planning region boundary, and environmental criteria for assessing representation (CMI, LCC, LED, and GPP). This includes shapefiles and TIF files. 

A shapefile consists of multiple files with the same name but different extensions. All files associated with the shapefile must be uploaded and must include .shp, .shx, .dbf, .prj. 

A tif is a single file. 

All spatial datasets must be projected to `NAD 1983 Albers` to match the catchment shapefile projection.

There are two options for uploading the spatial datasets: (1) upload csv file with pathways to the datasets and (2) upload each spatial dataset individually. 

For both upload options, the following spatial datasets are required: 
NOTE: See Overview-Dataset tab for dataset details, including required attributes for catchments and streams.

- **Catchments**: A shapefile representing a set of watershed catchments created by BEACONs Project. 
- **Streams**: A shapefile of linear features representing the stream network. 
- **Planning region**: A single polygon outlining the boundary of the planning area where KBAs will be generated
- **CMI**: A TIF representing Climate Moisture Index (continuous)
- **GPP**: A TIF representing Gross Primary Productivity (continuous)
- **LED**: A TIF representing Lake-Edge Density (continuous)
- **LCC**: A TIF representing Land Cover map of Canada 2020 (categorical- 19 classes)

Optional spatial dataset: 

- **Custom criteria**: One additional spatial dataset for the representation analysis can be uploaded e.g., climate-projected CMI. The name of file will appear in the map legend, naming of output files, and as an attribute name in shapefile tables. As such, a short name is recommended e.g., projcmi. 

**OPTION 1: Upload spatial datasets using a CSV with file pathways**

The spatial datasets can be uploaded using a csv file created in a text editor (e.g., Notepad). The csv file must have the following structure:

 Layer,Path  
 catchments,C:/KBA/data/catchments.shp  
 stream,C:/data/streams.shp  
 planning region,C:/data/planning_region.shp  
 CMI,C:/data/cmi.tif  
 LCC,C:/data/lcc.tif  
 LED,C:/data/led.tif  
 GPP,C:/data/gpp.tif

**OPTION 2: Upload each spatial dataset individually.**

Datasets are uploaded by navigating to the shapefile or tif, select the dataset, and click open. For shapefiles, select all files associated with the shapefile before clicking open.

Once the datasets are uploaded, no further action is required in this step. 

### Using the app: Scenario 2 - Add to Previous Analysis

A previous analysis can be added to by pointing to the Output directory that contains the analysis. The App will recognize the presence of the output files. 

Spatial datasets will still need to be uploaded. See **Upload spatial datasets** above. If tif files of spatial datasets for the representation analysis (e.g., kba_cmi.tif) exist in the "output" subfolder, these datasets do not need to be uploaded again.

### Output

No output is created at this stage.  
