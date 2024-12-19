

## Run Builder

Before continuing, ensure the command line version of the Builder software is in the R library (e.g., C:\Program Files\R\R-4.4.1\library\beaconsbuilder) and that Microsoft .NET Framework is installed on the computer.

Here, the user specifies parameters for running Builder.
  
### Using the app

First, sets the minimum intactness standards for a candidate KBA. Intactness values range from 0 (0% intact) to 1 (100% intact).

**Specify minimum intactness (0-1)**  
**catchment-level**: This is the minimum allowable intactness for a catchment to be included in the construction of a KBA. For example, if all catchments in the KBA must be ≥ 80% intact, enter 0.8.    
**KBA-level**: This is the overall intactness for the KBA and is measured as the catchment area-weighted mean intactness. For example, if the KBA must be ≥ 90% intact, enter 0.9. 

To remove the influence of intactness on the building process, enter 0 at both the catchment- and KBA-level. This will maximize hydrologic connectivity within the KBA because no catchments will be excluded due to intactness.

Next, specify what Builder will track to determine when the area target (e.g., 10,000 km2) is met. 

**Specify area target type for KBA size** - As Builder assembles catchments, it tracks the area of land (land), area of water (water), and area of land and water (landwater) by summing amounts in the catchment dataset attribute table. If the area target is the total area of the KBA, the user would select "landwater". If the area target is for land only, the user would select "land" - in this case, the overall size of the KBA would be larger than the area target due to water in the KBA. 

Next, identify the attributes in the catchment dataset that will be used for intactness, zone of construction, and area of land via dropdown menus.

**Specify catchment attributes**  
**intactness**: This is the catchment intactness attribute (proportion 0-1) that is used by Builder to determine if a catchment is included in the building process and to calculate the overall intactness of the KBA (see "Specify minimum intactness" above).  
**zone**: This attributes restricts Builder to a zone when building KBAs. For example, if the planning region is comprised of two watersheds. Each watershed could be assigned to a separate zone. As such, Builder would not construct KBAs that cross the watershed boundary. Construction would stay within a watershed.   
**area land**: This is the attribute used to track the area of land (Area_land) in the KBA. This attribute can be hijacked to track a custom measure for the area target. For example, if the objective was to protect 1000 km2 of caribou habitat, the catchment dataset would include an attribute for the amount of caribou habitat in each catchment (e.g., caribou_m2). Builder would track the amount of caribou habitat and stop construction when 1000km2 was met. For this to work, the user would specify the area target type as "land" (see above) and set the catchment's "area land" attribute as "caribou_m2".  

Finally, the user clicks the "Run Builder" button to start the construction process, which proceeds as follows:  

First, the App will check to see if Builder output files already exist in the subfolder called "Builder_output". If present, the App will automatically upload these files into the R environment, making them immediately available for use. If the parameters for running Builder have changed, a new destination folder needs to be set in order for Builder to run OR the files need to be deleted from the subfolder "Builder_output". 

Second, if there are no files in the "Builder_output" folder, the App will launch Builder, and files will be written to "Builder_output".  

### Output

Builder produces a suite of files that are written to the subfolder "Builder_output". Based on these files, a spatial layer of KBA polygons will be written to a geopackage in the folder called "output".

From the suite of files created, there are three essential files used by the App:

◦ **date_time_All_Unique_BAs.csv** : This file is a list of all unique KBA in column format. Each column represents a single KBA. Row 1 is the unique identifier of the KBA, and row 2 to n is a list of the catchments that comprise the KBA. 

◦ **date_time_ROW_UPSTREAM_CATCHMENTS_ROW.csv** : This file lists the catchments upstream of each KBA.

◦ **date_time_ HYDROLOGY_METRICS.csv** : This file contains hydrology metrics that describe the area upstream and downstream of each KBA, and associated intactness and length of stream network. 

All of the filenames include the prefix "date_time_" which is the date and time of when the Builder run was started.

KBAs created by Builder are converted to polygons and saved to a geopackage called "KBA_analysis.gpkg". The polygon layer is called **KBAs_builder**.


