

## Run Builder
Builder assembles catchments along stream networks to a user‐defined size (e.g., MDR) and intactness .Starting from a seed catchment, Builder
prioritizes growth in the upstream direction. This emphasizes inclusion of headwaters. Once all neighbouring upstream catchments are added, growth
redirects downstream until upstream growth is possible. Seed catchments can be restricted to headwater catchments by indication Strahler Order 1 within the planning region.
  
### Using the app
    
- **Set catchment parameters**: Once catchment is uploaded, the app requires users to specify some of the parameters. To define each parameter, the app uses dropdown menus 
dynamically populated with column names from the uploaded catchment layer.
    
The **Run Builder** launch Builder software. A set of output files are saved in the Builder_output folder. The app checks if Builder output files are already present in the specified directory. If input files are already present, the app will automatically upload these files into the environment, making them immediately available 
for use. If output files are missing, the app will launch Builder to generate the necessary files and save them in the selected directory. This approach ensures users save time by reusing existing input 
files while allowing the app to generate any new files required for the builder. If Builder was runned before and the parameters are not the same, a new destination folder needs to be set in order to force Builder to run.

### Output

A series of ouptut files are created by Builder and are saved in the Builder_output folder. From the series of output files created, three are essential to pursue the analysis:

◦ **date_time_All_Unique_BAs.csv** : This file is a list of all unique KBA Areas. The file consists of columns. Each column represents a single KBA Area: row 1 is the unique identifier of the BA, and row 2 to n is a list of the catchments that comprise the Benchmark Area. 

◦ **date_time_ROW_UPSTREAM_CATCHMENTS_ROW.csv** : This file lists the catchments upstream of each potential benchmark.

◦ **date_time_ HYDROLOGY_METRICS.csv** : This file contains hydrology metrics that describe the area and intactness of catchments, and length of stream network, upstream and downstream of each potential benchmark.

All of the filenames include the prefix "date_time_" which is the date and time of when the Builder run was started.

KBAs created by Builder are then saved in the layer **KBAs_builder**.


