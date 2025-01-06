
## Create Builder input

Builder software constructs KBAs by assembling catchments using hydrology-based rules, to a user-specified size and intactness. The construction of a KBA starts from a 'seed' or single catchment. 

Builder requires three input files:

1. catchments (catchments.csv) *This file is automatically created by the App, so user action is not required. 
2. seed list (seed.csv) *The seed list is a list of catchment seeds with an associated area target (m2). 
3. neighbours file (nghbrs.csv) *This file lists catchments and their neighbouring catchments.

These input files are uploaded into the analysis at this step. 

### Using the app

In this step, either upload or create the neighbours file and seed list. 

**Upload neighbours .csv** - If the neighbours file exists, navigate to the file and open. If the neighbours file does not exist, the App will automatically create the file when the "Create Builder Input" button is clicked.

**Upload seedlist .csv** - If the seed list exists, navigate to the file and open. If the seed list does not exist, create a seed list. 

**Create seedlist (if required)**
The seed list is created by querying the catchment dataset using catchment intactness and Strahler Order. The query will only select catchments from within the ecoregion (eco = 1). The following inputs are required:

- **Specify intactness attribute** *This is the catchment intactness attribute e.g., intactKBA.  

- **Specify minimum seed intactness (0-1)** *Only catchments ≥ to the specified intactness will be included in the seed list.  

- **Specify Strahler Order ≤** *Only catchments with a Strahler Order ≤ to the specified number will be included in the seed list. For example, if "2" is entered, the seed list will only include Strahler Order 1 and 2 catchments. We recommend starting with Strahler Order 1 or headwater catchments only, especially in highly intact landscapes. 

- **Specify area target (m2)**  *All seeds will be assigned this area target. If a variable area target is desired, a custom seed list will need to be created outside of the App.  

Click the "Run Builder input" button to either complete the upload or creation of files. If the neighbours files does not exist, it will be automatically created. 

The seed list and neighbours files are uploaded or created and saved in folder called **Builder_input** in the Output directory. The files will be named seed.csv and nghbrs.csv.

### Output

The seedlist (seed.csv) and neighbour table (nghbrs.csv) are created and saved in the Builder_input folder.

A geopackage named **KBA_analysis** is also initialized in the output folder at this stage and will serve to hold all intermediate layers generated in the analysis. 

