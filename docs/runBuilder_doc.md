## Run Builder and calculate DCI

This step runs BUILDER to construct candidate KBAs from the seed list and neighbours table, then calculates hydrology metrics for each KBA.

&#x1F4CC; **Before you start:** `BenchmarkBuilder_cmd.exe` must be in the directory selected in **Set input parameters**, Microsoft .NET Framework must be installed, and **Create Builder input** must be completed.

<br>

### Using the app

#### 1. Specify minimum intactness (0–1) 

Intactness ranges from 0 (0% intact) to 1 (100% intact).

- **Catchment-level**: minimum intactness for a catchment to be added to a KBA. For example, `0.8` means every catchment in the KBA
must be at least 80% intact.
- **KBA-level**: minimum area-weighted mean intactness of the whole KBA. For example, `0.9` means the KBA must be at least 90% intact.

Enter `0` at both levels to remove the influence of intactness. No catchment is then excluded, which maximizes hydrologic connectivity within KBAs. Default: `0` at both levels.
To remove the influence of intactness on the building process, enter 0 at both the catchment- and KBA-level. This will maximize hydrologic connectivity within the KBA because no catchments will be excluded due to intactness.

&nbsp;

#### 2. Specify area target type

Defines what BUILDER sums to decide when the seed's area target is reached:

- **landwater** (default): total area of the KBA.
- **land**: land area only. The KBA will be larger than the target because of its water area.
- **water**: water area only.


Next, identify the attributes in the catchment dataset that will be used for intactness, zone of construction, and area of land via dropdown menus.

&nbsp;

#### 3. Specify catchment attributes**  

- **Zone**: BUILDER does not build KBAs across zones. For example, if the planning region contains two watersheds with different zone 
values, no KBA will cross from one watershed to the other. `ZONE` is selected automatically if present. **A zone must be selected.**
- **Area land**: attribute used to track land area (default `Area_land`). It can be replaced by a custom measure. For example, to 
protect 1,000 km² of caribou habitat, select a `caribou_m2` attribute, choose area target type **land**, and set the seeds' area target 
to 1,000,000,000 m².

Finally, click the "Run Builder" button to start the construction process, which proceeds as follows:  

First, the App will check to see if Builder output files already exist in the subfolder called "Builder_output". If present, the App will automatically upload these files into the R environment, making them immediately available for use. If the parameters for running Builder have changed, a new destination folder needs to be set in order for Builder to run OR the files need to be deleted from the subfolder "Builder_output". 

Second, if there are no files in the "Builder_output" folder, the App will launch Builder, and files will be written to "Builder_output".  

<br>

### Run

Click **Run Builder**. The app:

1. Runs BUILDER. Output tables are written to `Builder_output`, prefixed with the run's date and time. The button changes to 
**Builder output created!**
2. Converts the KBAs to polygons and maps them as **Potential KBAs**.
3. Adds hydrology metrics to each KBA and calculates DCI. A progress bar is shown during this step.

The table under the map shows the number of KBAs created.

&#x1F4CC; **Note:** Each click runs BUILDER again. The app always uses the most recent output in `Builder_output`.

<br>

### KBA attributes
| Attribute | Description |
|---|---|
| `network` | KBA identifier (e.g., KBA_1) |
| `area_km2` | KBA area (km²) |
| `AWI` | Area-weighted mean intactness of the KBA (0–1) |
| `up_km2` | Area upstream of the KBA (km²) |
| `up_AWI` | Area-weighted mean intactness of the upstream area (0–1) |
| `dci` | Dendritic Connectivity Index: 0 = fragmented, 1 = fully connected stream network within the KBA (Cote et al. 2009) |

<br>

### Output
- **`Builder_output` folder**: BUILDER tables, prefixed with the run's date and time. Those used by the app are:
  - `*_COLUMN_All_Unique_BAs.csv`: all unique KBAs, one column per KBA. Row 1 is the KBA identifier, and the following rows list its catchments. Used to create the KBA polygons.
  - `*_Unique_BAs_attributes.csv`: area and intactness (AWI) of each KBA.
  - `*_HYDROLOGY_METRICS.csv`: upstream area and upstream intactness of each KBA.
  - `*_ROW_UPSTREAM_CATCHMENTS.csv`: list of catchment that form the upstream per KBA.
- **`output/KBA_analysis.gpkg`**:
  - `KBAs_builder`: KBA polygons as built by BUILDER
  - `upstream_KBAs`: upstream area of each KBA
  - `KBAs_reducedFALSE`: KBA polygons with all the attributes above. This layer is used in the next steps.

*Cote, D., Kehler, D.G., Bourne, C. et al. (2009). A new measure of longitudinal connectivity for stream networks. Landscape Ecol 24, 101–113.*

