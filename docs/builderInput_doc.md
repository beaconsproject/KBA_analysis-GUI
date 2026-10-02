
## Create Builder input

Builder constructs KBAs by assembling catchments using hydrology-based rules, to a user-specified size and intactness. The construction of a 
KBA starts from a single 'seed' catchment. This step prepares the two tables BUILDER needs:

1. **Seed list** (i.e.; seed.csv): the catchments from which KBAs are built, each with an area target (m2). 
2. **Neighbours table** (i.e.; nghbrs.csv): each catchment and the catchments it touches.

The catchments table also required by BUILDER is created automatically in the next step (**Run Builder**).

&#x1F4CC; **Note:** The intactness attribute used here is the one selected in **Set input parameters**. 
The catchments layer must contain `CATCHNUM` and `STRAHLER`.

<br>

### Using the app

#### Option A: Use existing files

Upload a seed list and/or a neighbours table created earlier by the app or by BUILDER. Uploaded files are used as-is, and the 
corresponding **Create** settings are ignored. In this step, either upload or create the neighbours file and seed list. 

**Seed list**: one row per seed catchment:

| Column | Description | 	
|---|---|
| `CATCHNUM` | Catchment identifier, matching `CATCHNUM` in the catchments layer |
| `Areatarget` | Minimum KBA size to reach from this seed (m²) |

Use this option if seeds need different area targets.

**Neighbours table**: one row per pair of touching catchments:

| Column | Description |
|---|---|
| `CATCHNUM` | Catchment identifier |
| `neighbours` | Identifier of a catchment sharing at least one point with it |
| `key` | Row index starting at 0 |

&nbsp;

#### Option B: Create the seed list
If no seed list is uploaded, the app selects seed catchments from the catchments layer:

| Setting | Description | Default |
|---|---|---|
| **Specify intactness attribute** | Keep catchments with intactness ≥ this value | `0` |
| **Specify Strahler Order ≤** | Keep catchments with a stream order **equal to** this value (`1` = headwater catchments). We recommend starting with order 1, especially in highly intact landscapes. | `1` |
| **Specify area target (m2)** | Applied to every seed. For variable targets, upload a custom seed list (Option A). | `10,000,000,000` (10,000 km²) |
| **Constrain seeds to the reference area** | Keep only catchments lying entirely within the reference area. If no reference area is loaded yet, an upload box appears. | Off |

If no neighbours table is uploaded, the app creates one. Catchments are neighbours when they share at least one point.

<br>

#### Run
Click **Set Builder input**. Creating the neighbours table can take several minutes for large catchment datasets. When the step is complete, 
the button changes to **Builder input now set!**

Possible messages:
- **Seed intactness must be between 0-1**: correct the value and click again.
- **No seed catchment found**: no catchment meets the criteria. Lower the minimum intactness or change the Strahler order.

&#x1F4CC; **Note:** When an existing project is reopened, the saved `seeds.csv` and `nghbrs.csv` are reloaded automatically, and this step can be skipped.

<br>

### Output
Saved in the project's `Builder_input` folder:
- `seeds.csv`: seed list
- `nghbrs.csv`: neighbours table
