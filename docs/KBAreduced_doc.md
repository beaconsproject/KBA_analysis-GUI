## Reduce KBAs (OPTIONAL)

Depending on the seed list and the size of the planning region, BUILDER can produce tens to thousands of KBAs, many of them spatially 
similar. This step groups similar KBAs and keeps only the best one in each group, which greatly speeds up the representation and network 
analyses.

You can skip this step: **Assess representation** can also use all KBAs (layer `KBAs_reducedFALSE`).

<br>

### How KBAs are reduced

A grid covering the KBAs is created, and each KBA is assigned to the grid cell containing its centroid. The smaller the cell, the more 
similar the KBAs within a group. A 10,000 m × 10,000 m cell works well in most cases, but other sizes are worth exploring. Within each group,
KBAs are ranked by:
   1. **DCI**, highest first (best internal hydrologic connectivity)
   2. **Upstream area**, smallest first
   3. **Upstream intactness (up_AWI)**, highest first

The top-ranked KBA of each group is kept.

<br>

### Using the app

1. When you open this tab, the KBAs created in **Run Builder and calculate DCI** are loaded. The table under the map shows their number.
2. Enter the **grid cell size** in metres (default `10000`, i.e. a 10 km × 10 km cell) and click **Run**. The reduced KBAs are displayed 
as **Potential KBAs (reduced)**, and the table shows the number kept.
3. To compare, change the grid size and click **Run** again.
4. Click **Save reduced KBAs in GPKG** to keep the result. **Only saved reductions are available in the next steps.**

&#x1F4CC; **Note:** If a reduction with the same grid size was already saved, it is reloaded instead of recalculated.

### Output

Added to `output/KBA_analysis.gpkg` when you click **Save**:
- `KBAs_reduced<grid size>` (e.g., `KBAs_reduced10000`): the reduced KBAs, with the same attributes as `KBAs_reducedFALSE` (`network`, `area_km2`, `AWI`, `up_km2`, `up_AWI`, `dci`) plus `group_id`, the grid cell of each KBA.

The removed KBAs remain available in `KBAs_reducedFALSE` (all KBAs with metrics) and `KBAs_builder`.