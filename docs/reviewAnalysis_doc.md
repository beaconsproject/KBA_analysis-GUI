## View previous analysis

Reopen results saved in the project, without recalculating them: single KBA/PA assessments, networks, and filtered sets you downloaded.

### Available layers
**Select prior KBAs analyses layer** lists the layers of `output/KBA_analysis.gpkg` whose name starts with:
- `rep`: representation results from **Assess single KBAs** (e.g., `repKBAs_reduced10000`, `repPAs`, `repKBAPAs_reduced10000`) and their filtered downloads
- `net`: networks from **Create and assess KBA networks** (e.g., `netKBAs_reduced10000_n2`) and their filtered downloads

The table under the map shows the number of features in the selected layer.

<br>

### Using the app
1. Select a layer and click **View results**. The layer, the reference area, the protected areas (if any) and the indicator rasters with their legends are added to the map.
2. **Explore**: choose an item in **Select KBA/network** (top right). It is highlighted on the map with its upstream area, along with a summary table and the indicator plots below the map. Click the expand icon to enlarge a plot.
3. **Filter**: set the maximum dissimilarity for each indicator and the maximum upstream area, then click **Apply filtering**. The map, table and selection list are restricted to the items that pass all thresholds.
4. **Download Filtered KBAs**: saves the filtered set to the GeoPackage and copies its plots.

<br>

### Summary table
| Variable | Description |
|---|---|
| Area km2 | Area of the KBA/PA/network |
| AWI (%) | Area-weighted intactness |
| Upstream area km2 / Upstream AWI (%) | Size and intactness of the upstream area |
| DCI | Dendritic Connectivity Index (not available for networks) |
| CMI, GPP, LED, *custom* | Kolmogorov–Smirnov statistic (0 = well represented, 1 = poorly represented) |
| LCC | Bray–Curtis statistic (0–1) |

<br>

### Output
Only when **Download Filtered KBAs** is used:
- a new layer in `output/KBA_analysis.gpkg`, named after the source layer and the thresholds, e.g. `repKBAs_reduced10000_up25000_cmi0.2_gpp0.2_led0.2_lcc0.2`
- copies of the corresponding plots in `output/plot/<that name>/<indicator>/`

&#x1F4CC; **Note:** The reference area, indicator rasters and plots must still exist in the project, so this tab works best on a project whose analyses were completed in earlier sessions.
