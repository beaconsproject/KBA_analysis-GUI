## Create Networks

A single KBA or PA may not represent the reference area well enough. This step combines several KBAs (and optionally all protected areas) 
into **networks**, then assesses the representation of each network as a whole, using the same indicators and dissimilarity metrics as 
**Assess single KBAs**.

<br>

### How networks are built
- Every combination of *n* KBAs from the selected layer is created. For example, 3 KBAs taken 2 at a time gives 3 networks: KBA_1 + KBA_2, 
KBA_1 + KBA_3 and KBA_2 + KBA_3.
- Combinations containing **overlapping KBAs** are discarded.
- If **Include all PAs in the network** is checked, all protected areas are added to every network.
- Each network is assessed as one unit. Its area, intactness, upstream area and dissimilarity metrics are calculated for all its parts 
together.

&nbsp;

&#x1F4CC; **Note:** The number of combinations grows very quickly. For example, 50 KBAs give 1,225 networks of 2 and 19,600 networks of 3. 
Reduce the number of KBAs first (**Reduce KBAs**), and start with 2 KBAs per network.

<br>

1. **Select KBA Layer**: the KBAs to combine, either `KBAs_reducedFALSE` (all KBAs) or a saved reduction such as `KBAs_reduced10000`.
2. **Set number of potential KBAs per network**: minimum 2, and no more than the number of KBAs in the layer.
3. **Include all PAs in the network** (optional; shown only when protected areas were provided in **Set input parameters**).
4. Click **Build network**. The app builds the networks, calculates their metrics with a progress bar, and displays the indicator rasters on the map. This can take a long time when there are many combinations. Networks already built with the same settings are reloaded instead of recalculated.
5. **Explore**: choose a network in **Select network** (top right). It is shown on the map with its upstream area, along with a summary table and the indicator plots below the map.
6. **Filter**: set the maximum dissimilarity for each indicator and the **maximum upstream area**, then click **Apply filtering**. The counts table and the **Select network** list are restricted to the networks that pass all thresholds.
7. **Download Filtered Networks**: saves the filtered networks to the GeoPackage and copies their plots (see Output).

<br>

### Network names
A network is named after its KBAs, joined by `__`, e.g. `KBA_3__KBA_12`. When PAs are included, they appear as `PAs`, e.g. `KBA_3__KBA_12__PAs`.

<br>

### Summary table
| Variable | Description |
|---|---|
| Area km2 | Total area of the network |
| AWI (%) | Area-weighted intactness of the network |
| Upstream area km2 | Area upstream of the network |
| Upstream AWI (%) | Area-weighted intactness of the upstream area |
| DCI | Not calculated for networks (NA) |
| CMI, GPP, LED, *custom* | Kolmogorov–Smirnov statistic (0 = well represented, 1 = poorly represented) |
| LCC | Bray–Curtis statistic (0–1) |

<br>

### Output
In `output/KBA_analysis.gpkg`:
- `net<KBA layer>_n<number>[_includePAs]` (e.g., `netKBAs_reduced10000_n2_includePAs`): all networks, with `network`, `area_km2`, 
`AWI`, `up_km2`, `up_AWI`, `cmi`, `gpp`, `led`, `lcc` and the custom indicator, if used
- `upstream_<network layer>`: upstream area of each network
- `net<KBA layer>_up<max upstream>_cmi<…>_gpp<…>_led<…>_lcc<…>` (e.g., `netKBAs_reduced10000_n2_up25000_cmi0.2_gpp0.2_led0.2_lcc0.2`): filtered networks saved with **Download Filtered Networks**

In `output/plot/`:
- `<network layer>/<indicator>/<network>.png`: distribution plots for each network
- `<filtered layer name>/<indicator>/`: copies of the plots for the filtered networks

