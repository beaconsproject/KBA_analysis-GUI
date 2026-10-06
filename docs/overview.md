
## Welcome to the KBA Explorer


**KBA Explorer** is a decision-support tool designed to facilitate the identification, evaluation, and ranking of potential **Key Biodiversity Areas (KBAs)** within a defined region.

A KBA is a site that contributes to the global persistence of biodiversity. KBA Explorer enables users to design conservation
areas according to defined criteria. To do this, it uses the Benchmark BUILDER software, a user friendly application, developed in C# .NET framework. 
BUILDER explicitly incorporates **hydrologic connectivity** for the integration of aquatic and terrestrial conservation planning in KBAs design. BUILDER 
constructs conservation areas using a deterministic construction algorithm that aggregates catchments to a user defined **size** (reflecting system resilience to disturbance
and **intactness** (representing the absence of industrial activity and serving as a proxy for the integrity of ecological processes).

Depending on the parameter values selected, this process can generate a substantial number of potential KBA candidates. To identify which of these sites best capture ecological variation, 
users can conduct a representation analysis inside **KBA Explorer** based on four criteria: 

  - Climate Moisture Index (CMI), which provides a measure of the climatic conditions that influence species distributions and ecosystem processes. 
  - Gross Primary Productivity (GPP), which indicates an index of overall ecosystem productivity
  - Lake Edge Density (LED), which captures landscape heterogeneity and habitat availability at aquatic–terrestrial interfaces
  - Land Cover (LCC), which provides insight into habitat composition and landscape heterogeneity.
  
&#x1F4CC; **Note:** An optional **fifth, custom criterion** (any continuous raster) can be added.
  
Representation is measured against a **reference area**, the region the KBAs are meant to represent (e.g., an ecoregion). The reference area can be the same as the planning 
region or smaller. Key metrics are as follow:

| Metric | Description | Range |
|---|---|---|
| AWI | Area-weighted intactness: proportion of the area that is intact | 0-1 |
| Upstream area / upstream AWI | Size and intactness of the land draining into the KBA, PA or network | sq.km / 0-1 |
| DCI | Dendritic Connectivity Index: longitudinal connectivity of the stream network within the area | 0 (fragmented) - 1 (connected) |
| Dissimilarity metric (DM) | Kolmogorov–Smirnov (continuous criteria) or Bray–Curtis (LCC) statistic comparing the area with the reference area | 0 (well represented) - 1 (poorly represented) |

<br>

Existing **protected areas (PAs)** can optionally be included. The app calculates the same hydrology metrics for PAs as for KBAs, so PAs can be assessed on their own, together 
with KBAs, or forced into KBA networks.

Once an index has been calculated for each criterion, users can filter the resulting candidate KBAs according to specific thresholds or representation objectives, 
allowing the final set of conservation areas to be refined in line with management or planning goals.

To meet conservation objectives, particularly in large or heterogeneous landscapes, a single KBA may not be sufficient. **KBA Explorer** can therefore build **networks of KBAs** 
(and PAs) that together achieve conservation targets, and assess their representation using the same criteria.


&#x1F4CC; **Note:** To run **KBA Explorer**, you need Benchmark BUILDER (`BenchmarkBuilder_cmd.exe`, available from the BEACONs team) and have Microsoft .NET Framework installed locally on your Windows system. 

<br>
<br>

#### Input data
  
**KBA Explorer** requires several spatial layers: catchments, streams, planning region, and the CMI, GPP, LED and LCC rasters. Protected areas, a reference area and one 
custom criterion are optional. Layers can be uploaded individually or listed in a CSV file that gives the path to each layer. See the **Dataset Requirements** tab for the required attributes and formatting.

&#x1F4CC; **Note:** All layers must use the same projected coordinate reference system (CRS). The CRS must be a standard, recognized 
coordinate system (e.g., identified by an EPSG or ESRI code). Custom or unrecognized projections are not supported. A projected CRS is 
required to ensure accurate area calculations and spatial statistics. Additionally, the study area must encompass the full extent of all 
criteria layers to ensure accurate analysis.

<br>

#### Project folder and outputs

Each project is stored as a folder within the BUILDER directory:

| Folder | Content |
|---|---|
| `data/` | Uploaded layers and `layer_paths.csv` |
| `Builder_input/` | Seed and neighbour tables |
| `Builder_output/` | Raw BUILDER outputs |
| `output/` | `KBA_analysis.gpkg` (all result layers), criteria rasters clipped to the reference area, and `plot/` (representation plots) |

Main layers in `KBA_analysis.gpkg`: `KBAs_builder`, `KBAs_reducedFALSE` (all KBAs with metrics), `KBAs_reducedxxxxxx>`, `upstream_KBAs`, `protected_areas`, `protected_areas_upstream`, `repKBAs_*`, `repPAs`, `net*`, and `upstream_net*`.

&#x1F4CC; **Note:** When an existing project is reopened, results already saved in the GeoPackage are reused rather than recalculated.

<br>

#### Typical workflow

The workflow below outlines the main steps for using KBA Explorer, from project setup and KBA construction to representation assessment and network development.

1. **Set input parameters**: Select the directory where Benchmark BUILDER software is located, create or open a project, load the spatial layers, and select the intactness attribute.
2. **Add display elements** (optional): add up to three vector layers for visual reference.
3. **Build KBAs**: create BUILDER input, run BUILDER and calculate DCI, then optionally reduce the number of KBAs.
4. **Assess single KBAs** (optional): measure how well each KBA and/or PA represents the reference area, and filter the results.
5. **Create and assess KBA networks**: combine KBAs (and optionally PAs) into networks, assess them, and filter the results.
6. **View previous analysis**: reopen any saved representation or network result.
7. **Convert as Shapefiles** (optional): export all results from the GeoPackage as shapefiles.

For step-by-step instructions, see the help panel of each section.


### KBA Explorer workflow diagram

The worflow diagram below provides an overview of the process.

<br><br>
<center><img src="pics/workflow.png" width="800"></center>
<br><br>

