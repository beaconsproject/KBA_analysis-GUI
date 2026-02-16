# Add Additional Metrics to KBA_Explorer Output

### Sections

- [Purpose](#purpose)
- [Available Types of Analysis](#available-types-of-analysis)
- [Project Files and Their Roles](#project-files-and-their-roles)
- [Example of workflow](#example-of-workflow)

This repository contains scripts and tools to calculate network-level metrics used to evaluate and filter KBA networks.

The objective of this analysis is to support management planners in identifying KBA configurations that best meet predefined conservation targets (e.g., connectivity, coverage, resilience, representation).

Rather than evaluating sites in isolation, this framework treats KBAs as part of a network, allowing planners to assess how different combinations of sites perform collectively.

## Purpose

When generating network with the KBA Explorer, multiple candidate networks are produced that meet minimum spatial or ecological requirements, such as size and intactness. KBA Explorer offers to filter the network
based on 4 primary criteria (cmi, gpp, led and lcc). While these criteria provide an important first-level screening, additional metrics may be critical for informed decision-making. 

These additional criteria can help assess:

This folder provides functions that allow users to compute additional metrics on each candidate network. These functions can be applied to any ecological layer (e.g., caribou, wolverine, marten habitat, climate velocity, land cover, or other spatial indicators).

## Available Types of Analysis

### `amount_area_vect`

Calculates the amount (area) of a given vector feature within each network.

**Typical use cases:**
- Area of species habitat polygons inside the network  
- Area of intact forest within the network  
- Overlap with designated management zones  

This helps users quantify how much of a specific spatial feature is captured by each network.

---

### `amount_area_rast`

Calculates the amount (area) of a given raster-based feature within each network.

**Typical use cases:**
- Area of suitable habitat derived from a habitat suitability map
- Area of climate refugia
- Area of high-value conservation pixels

This allows users to compare networks based on raster-derived indicators.

---

### `calc_dissimilarity_cat`

Calculates a categorical dissimilarity metric between a candidate network and a reference area.

**Typical use cases:**
- Comparing land cover composition
- Selecting the network most similar to a reference area

This helps identify which network best matches a desired ecological composition.

---
### `calc_dissimilarity_cont`

Calculates a continuous dissimilarity metric between a candidate network and a reference area.

**Typical use cases:**
- Comparing continuous habitat suitability values
- Comparing climate velocity distributions
- Evaluating similarity in ecological gradients

This supports selection of networks that most closely resemble a target condition.

### `arithmetic_mean`

Calculates the arithmetic mean of a continuous raster within each network.

**Typical use cases:**
- Mean habitat suitability score
- Mean climate velocity
- Mean ecological integrity value

Useful for comparing overall average performance across networks.

### `geometric_mean`

Calculates the geometric mean of values within each network.

**Typical use cases:**
- ...
- ...

This metric is ...

---

This post-analysis framework allows a more flexible and objective comparison of candidate protected area networks beyond the initial KBA filtering criteria.

By extending the evaluation beyond the initial KBA filtering criteria, this post-analysis framework enables planners to select networks that best align with specific conservation objectives and management priorities.

## Project Files and Their Roles

The analysis requires three main files in the project directory set by the KBA Explorer:

### `inputLayers.csv`

This file outlines ecological criteria to be included and the type of analysis to be performed for each. For every input layer, it specifies:

**Variable name** – An accronym to be used in the output shapefile where criteria will be saved.

**Access path** – Location of the input file (vector or raster).

**Type of analysis** – Specify which metric function to apply (see [Available Types of Analysis](#available-types-of-analysis)).

**Value**	- Raster values to consider in the calc_dissimilarity_cat and amount_area_rast. Default is NA which mean all values are considered. 

**Range**	- Range of raster values to consider in the calc_dissimilarity_cont and amount_area_rast. Default is NA which mean all values are considered. 

**Plot** - Path to folder in which to save dissimilarity plots. Only used by the calc_dissimilarity_cont and calc_disssimilaryity_rast. Default is not to create plots.  

*Note that the file found on GitHub is an example of what the file should look like. You will need to modify it according to your needs. 

### `net_metrics.R`

This script contains all the helper functions used to calculate network-level metrics (see [Available Types of Analysis](#available-types-of-analysis)).

It does not run the analysis on its own — it simply provides the functions that run_net_metrics.R calls.

### `run_net_metrics.R`

This is the main execution script. It reads the inputLayers.csv, access each criteria, selects the appropriate analysis function from net_metrics.R and computes the requested network metrics. Essentially, run_net_metrics.R orchestrates the workflow using the helper functions and input definitions.

## Example of workflow

**1. Download the require files**

Prior to running the analysis, ensure the following three files are downloaded into your KBA Explorer project directory:
```KBA_Explorer/
├── BUILDER_input/
├── BUILDER_output/
├── data/
├── output/
│   └── KBA_Analysis.gpkg
├── inputLayers.csv 
├── net_metrics.R
└── run_net_metric.R
```
 
**2. Configure inputLayers.csv**

Make sure all fields are properly filled, including custom criteria, file paths, and the type of analysis to perform.

**3. Set up the R environment**

In R Studio, open run_net_metric.R and set the working directory to the KBA Explorer project directory.

**4. Load the required libraries**

Load all necessary R packages for the analysis.

**5. Source helper functions**

Run net_metrics.R to load the helper functions in the environment.

**6. Specify the network layer and network name**

Set the layer in KBA_Analysis.gpkg on which you want the metrics to be calculated (kba_layer) and indicates the name of the column holding the network name (net_name).

**7. Define output file**

Set the path and file name where the updated layer with metrics should be saved (outLayer).  

**8. Run the analysis**

Source the file to run the analysis and save the results.  


