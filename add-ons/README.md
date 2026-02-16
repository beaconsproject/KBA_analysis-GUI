# KBA Explorer add ons

## Table of Contents

- [Purpose](#purpose)
- [Available Types of Analysis](#available-types-of-analysis)
- [Project Files and Their Roles](#project-files-and-their-roles)
- [Example of workflow](#example)

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

This file defines the ecological criteria to add and thetype of analysis to run for each dataset. For each input layer, it specifies:

**Variable name** – an accronym to be used in the output shapefile where criteria will be saved
**Access path** – location of the input file (vector or raster)
**Type of analysis** – which metric function to apply (see [Available Types of Analysis](#available-types-of-analysis))
**Value**	- Raster values to consider in the calc_dissimilarity_cat and amount_area_rast. Default is NA which mean all values are considered. 
**Range**	- Range of raster values to consider in the calc_dissimilarity_cont and amount_area_rast. Default is NA which mean all values are considered. 
**Plot** - Path to folder in which to save dissimilarity plots. Only used by the calc_dissimilarity_cont and calc_disssimilaryity_rast. Default is not to create plots.  

### `net_metrics.R`

This script contains all the helper functions used to calculate network-level metrics, including:

amount_area_vect

amount_area_rast

calc_dissimilarity_cat

calc_dissimilarity_cont

arithmetic_mean

geometric_mean

It does not run the analysis on its own — it simply provides the functions that run_net_metrics.R calls.

### `run_net_metrics.R`

This is the main execution script. It:

Reads the inputLayers.csv control file

For each input layer, selects the appropriate analysis function from net_metrics.R

Computes the requested network metrics

Saves the results to the output/ folder

Essentially, run_net_metrics.R orchestrates the workflow using the helper functions and input definitions.

## How to Run the Analysis

Prior to running the analysis, make sure the following files are present in the project directory used for KBA Explorer:
- inputLayers.csv
- net_metrics.R
- run_net_metrics.R

**inputLayers.csv** 

Run the main script:

python run_network_metrics.py


Results will be saved in the /outputs directory.
