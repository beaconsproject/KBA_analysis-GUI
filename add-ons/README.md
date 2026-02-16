# KBA Explorer add ons

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

`amount_area_rast`

Calculates the amount (area or proportion) of a given raster-based feature within each network.

Typical use cases:

Area of suitable habitat derived from a habitat suitability map

Area of climate refugia

Area of high-value conservation pixels

This allows users to compare networks based on raster-derived indicators.

calc_dissimilarity_cat

Calculates a categorical dissimilarity metric between a candidate network and a reference area.

Typical use cases:

Comparing land cover composition

Comparing habitat class proportions

Selecting the network most similar to a benchmark conservation area

This helps identify which network best matches a desired ecological composition.

calc_dissimilarity_cont

Calculates a continuous dissimilarity metric between a candidate network and a reference area.

Typical use cases:

Comparing continuous habitat suitability values

Comparing climate velocity distributions

Evaluating similarity in ecological gradients

This supports selection of networks that most closely resemble a target condition.

arithmetic_mean

Calculates the arithmetic mean of a continuous raster within each network.

Typical use cases:

Mean habitat suitability score

Mean climate velocity

Mean ecological integrity value

Useful for comparing overall average performance across networks.

geometric_mean

Calculates the geometric mean of values within each network.

Typical use cases:

Combining multiple performance indicators

Penalizing low values in multi-criteria evaluation

Creating composite indices where balance among indicators is important

This metric is particularly useful when low values in one criterion should strongly influence the overall score.

Application

These functions are species-agnostic and indicator-agnostic. Users can apply them to any spatial layer relevant to their conservation objective.

By combining these analyses, planners can:

Quantify representation of key habitats

Compare networks to ecological reference conditions

Integrate continuous model outputs

Rank candidate networks using composite metrics

This post-analysis framework allows a more flexible and objective comparison of candidate protected area networks beyond the initial KBA filtering criteria.

By extending the evaluation beyond the initial KBA filtering criteria, this post-analysis framework enables planners to select networks that best align with specific conservation objectives and management priorities.


The content of this folder enables:

Calculation of ecological and structural network metrics

Comparison between alternative protected area networks

Filtering of candidate networks based on quantitative performance thresholds

Support for evidence-based decision-making

These metrics help planners move from “Does this network meet the minimum target?” to “Which network performs best given our objectives?”



📂 Folder Structure

Example structure (adapt to your actual files):

/network_metrics/
│
├── data/                  # Input spatial or network data
├── scripts/               # Metric calculation scripts
├── config/                # Parameter files (targets, thresholds)
├── outputs/               # Generated metric results
└── README.md              # Documentation (this file)

Key Components

Data folder
Contains input files representing candidate protected area networks.

Scripts
Compute network metrics and produce summary tables for comparison.

Configuration files
Define analysis parameters such as connectivity thresholds, conservation targets, or filtering criteria.

Outputs
Store calculated metrics and ranked network results.

▶️ How to Run the Analysis

Place candidate network data in the /data folder.

Adjust analysis parameters in the /config file.

Run the main script:

python run_network_metrics.py


Results will be saved in the /outputs directory.
