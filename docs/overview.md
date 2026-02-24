
## Welcome to the KBA Explorer


**KBA Explorer** is a decision-support tool designed to facilitate the identification, evaluation, and ranking of potential **Key Biodiversity Areas (KBAs)** within a defined region.

A KBA is a site that contributes to the global persistence of biodiversity. KBA Explorer enables users to design conservation
areas according to defined criteria. To do this, it uses the Benchmark BUILDER software, a user friendly application, developed in C# .NET framework. 
BUILDER explicitly incorporates **hydrologic connectivity** for the integration of aquatic and terrestrial conservation planning in KBAs design. BUILDER 
constructs conservation areas using a deterministic construction algorithm that aggregates catchments to a user defined **size** (reflecting system resilience to disturbance
and **intactness** (representing the absence of industrial activity and serving as a proxy for the integrity of ecological processes).

Depending on the parameter values selected, this process can generate a substantial number of potential KBA candidates.To identify which of these sites best capture ecological variation, 
users can conduct a representation analysis inside **KBA Explorer** based on four criteria: 

  - Climate Moisture Index (CMI), which provides a measure of the climatic conditions that influence species distributions and ecosystem processes. 
  - Gross Primary Productivity (GPP), which indicates an index of overall ecosystem productivity
  - Lake Edge Density (LED), which captures landscape heterogeneity and habitat availability at aquatic–terrestrial interfaces
  - Land Cover (LCC), which provides insight into habitat composition and landscape heterogeneity.
  
Once an index has been calculated for each criterion, users can filter the resulting candidate KBAs according to specific thresholds or representation objectives, 
allowing the final set of conservation areas to be refined in line with management or planning goals.

To meet conservation objectives, particularly in large or heterogeneous landscapes, a single KBA may not be sufficient. **KBA Explorer** can address this by 
constructing networks of KBAs, grouping multiple sites to collectively achieve conservation targets. Once a network is proposed, the application can perform a 
representation analysis across all included KBAs, using the same four criteria to evaluate which network most effectively captures environmental variation.


&#x1F4CC; **Note:** In order to run **KBA Explorer**, you need to acquire Benchmark BUILDER from the BEACONs team and have Microsoft .NET Framework installed on a Windows system. 

<br>

### Input data
  
**KBA Explorer** requires several key spatial layers. Users can either upload the spatial layers as ShapeFiles or provide a CSV file that specifies access to each spatial layer.
Please refer to the **Dataset Requirements** tab for details on the required spatial layers and associated attributes and formatting.

### Functionality
    
The app consists of five sections:
<br>

#### Set input parameters

  - Select an output directory where Benchmark BUILDER software is located.
  
  - Select an existing project or create a new project where candidate KBAs and KBA network will be saved. 
    
  - Upload all necessary spatial layers.
  
  - Specify intactness attributes within the catchments layer.

&#x1F4CC; **Note:** All layers must have the same projection. Additionally, the study area must capture the full extent of the criteria layers to ensure accurate analysis.

<br>

#### Add display elements (OPTIONAL)

This section allows users to add additional features for visualization. These features must be vector data (points, lines, or polygons) and 
cannot be rasters. A maximum of three additional vector features can be added. The file or layer names are automatically used as display names on 
the map. Colors are assigned by the app and cannot be modified.

<br>

#### Build KBAs

This section guides users through the process of generating candidate KBAs using Benchmark BUILDER. It covers the creation of necessary input tables (seed and neighbor 
tables), running BUILDER to delineate KBAs and calculate the Dendritic Connectivity Index (DCI), and optionally refining the results by reducing spatial redundancy based 
on user-defined grid criteria. Steps are:
    
  - Create BUILDER input (seed and neighbour tables)
    
  - Run BUILDER and calculate Dendritic Connectivity Index (DCI).

  - Optional: Reduce KBAs based on spatial similarity within a user defined grid. 
<br>  
 
#### Assess representation

This section enables users to evaluate how well candidate KBAs — or networks of KBAs — capture key environmental variation based on the four criteria: CMI, GPP, LED, and LCC. 
Users can then choose one of two approaches to perform the assessment:

  - Assess single KBAs

  - Create and assess networks

Both options allow setting thresholds on the criteria to filter and identify the best-performing KBAs or networks.    
<br>
  
#### Convert as ShapeFiles

All output layers generated during the analysis are saved in a GeoPackage. For users who prefer or require working with individual Shapefiles, this optional step 
allows you to convert each layer from the GeoPackage into a separate Shapefile. The process automatically loops through all layers in the GeoPackage, creating Shapefiles 
that can be easily opened and used in other GIS software. This step is particularly useful for users who encounter compatibility issues with GeoPackages or need to share 
layers in the widely supported Shapefile format.

