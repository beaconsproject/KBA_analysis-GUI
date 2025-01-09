## Assess representation of KBAs and PAs

**Incomplete draft - more to come!**

KBAs and PAs representative of the planning region and/or reference area are identified using four biophysical indicators of environmental variation, which serve as surrogates for biodiversity: soil moisture (CMI), primary productivity (GPP), lake‐edge density (LED), and land cover (LCC). These indicators are described on the **Overview - Dataset** tab. A optional fifth indicator can be used - see "Set parameter inputs".

Representation is assessed using two dissimilarity metrics (DMs): Kolmogorov‐Smirnov (KS) for continuous indicators (CMI, GPP, LED) and Bray‐Curtis (BC) for categorical indicators (landcover or LCC). Dissimilarity metrics compare the distribution of indicators within KBAs and/or PAs against the distribution within a reference area. 

The reference area may or may not be the same as the planning region. For example, the planning region is the extent at which KBAs are identified such as an ecoregion plus intersecting FDAs (i.e., watersheds), while the reference area may be restricted to the ecoregion.

The indicator distributions are based on pixel‐level values. Both dissimilarity metrics, range from 0 to 1, where 0 is most similar and 1 is most dissimilar. The closer the two distributions are to each other, the more representative the KBA or PA is to its reference area and the lower the value of the dissimilarity metric. 

For each KBA / PA, the App produces plots of the distributions used to generate the DM (Figures 1 and 2). 

<insert examples of plots>
<center><img src="pics/figure1_KSplot.png" width="800"></center>

**Figure 1.** Density plots show the distribution of the indicator within the KBA or PA (red) and within the reference area (blue). The Kolmogorov‐Smirnov (KS) statistic describes the dissimilarity between these distributions, and ranges in value from 0 to 1, where 0 indicates perfect proportional representation within the KBA or PA. Portions of the KBA or PA distribution (red) that fall below the blue represent values for which proportional representation was not achieved.

<insert examples of plots>

**Figure 2.** Barplots show the proportions of each indicator class (i.e., land cover types) within the KBA or PA (bars) and within the reference area (black dots). The Bray‐Curtis (BC) statistic describes the dissimilarity between the bars and dots, and ranges in value from 0 to 1, where 0 indicates perfect proportional representation within the KBA or PA. Bars that fall below the black dots indicate that proportional representation of that class was not achieved. 

### Using the app

First, upload the reference area shapefile. If this shapefile was uploaded earlier under **Set input parameters** via the csv file, the shapefile does not need to be uploaded again.

**Upload reference area shapefile**

To upload the shapefile, Browse to the location of the file and select all files associated with the shapefile (.shp, .shx, .dbf, .prj, etc.) and click "Open".

Second, specify if KBAs and/or PAs are to be assessed. For PAs to be included, the PAs must first be evaluated under the **Evaluate PAs (optional)** step. 

**Assess representation using:**
- Only KBAs - select this option if only KBAs are to be assessed.
- Only PAs - select this option if only PAs are to be assessed. **See note above regarding PAs.**  
- Both KBAs and PAs - select this option if both KBAs and PAs are to be assessed. **See note above regarding PAs.** 

Click on the orange **Run representation analysis** button to launch the representation analysis. Depending on the number of KBAs/PAs and the resolution of the indicators, this step can take a while to finish. Once completed, a spatial layer called " " will be added to the KBA_analysis geopackage in the folder called "output". The attributes added to this spatial layer are listed and described below.

**Filter KBAs and/or PAs based on dissimilarity metrics (DMs)**


## Output
