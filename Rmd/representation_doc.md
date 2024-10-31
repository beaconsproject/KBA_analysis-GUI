

## Assess representation
The evaluation of representation is calculated using statistic that compares the distribution of the ecological indicator (cmi, gpp, led and lcc) within each KBA  
to the distribution of that same indicator within the planning region. It measures how similar the KBA is to the ecological unit within which 
it is embedded, in terms of the indicator. The idea being that the KBA should provide a sufficient area for the ecological processes associated with specific target classes to operate. 
Values of the KS statistic range from 0 to 1, with lower values indicating increasing representation. 
A value of 0.2 is often used as a threshold; values less than 0.2 indicate adequate representation and values above 0.2 indicate poor representation. 


### Using the app
    
- **Set grid cell size** : Specify grid cell size and assign KBAs to groups (attribute = group_id). Grid cell size is entered in metres e.g., 10kmx10km grid is 10000. 
- **Calculate hydrology metrics** : Calculate longitudinal hydrological connectivity within each KBAs area with values ranging from 0 (low connectivity) to 1 (fully connected).
It also add area (area_km2), intactness (AWI) from the *Unique_BAs_attributes.csv* and upstream area (up_km2) and upstream intactness (up_AWI) from the *HYDROLOGY_METRICS.csv*.
- **Reduce the number of KBAs** : Select the top KBA from each group based on smallest upstream area, largest DCI, and largest upstream intactness. 

The **Run representation analysis** initiates the comparison of the distribution of the 4 ecological criteria within each KBa to its distribution for the planning region using Dissimiarity metrics (DMs).
DMs range from 0 (low dissimilarity) to 1 (high dissimilarity). For continuous variables, the DM is Bray-Curtis. For categorical variables, the DM is Kolmogorov-Smirnov.



### Output

Dissimilarity metrics for each of the four indicators, DCI and hydrology metrics at the KBA level are saved in the layer **KBAs_att**. 

Density plots showing the distribution of continuous indicator within the KBAs (red) and within the planning region (blue) are saved in the output folder under plot. 
Barplots showing the proportions of each indicator class of a categorical indicator (i.e., land cover types) within a KBA (bars) and within the planning region (black dots). 
The Bray‐Curtis (BC) statistic describes the dissimilarity between the bars and dots, and ranges in value from 0 to 1, where 0 indicates perfect proportional representation for each KBA.
Bars that fall below the black dots indicate that proportional representation of that class was not achieved.

