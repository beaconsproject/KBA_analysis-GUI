## Dataset requirements

This page describes the spatial layers used by **KBA Explorer** and how to prepare them. Layers are loaded in **Set input parameters**, either individually or through a CSV file listing their paths.

### General requirements
- **Formats**: vector layers as **Shapefiles** (`.shp`, `.shx`, `.dbf` and `.prj` are all required); rasters as single-band **GeoTIFF** (`.tif`).
- **Coordinate reference system (CRS)**: all layers must use the **same projected CRS** with units in **metres** (e.g., NAD 1983 Albers), 
identified by an EPSG or ESRI code. Geographic (latitude/longitude) and custom projections are not supported.
- **Extent**: the rasters must fully cover the planning region and the reference area.
- **Names**: folder, project and file names must not contain spaces. File names are used as layer names in the app, so keep them short.
- **NoData**: raster cells outside the data area must be set to NoData.

### Summary
| Layer | Type | Required | Key attributes / values |
|---|---|---|---|
| Catchments | Polygon | Yes | See [Catchments](#catchments) |
| Streams | Line | Yes | `BASIN` |
| Planning region | Polygon | Yes | — |
| CMI | Raster, continuous | Yes | Climate Moisture Index |
| GPP | Raster, continuous | Yes | Gross Primary Productivity |
| LED | Raster, continuous | Yes | Lake-edge density |
| LCC | Raster, categorical | Yes | Land Cover of Canada 2020 classes |
| Reference area | Polygon | Before **Assess representation** | — |
| Protected areas | Polygon | Optional | `NAME` (optional) |
| Custom criterion | Raster, continuous | Optional (one only) | Any numeric values |

### Catchments
The BEACONs catchment dataset: approximate drainage areas for segments of the stream network, with attributes describing water flow, land 
and water area, intactness, watershed membership and stream length.

**Required attributes** (exact names; the layer is rejected if one is missing):

| Attribute | Description |
|---|---|
| `CATCHNUM` | Unique catchment identifier (integer) |
| `ORDER1`, `ORDER2`, `ORDER3`, `BASIN` | Flow direction and watershed attributes |
| `SKELUID` | Stream skeleton identifier |
| `STRAHLER` | Strahler stream order, used to select seeds |
| `Area_land`, `Area_water`, `Area_total` | Land, water and total area (**m²**) |
| `length_m` | Stream length (**m**) |
| `FDA_M` | Fundamental drainage area |
| `Isolated` | Flag for isolated catchments |

**User-selected attributes** (any name):
- **Intactness**: proportion of the catchment that is intact, from **0** (0%) to **1** (100%). Selected in **Set input parameters**.
- **Zone**: subregions that KBAs cannot cross (e.g., watersheds). KBAs are built entirely within one zone. Selected in **Run Builder**. 
An attribute named `ZONE` is selected automatically.

### Streams
Line layer of the stream network, used to calculate the Dendritic Connectivity Index (DCI).
- Required attribute: `BASIN`. Segments with `BASIN = -1` are ignored.

### Planning region
A single polygon delimiting the area where KBAs are built.

### Reference area
A polygon of the region the KBAs should represent, e.g., an ecoregion. It can be the same as the planning region or smaller, and must be covered by the rasters. It can be loaded in **Set input parameters**, **Create Builder input** or **Assess single KBAs**.

### Protected areas (optional)
Polygons of existing protected areas. Hydrology metrics are calculated for them automatically. An optional `NAME` attribute is shown in the PA table.

### Indicator rasters
| Raster | Description | Values |
|---|---|---|
| **CMI** | Climate Moisture Index | Continuous |
| **GPP** | Gross Primary Productivity | Continuous |
| **LED** | Lake-edge density | Continuous |
| **LCC** | Land Cover of Canada 2020 | Class codes 1, 2, 5, 6, 8, 10–19 |
| **Custom** | Any additional indicator, e.g., projected CMI | Continuous |

&#x1F4CC; **LCC:** classes **15 (Cropland)** and **17 (Urban)** are excluded from the analysis. Colours and labels follow the Land Cover of Canada 2020 legend; other classification schemes will not display correctly.

Rasters are clipped to the reference area during the analysis. Very high-resolution rasters increase processing time.

### Loading layers with a CSV file
Create a CSV file with two columns, `Layer` and `Path`. Use the layer names below **exactly**. The custom criterion can have any short name. Remove the lines of optional layers you don't use. Use full paths with forward slashes (`/`).

```
Layer,Path
catchments,C:/data/catchments.shp
stream,C:/data/streams.shp
planning region,C:/data/planning_region.shp
protected areas,C:/data/protected_areas.shp
reference area,C:/data/reference_area.shp
CMI,C:/data/cmi.tif
LCC,C:/data/lcc.tif
LED,C:/data/led.tif
GPP,C:/data/gpp.tif
projcmi,C:/data/projcmi.tif
```

[Download the CSV template](accessPath.csv)
