# Add display elements (OPTIONAL)

Add up to **three vector layers** to the map as visual references, e.g., salmon spawning sites or critical mineral potential. These layers are **for display only**: they are not used in any analysis and are not saved to the project.

<br>

### Using the app
1. Under **Select source for extra layers**, choose **Shapefile** or **GeoPackage**.
2. Select up to three layers:
   - **Shapefile**: for each layer, browse to the shapefile and select all of its files (`.shp`, `.shx`, `.dbf`, `.prj`) before clicking **Open**.
   - **GeoPackage**: upload one GeoPackage, then pick up to three of its layers from the drop-down lists.
3. Click **Confirm**. The layers are drawn on the map and added to the layer control (top right), where they can be turned on or off.

<br>

### Notes and limitations
- **Supported geometries:** points, lines and polygons. Rasters are not supported.
- **Names:** the file name (Shapefile) or the layer name (GeoPackage, first 25 characters) is used as the display name. Short names are recommended.
- **Colors** are fixed by position (see the color box next to each layer) and cannot be changed:
  - Layer 1: orange
  - Layer 2: purple
  - Layer 3: dark teal
- **Projection:** any CRS is accepted. Layers are reprojected for display.
- Clicking **Confirm** again replaces the previously added display layers.
- Display layers are not kept when the app is reloaded or when a project is reopened.

<br>

### Output
None. Display layers are not written to the project folder or the GeoPackage.