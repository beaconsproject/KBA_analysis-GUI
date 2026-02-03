# BEACONs KBA Explorer
BEACONs KBA Explorer is an app that assist conservation planners with the design of ecological benchmarks. It explicitly incorporates
hydrologic connectivity for the integration of aquatic and terrestrial conservation planning. The app uses Benchmark Builder executable, a user‐friendly software application by the BEACONs team. Benchmark Builder constructs ecological benchmarks using a deterministic construction algorithm that aggregates catchments to a user‐defined size and intactness. While this software was developed for the design of ecological benchmarks, it can also be used to design conservation areas not intended to serve as benchmarks.

## **How to run BEACONs KBA Explorer**
Download and unzip the BEACONs KBA Explorer on your local machine.
## R requirements
- Install R (R version 4.5.2) and RStudio RStudio 2025.09.1 Build 401) (https://posit.co/download/rstudio-desktop/)
- Intall the required packages:
install.packages(c("leaflet","shiny","sf","shinydashboard","shinyFiles","shinyWidgets","shinyjs","purrr","markdown","tibble","DT","dplyr","terra","exactextractr","tidyr","stringr","ggplot2"))

## Benchmark BUILDER requirements
- Acquire Benchmark BUILDER from the BEACONs team. Beanchmark Builder works on any Windows system. The BenchmarkBuilder_cmd.exe is built with the Microsoft .NET Framework. Microsoft .NET Framework is often installed with Windows, but if needed, you can  download .NET Framework [here](https://dotnet.microsoft.com/en-us/download/dotnet-framework).
- Make sure your Region settings on your local machine are set as follow:
   - Decimal symbol uses .
   - Digit grouping symbol uses ,
   - List separator uses ,
- Before running the application, copy BenchmarkBuilder.exe into the output directory. You will be prompted to select this directory when setting the parameters.

## Run the application
- Open RStudio
- Set the working directory where the root folder BEACONs KBA Explorer was unzipped.
- Open the server.R
- Click the Run App button found in the upper right corner of the Source Pane
- 
**Sample csv file for uploading spatial data**  
A sample csv file for uploading spatial data into the App (accessPath.csv) can be found in the folder called "www".
