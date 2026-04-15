# Next Station? The Impact of Subway Network Integration on Travel Behavior: Evidence from São Paulo’s Line 5 - Lilac

This repository contains the data, replication scripts, and results for the research project evaluating the socio-economic impacts of the São Paulo Metro Line 5-Lilac integration on employment and education variables.

**Status:** Working Paper

## Overview
This study investigates the changes in mobility patterns, job accessibility, and educational outcomes resulting from the expansion and integration of Line 5-Lilac. Using microdata from the São Paulo Metro Origin-Destination (OD) Survey (including the latest 2023 wave) and applying spatial econometrics and impact evaluation techniques, this repository documents data cleaning, spatial matching of traffic zones, and the estimation of heterogeneous effects.

## Computational Requirements
The data cleaning and estimation routines were developed in **R**. 
Main packages required for replication:
* **Data Manipulation:** `tidyverse`
* **Spatial Data:** `sf` (for reading shapefiles and `.gpkg` packages) and `ggplot2` (for data visualization)
* **Modeling:** `fixest` and `MatchIt`

## Repository Structure

Below is the project's file map, organized by purpose:

### 1. Data and Shapefiles
* `Bases de dados OD/`: Raw and processed microdata from the Origin-Destination Survey.
* `Shapefiles OD/`: Polygons of the OD survey traffic zones.
* `Shapefiles estações metro SP/` & `Shapefiles estações trem SP/`: Georeferenced rail network.
* `Shapefile centro exp SP/`: Expanded center boundaries for sample restriction analyses.
* `zmc_polygons.gpkg`: Consolidated Geopackage containing the traffic zones for spatial analysis.

### 2. Replication Scripts (Execution Order)
* `01_database_cleanup.R`: Primary script. Cleans the OD survey variables and prepares the analysis matrices.
* `02_matching.R`: Defines the treatment and control groups based on station proximity and matching of observable characteristics.
* `03_descriptive_stats.R`: Generates summary statistics and sample balance checks.
* `04_estimations.R`: Main estimation routines for the econometric models and heterogeneity analysis.

### 3. Results and Outputs
* `Imagens/`: Heatmaps, event study plots, and visual robustness checks.
* `lista_modelos_originais*.rds`: R objects containing the main regression results, CPTM models, and matched models for quick table compilation.
* `São Paulo Shapefiles`: Shapefiles for districts and neighbourhoods from the city of São Paulo.

## Replication Steps

To replicate the results of this study:
1. Clone this repository to your local machine.
2. Ensure that the shapefiles and OD databases are in the correct directories as mapped above.
3. Open your R environment (Positron or RStudio recommended) and set this directory as your Working Directory.
4. Run `01_database_cleanup.R` to load the clean datasets into memory.
5. Next, run `02_matching.R` to establish the treatment assignments.
6. Finally, execute `04_estimations.R` to reproduce the results.

## Citation and Contact

**Researcher:** Philipe Libretti Dias  
**Affiliation:** University of São Paulo (USP) - FEARP/USP
If you use parts of this code or data in your research, please cite this repository or the associated working paper.
