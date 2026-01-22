# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

**ConApp** is a Shiny application for mapping plant species and computing preliminary IUCN conservation status assessments following Criterion B. It is dedicated to Central Africa and built as an R package that wraps the ConR package with a web interface.

The application allows users to:
- Import species occurrence data from multiple sources (SHP files, GBIF, Rainbio database, manual polygon drawing)
- Map species distributions on interactive maps
- Compute range parameters (EOO, AOO, number of locations)
- Evaluate IUCN Red List status using Criterion B
- Generate assessment reports

## Commands

### Running the Application

```r
# Development mode (loads package from source)
library(pkgload)
library(shiny)
pkgload::load_all()
options(shiny.autoload.r = FALSE)
shiny::shinyApp(ui = conr_ui(), server = conr_server())
```

Or use the exported function:
```r
library(ConApp)
launch()  # Launches on port 5791 by default
launch(lang = "fr")  # Launch in French
```

### Documentation

```r
# Build documentation from roxygen comments
devtools::document()

# View help for a function
?launch
?conr_ui
```

### Building the pkgdown Website

```r
# Build the pkgdown site (without running examples)
pkgdown::build_site(install = FALSE, examples = FALSE, new_process = FALSE)

# Preview the site locally
pkgdown::preview_site()
```

The site will be built in the `docs/` directory. The site is configured to be hosted at https://umr-amap.github.io/ConApp/

### Package Installation

The package depends on a remote version of ConR:
```r
# Install from GitHub
remotes::install_github("gdauby/ConApp", upgrade = FALSE)

# ConR dependency (required)
remotes::install_github("gdauby/ConR")
```

## Architecture

### Module-Based Shiny Application

The application follows a modular Shiny architecture. Each major workflow step is isolated as a Shiny module with UI and server components:

- **Data Import Modules**: Multiple sources for data input
  - `module-data-full.R`: Main data import orchestrator (CSV, Excel, GBIF, Rainbio)
  - `module-data-poly.R`: Import from SHP files and polygon drawing
  - `module-data-import-gbif.R`: GBIF integration
  - `module-data-import-rainbio.R`: Rainbio database queries
  - `module-data-import-polygon.R`: Manual polygon drawing on map

- **Data Processing Modules**:
  - `module-data-validation.R`: Data validation using validation rules
  - `module-data-filterout.R`: Filtering out invalid records
  - `module-data-variable.R`: Column mapping to required variables
  - `module-data-display.R`: Data preview and exploration

- **Analysis Modules**:
  - `module-mapping.R`: Interactive leaflet map for point selection and visualization
  - `module-criterion_b.R`: IUCN Criterion B analysis (EOO, AOO, locations computation)
  - `module-data-country.R`: Distribution of threatened trees in Gabon
  - `module-report.R`: Summary report generation

### Core Application Structure

- **`conr-ui.R`**: Main UI using `bslib::page_navbar()` with hidden navigation panels and a collapsible sidebar for navigation
- **`conr-server.R`**: Main server logic coordinating all modules via reactive values (`data_rv`) that hold:
  - `x`: Main occurrence dataset
  - `polygon`: User-drawn or imported polygons
  - `latlon`: Validated latitude/longitude data

### Data Flow

1. **Import**: User imports data via one of the data modules → data stored in module-specific reactive
2. **Validation**: Data passed through validation checks (coordinate validity, taxonomic name checks, etc.)
3. **Mapping**: Validated data flows to mapping module where users can visualize and select points
4. **Analysis**: Selected data analyzed using ConR package functions (EOO.computing, AOO.computing, locations.comp)
5. **Reporting**: Results compiled into downloadable reports (Rmarkdown templates in `inst/reports/`)

### Key Reactive Data Pipeline

```
data_full_server/data_poly_server
  → data_rv$x (main dataset)
  → mapping_server (filtered to STATUS_CONR == "IN")
  → criterion_b_server (computation of conservation parameters)
  → summary_report_server (final reports)
```

### External Data Sources

- **GBIF**: Uses `rgbif` package for occurrence data (`gbif.R`)
- **Rainbio Database**: PostgreSQL database accessed via `RPostgres` with predefined credentials for public access (`rainbio.R`)
  - Connection function: `conn_mydb_rb(pass = "Anyuser2022", user = "common")`
  - Contains Central African plant occurrence records
- **Shapefiles**: Protected areas and threat layers from PostgreSQL database (`sig.R`)

### Internationalization

- Supports English (default) and French via `datamods::i18n()`
- Translation files in `inst/i18n/fr.csv`
- Language switcher in navbar updates entire UI

### Validation System

Uses the `validate` package with custom rules defined in `validation.R`:
- Coordinate range checks
- Empty value detection
- Taxonomic name validation
- Uses `bdc` package for biodiversity data cleaning

### Theming

Custom Bootstrap theme defined in `bs_theme_conr.R` using `bslib`:
- Dark theme with custom color scheme
- Google font integration
- Custom CSS for sidebar navigation

## Important Implementation Details

- **Internal Variables**: The app uses internal column naming with `.__` prefix (e.g., `.__latitude`, `.__longitude`) to avoid conflicts. Always filter these out when displaying to users via `unselect_internal_vars()`.

- **ConR Integration**: Core conservation analysis functions from ConR package:
  - `EOO.computing()`: Extent of Occurrence (minimum convex polygon)
  - `AOO.computing()`: Area of Occupancy (grid-based)
  - `locations.comp()`: Number of threat-defined locations
  - `cat_criterion_b()`: IUCN category assignment

- **Leaflet Maps**: All maps use `base_map()` helper with three basemap options (OSM, Esri, OpenTopoMap) and custom zoom controls positioned top-right.

- **Data Filtering**: The `STATUS_CONR` column tracks record validity:
  - `"IN"`: Valid record included in analysis
  - `"OUT"`: Filtered out record

- **Report Templates**: R Markdown templates in `inst/reports/`:
  - `species_report.Rmd`: Single species assessment
  - `all_tax_report.Rmd`: Multi-species summary
  - `country_threat_report.Rmd`: Country-level threatened species

## Deployment

Deployment configuration is in `deploy/app.R` which loads all dependencies explicitly before launching the app. This is used for deployment to RStudio Connect or Shiny Server.
