
  - [foot: An R package for processing building
    footprints](#foot-an-r-package-for-processing-building-footprints)
      - [Installation](#installation)
      - [Quick Start](#quick-start)
      - [Contributions](#contributions)
      - [Repository Structure](#repository-structure)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# foot: An R package for processing building footprints

*[WorldPop Research Group, University of
Southampton](https://www.worldpop.org/)*

The `foot` package is designed to provide a set of consistent and
flexible tools for processing 2D vector representations of buildings.
The functionality includes basic geometry and morphology measures,
distance and clustering metrics. These calculations are supported with
helper functions for spatial intersections and tiled reading/writing of
data. Two main wrapper functions provide standardised workflows for
process building polygons to summarise metrics within polygons or on a
gridded surface.

## Installation

The `foot` package can be installed directly from Github.

``` r
devtools::install_github("wpgp/foot", auth_token="tkn", build_vignettes=TRUE)
```

where, “tkn” is an authentication token. This is required because `foot`
is still in a private repo. You can obtain a token from
<https://github.com/settings/tokens> and generating a personal token
with ‘repo’ level access. This will be a long character string.

Alternatively, clone the repository and install using the code in `./pkg_build.R`

Note that building and running the code may require additional packages:
`stars`, `raster`, `sf`, `data.table`, `lwgeom`, `purrr`. et al.

## Quick Start

A sample dataset of building footprints is provided:

    # load the sample
    data("kampala")
    
    # 2D vector building polygons
    kampala$buildings

Read the vignette on basic usage `vignette(footsteps)`. For more
advanced uses and creating gridded data layers, see `vignette(bigfoot)`.

### Basic Usage

``` r
library(foot)
# load sample data
data("kampala")
buildings <- kampala$buildings
zones <- kampala$adminZones
grid <- kampala$mastergrid
```

The `foot` package provides tools to calculate and summarise building
morphology measures at multiple scales. These include building-level
measures.

``` r
# building-level metrics
buildings$built_area <- fs_area(buildings, 
                                unit="m^2")
  head(buildings)
#> Simple feature collection with 6 features and 2 fields
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: 32.60765 ymin: 0.341117 xmax: 32.61288 ymax: 0.345773
#> geographic CRS: WGS 84
#>   FID_1                       geometry      built_area
#> 1   130 POLYGON ((32.61282 0.341132...  22.00824 [m^2]
#> 2   132 POLYGON ((32.61229 0.341693... 220.39011 [m^2]
#> 3   133 POLYGON ((32.60817 0.342753...  38.95750 [m^2]
#> 4   135 POLYGON ((32.60808 0.343578... 386.74429 [m^2]
#> 5   137 POLYGON ((32.60786 0.344552... 349.57765 [m^2]
#> 6   138 POLYGON ((32.60765 0.345604... 164.00931 [m^2]
```

As well as area-level summaries.

``` r
# Area-level summary metrics
# index the buildings to zones
building_zone <- zonalIndex(buildings, 
                            zones, 
                            zoneField = "Id", 
                            returnObject = TRUE)

# summarise within small areal units
admin_area <- fs_area_mean(building_zone, 
                           index="Id", 
                           unit="m^2")
  head(admin_area)
#>    index fs_area_m^2_mean
#> 1:     1   402.5984 [m^2]
#> 2:     2   211.0534 [m^2]
#> 3:     3   525.0747 [m^2]
#> 4:     4   555.0931 [m^2]
#> 5:     5   568.7154 [m^2]
#> 6:     6  1021.9529 [m^2]
```

Or gridded summary outputs, with the options to include a circular focal
window.

``` r
# calculated along a raster within a focal window
gridded <- calculate_bigfoot(buildings, 
                             metrics="area_mean", 
                             focalRadius=200,
                             template=grid)

  raster::plot(raster::raster(gridded))
  plot(sf::st_geometry(buildings), add=TRUE)
```

<img src="man/figures/REAsDME-unnamed-chunk-4-1.png" width="100%" />

### Outputs

Rasters (or tables):

1.  Binary settlement indicators
2.  Counts of structures
3.  Building area (total, mean, median, min, max, standard dev., coeff.
    var.)
4.  Building perimeter (total, mean, median, min, max, standard dev.,
    coeff. var.)
5.  Nearest neighbour distance (mean, median, standard dev.)
6.  Nearest neighbour index
7.  Structure orientation angle (normalised entropy)
8.  Compactness (Polsby-Popper) (mean, median)
9.  Roundess (mean, median)

A full list of function names can be retrieved with `get_fs_metrics()`.

## Contributions

Contributions are welcome. Raise or respond to an issue, or create a new
branch to develop a feature/modification and submit a pull request.

## Repository Structure

The repository is structured as an R package with an additional folder
“wd” that is a working directory for storing scripts, input data, and
output data.

**./pkg\_build.R**  
A script to build the R package and install it on your machine.
Optionally, builds vignettes.

**./data/** Folder containing internal data files and sample building
footprints for the vignettes.

**./data-raw/** Folder containing the script to create the internal data
files.

**./doc/** A folder containing the markdown and scripts for the
vignettes.

**./man/**  
A folder containing function documentation created by Roxygen. **Do not
edit these files.** Instead, use Roxygen to document each function (see
`./R/footFun.R` for example) and build the documentation using
`devtools::document()` (see `./pkg_build.R` for example).

**./Meta/** A folder created as part of building vignettes.

**./R/**  
A folder containing functions for the R package. Each file contains a
function with Roxygen documentation for the function at the top of the
script.

**./wd/code/**  
A folder containing scripts. See the example script
`./wd/code/example_script.R` for a template. Note that the vignette
scripts and .html source have been copied here for easy access for users
who don’t want to install/build the package.

**./wd/in/**  
A folder containing input data. This folder is included in the
.gitignore for the repository, so files you save here will not be
uploaded to github or shared with collaborators on the repository.

**./wd/out/**  
A folder containing outputs from the scripts. This folder is included in
the .gitignore for the repository, so files you save here will not be
uploaded to github or shared with collaborators on the repository.
