# foot: An R package for processing building footprints
WorldPop Research Group, University of Southampton

The `foot` package is designed to provide a set of consistent and flexible tools for processing 2D vector representations of buildings. The functionality includes basic geometry and morphology measures, distance and clustering metrics. These calculations are supported with helper functions for spatial intersections and tiled reading/writing of data. Two main wrapper functions provide standardised workflows for process building polygons to summarise metrics within polygons or on a gridded surface.

### Quick Start  

1. Clone the repository to your computer (easiest with GitHub Desktop)

2. Install the *foot* package using the code in `./pkg_build.R`

3. Run the `vignette("footsteps")` to explore basic usage.

4. Check out the vignette `bigfoot` for creating gridded summary layers.

### Installation
Instead of cloning the repo, the *foot* package can be installed directly from Github.
```
devtools::install_github("wgpg/foot", auth_token="tkn", build_vignettes=TRUE)
```
where, "tkn" is an authentication token. This is required because *foot* is still in a private repo. You can obtain a token from <https://github.com/settings/tokens> and generating a personal token with 'repo' level access. This will be a long character string.

Note that building and running the code may require additional packages: `stars`, `raster`, `sf`, `data.table`, `lwgeom`, `purrr`.

### Contributions
Contributions are welcome. Raise or respond to an issue, or create a new branch to develop a feature/modification and submit a pull request.

### Inputs
Building footprints (for example):  

//worldpop.files.soton.ac.uk/worldpop/Projects/WP517763_GRID3/DataIn/raw/DigitizeAfrica_building_footprints/

Sample dataset provided:
`data("kampala"); kampala$buildings`

### Outputs
Rasters (or tables):  

1. Binary settlement indicators
2. Counts of structures
3. Building area (total, mean, median, standard dev., coeff. var.)
4. Building perimeter (total, mean, median, standard dev., coeff. var.)
5. Nearest neighbour distance (mean, median, standard dev.)
6. Nearest neighbour index
7. Structure orientation angle (normalised entropy)
8. Compactness (Polsby-Popper) (mean, median)
9. Roundess (mean, median)

### Repository Structure
The repository is structured as an R package with an additional folder "wd" that is a working directory for storing scripts, input data, and output data.

**./pkg_build.R**  
A script to build the R package and install it on your machine. Optionally, builds vignettes.

**./data/**
Folder containing internal data files and sample building footprints for the vignettes.

**./data-raw/**
Folder containing the script to create the internal data files.

**./doc/** 
A folder containing the markdown and scripts for the vignettes.

**./man/**  
A folder containing function documentation created by Roxygen. **Do not edit these files.** Instead, use Roxygen to document each function (see `./R/footFun.R` for example) and build the documentation using `devtools::document()` (see `./pkg_build.R` for example).

**./Meta/**
A folder created as part of building vignettes.

**./R/**  
A folder containing functions for the R package. Each file contains a function with Roxygen documentation for the function at the top of the script. 

**./wd/code/**  
A folder containing scripts. See the example script `./wd/code/example_script.R` for a template. Note that the vignette scripts and .html source have been copied here for easy access for users who don't want to install/build the package.

**./wd/in/**  
A folder containing input data. This folder is included in the .gitignore for the repository, so files you save here will not be uploaded to github or shared with collaborators on the repository.

**./wd/out/**  
A folder containing outputs from the scripts. This folder is included in the .gitignore for the repository, so files you save here will not be uploaded to github or shared with collaborators on the repository.

