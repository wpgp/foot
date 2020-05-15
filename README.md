# foot: An R package for processing building footprints
WorldPop Research Group, University of Southampton

### Quick Start  

1. Clone the repository to your computer (easiest with GitHub Desktop)

2. Install the *foot* package using the code in `./pkg_build.R`

3. Run the `vignette("footsteps")` to explore basic usage.

### Installation
Instead of cloning the repo, the *foot* package can be installed directly from Github.
```
devtools::install_github("wgpg/foot", auth_token="tkn", build_vignettes=TRUE)
```
where, "tkn" is an authentication token. This is required because *foot* is still in a private repo. You can obtain a token from <https://github.com/settings/tokens> and generating a personal token with 'repo' level access. This will be a long character string.

Note that building and running the code may require additional packages: `stars`, `raster`, `sf`, `data.table`, `lwgeom`.

### Inputs
Building footprints:  

//worldpop.files.soton.ac.uk/worldpop/Projects/WP517763_GRID3/DataIn/raw/DigitizeAfrica_building_footprints/

Sample dataset:
`data("kampala"); kampala$buildings`

### Outputs
Rasters (or tables):  

1. Settlement (binary)
2. Building count
3. Total building area
4. Average building area
5. Standard deviation of building area
6. Coefficient of variation of building area
7. Average building perimeter length
8. Standard deviation of building perimeter length
9. Coefficient of variation of building perimeter length
10. Average nearest neighbour distance
11. Standard deviation of nearest neighbour distance
12. Nearest neighbour index
13. Entropy of structure orientation
14. Average compactness measure (Polsby-Popper)
15. Average roundness measure

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
A folder containing scripts. See the example script `./wd/code/example_script.R` for a template.

**./wd/in/**  
A folder containing input data. This folder is included in the .gitignore for the repository, so files you save here will not be uploaded to github or shared with collaborators on the repository.

**./wd/out/**  
A folder containing outputs from the scripts. This folder is included in the .gitignore for the repository, so files you save here will not be uploaded to github or shared with collaborators on the repository.

