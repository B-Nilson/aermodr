# aermodr

<!-- badges: start -->
<!-- badges: end -->

The goal of aermodr is to provide an interface with the U.S. E.P.A. AERMOD air pollutant dispersion model.
aermodr is currently under development, and is only available for Windows at this time.

## AERMOD

**AERMOD** is a steady-state atmospheric dispersion model developed by the U.S. Environmental Protection Agency for regulatory and applied air quality assessments.
It simulates how pollutants emitted from sources (e.g., stacks, area sources) are transported and dispersed in the atmosphere under varying meteorological conditions.

AERMOD represents plume behavior differently depending on atmospheric stability:

- in stable conditions, concentrations follow a Gaussian distribution in both horizontal and vertical directions.
- in convective conditions, the vertical structure is more complex, using a bi-Gaussian distribution to capture strong turbulent mixing.

The model handles complex terrain in a continuous and physically consistent way,
allowing plumes to interact with terrain (e.g., impingement or flow-following),
without requiring users to classify terrain regimes.
It also accounts for key physical processes such as:

- plume rise,
- lofting of buoyant emissions,
- entrainment into and out of boundary layers, and
- enhanced lateral dispersion due to wind meander

AERMOD relies on two key preprocessors:

1) **AERMAP** processes terrain data (e.g., digital elevation models),
to assign elevations and hill heights to sources and receptors,
enabling AERMOD to account for terrain effects on plume transport and dispersion.
2) **AERMET** prepares meteorological inputs by combining surface observations, 
upper-air soundings, and site characteristics (e.g., surface roughness, albedo, Bowen ratio),
to estimate boundary layer parameters and generate the formatted meteorological files required by AERMOD.

## Installation

You can install the development version of aermodr like so:

``` r
install.packages("remotes")
remotes::install_github("B-Nilson/aermodr")
```

## Creating a new AERMOD project

You can setup an aermodr project and install AERMAP, AERMET, and AERMOD using `aermodr::create_project()`.
This leverages the [`usethis`](https://usethis.r-lib.org/) package to setup a new [R project](https://r4ds.had.co.nz/workflow-projects.html) and initialize a [git repository](https://docs.github.com/en/get-started/using-git/about-git).
It also activates [`renv`](https://rstudio.github.io/renv/articles/renv.html) for maintaining consitent R packages and their versions.
You can also call `aermodr::install_model()` directly if you want more control, however a seperate project is recommended for reproducibility.

You will be asked to okay an initial commit to a new git repository,
then asked separately for each model prior to downloading and unzipping them (unless you set `prompt = FALSE` in `aermodr::create_project()`).
<!-- You will then be asked again to okay each R package installed with `renv`. -->
If you are using **RStudio** or **Positron** the project should open in a new session automatically if you set `open = TRUE`.

``` r
library(aermodr)
create_project("./aermod_test", open = TRUE)
```

When the new project opens you will see a folder for each model,
as well as an `R/` folder (for project-specific R code),
and several project related files ({project_name}.Rproj for project settings, .git/ & .gitignore for version control, .vscode/ and air.toml for controlling the `Air` R code formatter).

## Running Preprocessors (AERMAP, AERMET)

To run AERMOD, you first need to run the AERMAP and AERMET preprocessors,
which requires collecting input data and making and input control file.

Input data for each model can be placed in the `inputs` directory within the project's model directories,
and outputs will be written to their respective `outputs` directory.

You can use `aermodr::make_aermap_inp()` to create the input file for AERMAP,
and `aermodr::run_aermap()` to run AERMAP

<!-- TODO: add examples of specifying source/receptor files -->
<!-- TODO: add examples of other receptor/source types -->
<!-- TODO: clarify datum arg in anchor -->

``` r
library(aermodr)

project_title = "AERMOD Test Run"

# Files to be created by make_aermap_inp()
aermap_inp_path <- "AERMAP/inputs/aermap.inp"
aermap_out_path <- "AERMAP/outputs/aermap.out"
aermap_log_path <- "AERMAP/outputs/aermap.log"
aermap_src_path <- "AERMAP/outputs/aermap.src"
aermap_rec_path <- "AERMAP/outputs/aermap.rec"

# Required inputs to gather
terrain_files <- "AERMAP/inputs/terrain.dem" # more than 1 can be provided
receptor_files <- NULL # optional files specifying receptors - ignored here
source_files <- NULL # optional files specifying sources - ignored here

# Define the grid starting point
anchor <- list(
    x_user = 0,
    y_user = 0,
    x_utm = 500000,
    y_utm = 5400000,
    zone = 10,
    datum = 0
)

# Define a few basic point receptors and sources
receptors <- data.frame(
    id = c("daycare", "school", "store"),
    type = "DISCCART",
    subtype = "XYPNTS",
    x = c(0, 100, 200), # relative to anchor$x_user
    y = c(0, 100, 200)  # relative to anchor$y_user
)
sources <- data.frame(
    id = c("woodstove", "industry"),
    type = "POINT",
    x = c(50, 150), # relative to anchor$x_user
    y = c(50, 150), # relative to anchor$y_user
    elev = c(5, 15)
)

# Create .inp file for controling the AERMAP run
aermap_inp_path |> 
    make_aermap_inp(
        anchor = anchor,
        sources = sources,
        receptors = receptors,
        source_files = source_files,
        receptor_files = receptor_files,
        terrain_data_files = terrain_files,
        output_src_file = aermap_src_path,
        output_rec_file = aermap_rec_path,
        title = project_title, 
        control_options = aermap_control_options(), # set optional "COntrol pathway" settings here
        output_options = aermap_output_options() # set optional "OUtput pathway" settings here
    )

# Run AERMAP
run_aermap(
    inp_name = aermap_inp_path,
    out_name = aermap_out_path,
    log_name = aermap_log_path
)
```

Similiarly, you can use `aermodr::make_aermet_inp()` and `aermodr::run_aermet()` for AERMET.

``` r
library(aermodr)

# TODO: insert example
make_aermet_inp()
run_aermet()
```

## Running AERMOD

Once the preprocessors have been run, 
you can use `aermodr::make_aermod_inp()` to create the input control file for AERMOD,
and `aermodr::run_aermod()` to run the model.

<!--If a previous AERMOD run was interrupted, you can TODO: WRITE THIS AND FUNCTION-->

``` r
library(aermodr)

# TODO: add example
make_aermod_inp()
run_aermod()
```

## Viewing Results

<!-- TODO -->
