# HIV Contextual Factors in Malawi

This repository contains code for identifying contextual factors for HIV viraemia across Malawi.

## Repository structure

This code is structured as an **R package** to help with sharing and reproducibility. It is stored in a **Git repository** to track changes from multiple authors over time.

* [More about R packages](https://r-pkgs.org/)
* [More about Git and Github](https://docs.github.com/en/get-started/start-your-journey/about-github-and-git)

The code is structured like this:

* **Functions** that get used multiple times (for example, a function to prepare a gridded raster indicator for analysis, or a function to create a scatter plot) will be stored in an `R/` subfolder.
* **Scripts** execute the functions for particular purposes---for example, one script might run a principle component analysis on all the indicators that are currently available. These will be stored in the `inst/scripts` subfolder. Each script will have a descriptive comment at the top explaining what the script does and giving examples.
* **Settings** cover all the details that scripts need that might change over time, including file and folder paths, data source details, data visualization settings, and so on. These settings are all stored in the **Config file** at the top level of the repository, `./config.yaml`. Each Script uses the [`versioning` package](https://cran.r-project.org/package=versioning) to load in the user's Config file and pulls the latest Settings from that file.
