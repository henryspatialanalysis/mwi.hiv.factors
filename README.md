# HIV Contextual Factors in Malawi

This repository contains code for identifying contextual factors for HIV viraemia across Malawi.

## Repository structure

This code is structured as an **R package** to help with sharing and reproducibility. It is stored in a **Git repository** to track changes from multiple authors over time.

* [More about R packages](https://r-pkgs.org/)
* [More about Git and Github](https://docs.github.com/en/get-started/start-your-journey/about-github-and-git)

The code is structured like this:

* **Functions** define behavior gets run multiple times. For example, a function to prepare a gridded raster indicator for analysis, or a function to create a scatter plot. Functions are stored in an `R/` subfolder.
* **Scripts** execute sets of functions for particular purposes. For example, one script might run a principle component analysis on all the indicators that are currently available. Scripts are stored in the `inst/scripts` subfolder. Each script starts with a descriptive comment at the top explaining what the script does and giving examples.
* **Settings** cover all the details that scripts need that might change over time. For example, file and folder paths, data source details, and data visualization terms are all examples of changeable settings. Settings are all stored in the **config file** at the top level of the repository, `./config.yaml`. Each script uses the [`versioning` package](https://cran.r-project.org/package=versioning) to load in the user's config file and pulls the latest settings from that file.
