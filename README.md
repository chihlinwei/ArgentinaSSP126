---
title: "ArgentinaSSP126: Data package for seafloor climage change"
author: "Chih-Lin Wei"
date: "2024-07-25"
output: 
  html_document: 
    smart: false
    keep_md: true
---

This data package comprises ensemble model averages of CMIP6 historical and ssp126 climate change projections for the seafloor within Argentina’s Exclusive Economic Zone (EEZ). Yearly means were calculated from five climate models: GFDL-ESM4, IPSL-CM6A-LR, MPI-ESM1-2-LR, CNRM-ESM2-1, and NorESM2-MM, as part of the Coupled Models Intercomparison Project Phase 6 (CMIP6). The ensemble averages were computed for three time periods: 1950 to 2000 (historical), 2041 to 2060 (ssp126), and 2081 to 2100 (ssp126).

### Installation

To install the package, open R and type:

``` r
install.packages("devtools")
```

Then, you can install ArgentinaSSP126:

``` r
devtools::install_github("chihlinwei/ArgentinaSSP126", dependencies = TRUE)
library(ArgentinaSSP126)
```

### License
GNU Affero General Public License v3.0

### Tutorials
* Wei, C.-L. (2024) [Display seafloor climage change data](https://github.com/chihlinwei/ArgentinaSSP126/blob/main/tutorials/tute1.md)
* Wei, C.-L. (2024) [Extract seafloor climage change data by polygon](https://github.com/chihlinwei/ArgentinaSSP126/blob/main/tutorials/tute2.md)
* Wei, C.-L. (2024) [Extract seafloor climage change data by coordinate](https://github.com/chihlinwei/ArgentinaSSP126/blob/main/tutorials/tute3.md)
