
<!-- To modify Package/Title/Description/Authors fields, edit the DESCRIPTION file -->

## `shinyTempSignal`: Explore Temporal and Other Phylogenetic Signals

Sequences sampled at different time points can be used to infer
molecular phylogenies on natural time scales, but if the sequences
records inaccurate sampling times, that are not the actual sampling
times, then it will affect the molecular phylogenetic analysis. This
shiny application helps exploring temporal characteristics of the
evolutionary trees through linear regression analysis and with the
ability to identify and remove incorrect labels. The method was extended
to support exploring other phylogenetic signals under strict and relaxed
models.

If you use `shinyTempSignal`, please cite:

<!-- Modify this by editing the file: inst/CITATION  -->

> L Zhan, X Luo, W Xie, XA Zhu, Z Xie, J Lin, L Li, W Tang, R Wang, L
> Deng, Y Liao, B Liu, Y Cai, Q Wang, S Xu, G Yu. shinyTempSignal: an R
> shiny application for exploring temporal and other phylogenetic
> signals. Journal of Genetics and Genomics 2024. doi:
> 10.1016/j.jgg.2024.02.004

## :writing_hand: Authors

YuLab@SMU <https://yulab-smu.top>

## Installation

``` r
# to install cran version
install.packages("shinyTempSignal")

# to install github version
if(!require("remotes")) install.packages("remotes")

remotes::install_github("YuLab-SMU/shinyTempSignal")
```

## Usages

``` r
library(shinyTempSignal)
run_shinyTempSignal()
```

<br>
