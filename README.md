
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
> [DOI](https://doi.org/10.1016/j.jgg.2024.02.004)

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

## Quick start

After launching the app, a typical analysis workflow is:

1.  Choose the tree file type (`Newick`, `NEXUS`, `BEAST`, or `Phylip`)
    and upload the tree.
2.  Enter a regular expression to extract sampling dates from tip
    labels.
3.  Choose the corresponding date format.
4.  Optionally upload an external CSV file.
5.  Click `submit` and inspect the results in the different analysis
    panels.

## Input data

### Tree file

The app accepts phylogenetic trees in `Newick`, `NEXUS`, `BEAST`, and
`Phylip` formats.

### Sampling dates

Sampling dates can be provided in two ways:

- extracted from tip labels with a regular expression;
- imported from an external CSV file with at least `label` and `date`
  columns.

When a regular expression contains a capture group, the captured part
will be used as the sampling date. When an external CSV is provided, the
`label` column should match the tree tip labels.

### External data for phylogenetic signal analysis

The external CSV file can also contain additional variables for
phylogenetic signal analysis. In this case, the file should contain a
`label` column plus one or more measurement columns to be used as `x`
and `y` variables in the app.

## What you can explore

- **Tree structure exploration**: visualize the tree, adjust the layout,
  and inspect node numbering.
- **Sample Dates**: review extracted or imported sampling dates together
  with root-to-tip divergence.
- **Temporal signal**: perform root-to-tip regression, inspect outliers,
  and analyze selected subtrees.
- **Phylogenetic signal**: analyze user-supplied variables on the tree
  and assess their relationships with PIC or PGLS.

## Learn more

For a fuller walkthrough of the interface and example panels, see the
package vignette:

``` r
vignette("shinyTempSignal")
```

<br>
