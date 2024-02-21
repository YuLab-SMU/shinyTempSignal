# shinyTempSignal 0.0.7

+ keep x and y variables selected by user when entering new node for analysis (2024-02-21, Wed, #37)
+ supports more parameters for tree visualization (2024-02-19, Mon)
+ allows visualizing regressions for multiple subtrees and allows downloading tree after removing outliers (2024-02-18, Sun, #35)
+ add package level manual (2024-02-06, Tue)
+ remove dependencies (Cairo, config and ggpubr) that are actually not in used (2024-01-29, Mon)
+ import `yulab.utils::str_extract()` instead of `stringr::str_extract()` (2024-01-28, Sun)
+ correlation analysis after controling for phylogenetic signal 
    - PGLS method (2024-02-18, Sun, #35)
    - PIC method (2024-01-24, Wed, #32)
    
# shinyTempSignal 0.0.6

+ add vignette (2024-01-20, Sat)
+ modify color scheme (2024-01-15, Mon, #30)
+ update output summary data frame (2024-01-13, Sat, #29)

# shinyTempSignal 0.0.5

+ simultaneous update node across different panels (2023-12-12, Tue, #28)
+ add a summary table to present intercept, slope and r-squared. 
+ allows visualizing regression of subtree with temporal or phylogenetic signals.

# shinyTempSignal 0.0.4

+ doing regression for subsetting tree by using `treeio::tree_subset()` (2022-12-12, Mon, #21)
+ showing the regression parameter with dataFrame format to help uses to find the abnormal tips of subsetting tree.

