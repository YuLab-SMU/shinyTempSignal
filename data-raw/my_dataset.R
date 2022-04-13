## code to prepare `my_dataset` dataset goes here

usethis::use_data(MCC_FluA_H3_tree, overwrite = TRUE)

library(ggtree)
beast_file<-system.file("examples/MCC_FluA_H3.tree",package="ggtree")

rst_file<-system.file("examples/rst",package="ggtree")

mlc_file<-system.file("examples/mlc", package="ggtree")

beast_tree<-read.beast(beast_file)
codeml_tree<-read.codeml(rst_file, mlc_file)

merged_tree<-merge_tree(beast_tree,codeml_tree)
tree<-fortify(merged_tree)
tree <- rescale_tree(merged_tree, "dN")
MCC_FluA_H3_tree <- tree@phylo