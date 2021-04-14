## get and explore phylogeny
## cm 04/2020


################ set up
#install.packages(c("ape", "phytools", "ggtree", "caper"))
library(caper)
library(ape)
library(phytools)
library(phangorn)

## look at phylogenetic tree options and species listoverlap with my usable PREDICTS data

## SMITH 2018 "open tree" tree

treefile <- read.tree("big_seed_trees_SMITH2018/ALLMB.tre")
if(exists("treefile")) {
  print("phylogeny read in")
} else warning("phylogeny does not exist in this directory")


tip_labels <- as.character(treefile$tip.label) ## 356305 species (well, tips..)

#### drop unused species from phylogeny 
omit_spe <- as.character(setdiff(treefile$tip.label, unique(mydata$species)))
clean_tree <- drop.tip(treefile, omit_spe)

## label nodes
clean_tree$node.label <- c(1:length(clean_tree$node.label))  

## make tree ultrametric
clean_tree <- nnls.tree(cophenetic(clean_tree),clean_tree,rooted=TRUE)
## "RSS: 0.574181455739743"

#### check trees similarity this should equal 1
tips<-clean_tree$tip.label
cor(as.vector(cophenetic(clean_tree)[tips,tips]),
    as.vector(cophenetic(clean_tree)[tips,tips]))
### 1

clean_tips <- as.character(clean_tree$tip.label)
## save
#write.tree(clean_tree, file = "Data_str_clean_tree.tre")

plotTree(clean_tree,type="fan",fsize=0.1,lwd=0.5, ftype="i", part = 0.93)

if(exists("clean_tree")) {
  print("clean_tree created")
} else warning("failed to create clean_tree")
#str(clean_tree)







