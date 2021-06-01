## get and explore phylogeny
## cm 04/2020


################ set up
#install.packages(c("ape", "phytools", "ggtree", "caper"))
library(caper)
library(ape)
library(phytools)
library(phangorn)
library(phylobase)
library(BiocManager)
BiocManager::install("treeio")
library("treeio")
BiocManager::install("ggtree")
library("ggtree")

## look at phylogenetic tree options and species listoverlap with my usable data

metrics <- read.csv("Data_range_metrics.csv") ## metrics provided by Anna Csergo in spring 2019
metrics <- metrics[metrics$Model == "Occurrence",]
metrics <- unique(metrics[, which(names(metrics) %in% 
                                    c("Species", "total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape",
                                      "perimeter.area.frac.dim"))])
metrics <- metrics[c("Species", "total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim")]
names(metrics) <- c("species", "total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim")

metrics$species <- gsub(" ssp.*", "", metrics$species)
metrics$species <- factor(metrics$species)
#metrics <- unique(merge(metrics, nb[,which(names(nb) %in% c('sp.list', "nb"))], by.x = "species", by.y = "sp.list"))
levels(metrics$species) <- gsub(" ", "_", levels(metrics$species))
levels(metrics$species) <- gsub("-", ".", levels(metrics$species))
#metrics <- metrics[metrics$perimeter.area.frac.dim != "Inf",]
metrics <- metrics[metrics$species != "Helleborus lividus",]

#m <- drop_na(metrics)
metrics$perimeter.area.frac.dim <- (metrics$perimeter.area.frac.dim + sqrt(min(metrics$perimeter.area.frac.dim, na.rm = T)^2)) + 1
for (i in names(Filter(is.numeric, metrics[, which(names(metrics) %nin% c("mean.shape.index", "prop.landscape"))]))) {
  metrics[, i] <- c(log(metrics[,i]))
}


## SMITH 2018 "open tree" tree

treefile <- read.tree("big_seed_trees_SMITH2018/ALLMB.tre")
if(exists("reefile")) {
  print("phylogeny read in")
} else warning("phylogeny does not exist in this directory")


tip_labels <- as.character(treefile$tip.label) ## 356305 species (well, tips..)

#### drop unused species from phylogeny 
omit_spe <- as.character(setdiff(treefile$tip.label, unique(metrics$species)))
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





## trying another tree to see if that increases overlap
# ## ZANNA 2013
# ## read in file
zannne_treefile <- read.tree(file ="Vascular_Plants_rooted.dated.tre")
omit_spe <- as.character(setdiff(zannne_treefile$tip.label, unique(metrics$species)))
zanne_tree <- drop.tip(zannne_treefile, omit_spe)

## label nodes
zanne_tree$node.label <- c(1:length(zanne_tree$node.label))

## make tree ultrametric
zanne_tree <- nnls.tree(cophenetic(zanne_tree),zanne_tree,rooted=TRUE)
## "RSS: 0.574181455739743"

#### check trees similarity this should equal 1
tips<-zanne_tree$tip.label
cor(as.vector(cophenetic(zanne_tree)[tips,tips]),
    as.vector(cophenetic(zanne_tree)[tips,tips]))
### 1

zanne_tips <- as.character(zanne_tree$tip.label)
#
# #plot(zannne_treefile, cex = 0.3)
# ## look at which species are represented in the tree
 tip_labels <- as.character(zannne_treefile$tip.label) ## 31749 species
#
sp_zanne <- Reduce(intersect, list(noquote(tip_labels), unique(metrics$species))) ##



# merged_tree <- bind.tree(clean_tree, zanne_tree)


sp_zanne <- Reduce(intersect, list(clean_tips, zanne_tips)) ## 

