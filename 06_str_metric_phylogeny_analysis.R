## 06_str_metric_phylogeny_analysis
## cm 06/05/2021

metrics <- read.csv("Data_range_metrics.csv") ## metrics provided by Anna Csergo in spring 2019
metrics <- metrics[metrics$Model == "Occurrence",]
metrics <- unique(metrics[, which(names(metrics) %in% 
                                    c("Species", "total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape",
                                      "perimeter.area.frac.dim"))])
metrics <- metrics[c("Species", "total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim")]
names(metrics) <- c("species", "total.area", "range.size", "effective.mesh.size", "mean.shape.index", "prop.landscape", "perimeter.area.frac.dim")

m <- drop_na(metrics)
m$perimeter.area.frac.dim <- m$perimeter.area.frac.dim + sqrt(min(m$perimeter.area.frac.dim)^2)
for (i in names(Filter(is.numeric, m[, which(names(m) %nin% c("mean.shape.index", "prop.landscape"))]))) {
  m[, i] <- c(log(m[,i]))
}
for (i in names(Filter(is.numeric, m))) {
  m[, i] <- c(scale(m[,i]))
}


metrics$species <- gsub("ssp.*", "", metrics$species)

## read in and handle data------------------------------------------------------------------------------------------------
mcmc_data <- m
mcmc_data$animal <- mcmc_data$species
## create comparative dataset
comp_data <- clean.data(mcmc_data, clean_tree, data.col = "animal")
