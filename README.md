# STR_tr8
Spatial patterning of endemic European plant species.

540 species ratios still to calculate

to run remotely on Mannon (<3)'s machine, the files I need are:

- "GRSmetrics_AFE_pol_50km_pol_line_mcp.csv"
- "Data_1km_EU_vel.rds"
- "Data_1km_EU_hf.rds"
- "Data_1km_EU_clim.rds"
- "clean_tips_653.rds"
- "AFEcells/cgrs_grid.shp"
- "Data_ratios_dataframe.rds"

the commands are:
- setwd("/Users/macbookpro/Library/CloudStorage/OneDrive-Personal/PhD/spatialpattern_climate_humanfootprint")
-  source("00_sp_functions.R")
-  

cd ~
touch .Renviron
open .Renviron

R_MAX_VSIZE=100Gb 