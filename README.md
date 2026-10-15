# STR_tr8

Code used in manuscript "Human footprint in addition to climate drives spatial patterns in European plants"

Analysis can be run from script 03_sp_analysis using: 

Data_ratios_dataframe.rds # modelling dataframe
Data_sp_clean_tree.tre # species' phylogeny 

Data needed to run this project from scratch:

## species range metrics from Csergo et al., (2020) https://doi.org/10.1111/jbi.70319
- GRSmetrics_AFE_pol_50km_pol_line_mcp.csv

## phylogeny from Smith et al., (2018) https://doi.org/10.1002/ajb2.1019
- big_seed_trees_SMITH2018/ALLMB.tre

## climate data from bioclim at a 30 second resolution https://www.worldclim.org/data/bioclim.html#google_vignette
- wc2/wc2.1_30s_bio_1.tif  (mean annual temperature (C*10))
- wc2/wc2.1_30s_bio_12.tif (mean annual precipatation (mm))
- wc2/wc2.1_30s_bio_15.tif  (mean annual precip coeff variation)
- wc2/wc2.1_30s_bio_4.tif (mean annual temp SD*100)

## human footprint data from SEDAC (Venter et al., 2018)  HTTPS://DOI.ORG/10.7927/H46T0JQ4
- Data_wildareas-v3-2009-human-footprint.tif

## Past climate change velocity data from Sandel et al., 2011 10.1126/science.1210173 
- Velocity.tif

## locations of species occurrence data 
- AFE_endemics.txt 
from Jalas, J., and J. Suominen. 1972–1996. Atlas Florae Europaeae. Vol. 1–11. 
     Committee for Mapping the Flora of Europe & Societas Biologica Fennica Vanamo.

## Atlas flora europea grid
- AFEcells/cgrs_grid.shp
https://tietopankki.luomus.fi/atlas-florae-europaeae/grids-mapping-atlas-florae-europaeae/new-grid-system-atlas-florae-europaeae/#newAFEgridsystem
https://www.eea.europa.eu/en/analysis/maps-and-charts/common-european-chorological-grid-reference-system-cgrs?activeTab=a7caf3b5-7254-4a24-8919-693d4115158b

used to create: Data_occurences_climate_values.rds


