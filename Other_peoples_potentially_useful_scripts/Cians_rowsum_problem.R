#looking for solution to Cian's row summing issue

CH <-read.csv(file.choose())


p23 <-subset(CH, CH$plant_id=="23")
attach(p23)

sum(p23$biomass_bsub)


CH$biomass_bsub[47] <-mapvalues(CH$biomass_bsub[47],CH$biomass_bsub[47],sum(CH$plant_id =="23"))
CH$biomass_b[47] <-mapvalues(CH$biomass_b[47],CH$biomass_b[47],sum(CH$plant_id =="23"))
CH$no_leaves[47] <-mapvalues(CH$no_leaves[47],CH$no_leaves[47],sum(CH$plant_id =="23"))
CH$leaf_width[47] <-mapvalues(CH$leaf_width[47],CH$leaf_width[47],sum(CH$plant_id =="23"))
CH$leaf_length[47] <-mapvalues(CH$leaf_length[47],CH$leaf_length[47],sum(CH$plant_id =="23"))
CH$no_fl_stems[47] <-mapvalues(CH$no_fl_stems[47],CH$no_fl_stems[47],sum(CH$plant_id =="23"))
CH$fl_stem_height[47] <-mapvalues(CH$fl_stem_height[47],CH$fl_stem_height[47],sum(CH$plant_id =="23"))
CH$inflor_length[47] <-mapvalues(CH$inflor_length[47],CH$inflor_length[47],sum(CH$plant_id =="23"))
CH$biomass_sc[47] <-mapvalues(CH$biomass_sc[47],CH$biomass_sc[47],sum(CH$plant_id =="23"))

#must run ALL summations before dropping irrelivant rows
#when dropping, must drop from bottom UP, or else row numbers will change and fuck everything

#CH <-CH[33:4]

#great success x