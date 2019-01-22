#Thesis_script1

#***************************************************
#General notes and skill accumulation
#Ctrl L clears script from console

#to install R packages
#>install.packages("packagename")
#you can create a dataset with normal distribution and specify the mean (m), standard deviation (sd)
#and sample size (n) as below
#>nameoffakedataset<-rnorm(n=1000, m=x, sd=y)
#gives a histogram of explainitory and response variable, provided their columns are read as numeric
#>hist(datasetname)
# display last 25 commands
#>history()
# display all previous commands
#>history(max.show=Inf) 

#R reads \ as an escape character. use / or \\ instead
#> c:/mydocuments/myfile.txt
# print the current working directory - cwd 
getwd() 
# list the objects in the current workspace
ls()    

#internet says sets working dircetory to something else
#except R says I can't change working directory 
#?
setwd("globalenv")

#so working directory is where R goes to find files 
#to read in when you ask it to open someting

setwd("C:/Users/Caroline/Dropbox/Thesis/") 

install.packages("TR8",dependencies = TRUE)
install.packages(stringr)
install.packages(reshape)

#what does library do?
#loadds and attaches add-on packages

library(stringr)
#rearrange trait dataframes
library(reshape)
library(TR8)

#read in dataset to R (now named seedbank)
sb<-read.csv(file.choose())
afe<-read.csv(file.choose())

attach(sb)
attach(afe)


levels(seedbank)
str(sb)
seedbank

head(sb)
#shows internal dataset structure 
str(sb)
print(sb)
#look for NA entries in dataset
idx <- is.na(sb)
head(idx)

#tells R to read new_sb as a subset of sb dataset, made of only rows containing NA
new_sb <-subset(sb,sb$seedbank=="NA")
head(new_sb)
#result was no NA datapoints

duplicated(Species, incomparables = FALSE)



#having issues with organisation of data
idx.seedbank <- grepl(" ", seedbank)
head(idx.seedbank)

#gsub will replace all the matches found by grepl
#sub will replace only the first one

#trying to remove spaces from seedbank columm in origanal dataset 
sb2 <- as.factor(gsub(" ","_", sb[,1]))

sb$seedbank <- as.(gsub(",","", mydata[,5]))



#***************************
#trying to re-organise the data from sb

SB <- data.frame(SPECIES=Species, Transient=seedbank == "transient")

attach(SB)
head(SB)
print(SB)

str(sb)

list(levels(seedbank))

head(sb)

SB <- data.frame(SPECIES=Species, T=seedbank == "transient", LTP=seedbank == "long-term persistent", P=seedbank == "present", STP=seedbank == "short-term persistent")
head(SB)


#*********************************************
#having a look at the afe data
#*********************************************
attach(afe)
str(afe)

duplicated(Species.x, incomparables = FALSE)

levels(Species.x)
list(Species.x)
print(unique(Species.x))
#gives only unique leves, but in quotations. 
levels(unique(Species.x))
#need to use c("name") to bind the species
s_list <- c("Abies_alba", "Abies_borisii-regis", "Abies_cephalonica", "Aquilegia_vulgaris_ssp_dichroa")

#Anna's shortcut for creating species list vector that I can use in tr8 function
spl <-as.vector(Species.x, mode = "list")

splist<- as.vector(unique(Species.x))

#gives only first 3 letters in each row
substr(splist, 1, 3)

firstry <-splist[1:3]
firstry1 <-gsub("_", " ", firstry)

threesp <- tr8(firstry1, download_list = NULL, gui_config = TRUE, synonyms=TRUE)
#Error in names(reference_names) <- c("original_names", "synonyms") : 
#  'names' attribute [2] must be the same length as the vector [1]

threespDF <-extract_traits(threesp)
threespDF$original_names <-gsub(" ", "_", threespDF$original_names)

firstmerge<- merge(afe, threespDF, by.x="Species.x", by.y="original_names",all.x=TRUE)



tr8("Abies_alba", download_list = NULL, gui_config = TRUE, synonyms=TRUE)

#running command without synonyms, because of #Error in names(reference_names) <- c("original_names", "synonyms") :  'names' attribute [2] must be the same length as the vector [1]
threesp <- tr8(firstry, download_list = NULL, gui_config = TRUE, synonyms=FALSE)

#tried to let R read the vector as a longer vector
#did not work at all
attr(firstry, "dim") <- c(1,6)



#WHOOP! Finally got the species list!!
cat(paste(shQuote(Species.x, type="cmd"), collapse=", "))
#above gave all values, so need to exclude duplicate species lists
cat(paste(shQuote(unique(Species.x, type="cmd")), collapse=", "))

#*********************************
#not needed, can simple set unique species as a vector
#spl1 <-c("Abies_alba", "Abies_borisii-regis", "Abies_cephalonica", "Aconitum_degenii", "Aconitum_lycoctonum_ssp_lasiostomum", "Aconitum_lycoctonum_ssp_vulparia", "Aconitum_napellus", "Aconitum_napellus_ssp_firmum", "Aconitum_napellus_ssp_fissurae", "Aconitum_napellus_ssp_hians", "Aconitum_napellus_ssp_lusitanicum", "Aconitum_napellus_ssp_napellus", "Aconitum_napellus_ssp_tauricum", "Aconitum_napellus_ssp_vulgare", "Aconitum_toxicum", "Aconitum_valesiacum", "Aconitum_variegatum_ssp_variegatum", "Adonis_pyrenaica", "Alnus_cordata", "Alnus_incana_ssp_kolaensis", "Alnus_viridis_ssp_viridis", "Alyssoides_cretica", "Alyssum_alpestre", "Alyssum_bertolonii_ssp_bertolonii", "Alyssum_bertolonii_ssp_scutarinum", "Alyssum_corymbosoides", "Alyssum_cuneifolium", "Alyssum_diffusum", "Alyssum_heldreichii", "Alyssum_lapeyrousianum", "Alyssum_loiseleurii", "Alyssum_macrocarpum", "Alyssum_markgrafii", "Alyssum_ovirense", "Alyssum_pulvinare", "Alyssum_scardicum", "Alyssum_stribrnyi", "Alyssum_wierzbickii", "Anemone_apennina", "Anemone_hortensis", "Anemone_hortensis_ssp_heldreichii", "Anemone_hortensis_ssp_hortensis", "Anemone_pavoniana", "Anemone_trifolia_ssp_albida", "Anemone_trifolia_ssp_trifolia", "Aquilegia_alpina", "Aquilegia_bertolonii", "Aquilegia_einseleana", "Aquilegia_pyrenaica_ssp_pyrenaica", "Aquilegia_transsilvanica", "Aquilegia_vulgaris_ssp_atrata", "Aquilegia_vulgaris_ssp_dichroa", "Aquilegia_vulgaris_ssp_nigricans", "Arabis_bryoides", "Arabis_caerulea", "Arabis_ciliata", "Arabis_collina", "Arabis_cretica", "Arabis_juressi", "Arabis_nova_ssp_iberica", "Arabis_nova_ssp_nova", "Arabis_procurrens", "Arabis_pumila", "Arabis_scabra", "Arabis_scopoliana", "Arabis_serpillifolia", "Arabis_soyeri_ssp_soyeri", "Arabis_soyeri_ssp_subcoriacea", "Arabis_vochinensis", "Aremonia_agrimonoides_ssp_pouzarii", "Arenaria_aggregata_ssp_racemosa", "Arenaria_algarbiensis", "Arenaria_balearica", "Arenaria_bertolonii", "Arenaria_conferta_ssp_conferta", "Arenaria_conimbricensis", "Arenaria_conimbricensis_ssp_var_loscosii", "Arenaria_controversa", "Arenaria_cretica", "Arenaria_erinacea", "Arenaria_gracilis", "Arenaria_marschlinsii", "Arenaria_montana_ssp_intricata", "Arenaria_multicaulis", "Arenaria_norvegica_ssp_norvegica", "Arenaria_obtusiflora_ssp_ciliaris", "Arenaria_obtusiflora_ssp_obtusiflora", "Arenaria_purpurascens", "Arenaria_querioides_ssp_querioides", "Arenaria_retusa", "Arenaria_rigida", "Arenaria_tetraquetra_ssp_amabilis", "Arenaria_valentina", "Aristolochia_cretica", "Aristolochia_microstoma", "Armoracia_macrocarpa", "Asplenium_adulterinum", "Asplenium_billotii", "Asplenium_fissum", "Asplenium_forisiense", "Asplenium_marinum", "Asplenium_obovatum", "Asplenium_petrarchae_ssp_petrarchae", "Asplenium_seelosii_ssp_seelosii", "Atriplex_calotheca", "Atriplex_recurva", "Aubrieta_columnae_ssp_croatica", "Aubrieta_gracilis_ssp_scardica", "Aurinia_corymbosa", "Aurinia_petraea", "Aurinia_sinuata", "Barbarea_longirostris", "Barbarea_rupicola", "Barbarea_sicula", "Berteroa_orbiculata", "Beta_nana", "Biscutella_auriculata", "Biscutella_cichoriifolia", "Biscutella_controversa", "Biscutella_coronopifolia", "Biscutella_flexuosa", "Biscutella_gredensis", "Biscutella_guillonii", "Biscutella_intermedia", "Biscutella_lusitanica", "Biscutella_mediterranea", "Biscutella_variegata", "Bolanthus_graecus", "Brassica_cretica_ssp_cretica", "Brassica_incana", "Brassica_insularis", "Brassica_montana", "Brassica_oleracea_ssp_oleracea", "Brassica_repanda_ssp_blancoana", "Brassica_repanda_ssp_cantabrica", "Brassica_repanda_ssp_repanda", "Brassica_rupestris", "Bufonia_macropetala", "Bufonia_perennis", "Bufonia_tuberculata", "Bufonia_willkommiana", "Callianthemum_coriandrifolium", "Camphorosma_annua", "Capsella_grandiflora", "Cardamine_amara_ssp_opicii", "Cardamine_asarifolia", "Cardamine_carnosa", "Cardamine_chelidonia", "Cardamine_crassifolia", "Cardamine_enneaphyllos", "Cardamine_glanduligera", "Cardamine_kitaibelii", "Cardamine_maritima", "Cardamine_monteluccii", "Cardamine_pentaphyllos", "Cardamine_plumieri", "Cardamine_pratensis_ssp_rivularis", "Cardamine_raphanifolia_ssp_barbareoides", "Cardamine_raphanifolia_ssp_raphanifolia", "Cardamine_trifolia", "Cardamine_waldsteinii", "Cardaminopsis_arenosa_ssp_borbasii", "Cardaminopsis_neglecta", "Cerastium_arvense_ssp_suffruticosum", "Cerastium_biebersteinii", "Cerastium_brachypetalum_ssp_pindigenum", "Cerastium_brachypetalum_ssp_tenoreanum", "Cerastium_candidissimum", "Cerastium_carinthiacum", "Cerastium_carinthiacum_ssp_austroalpinum", "Cerastium_carinthiacum_ssp_carinthiacum", "Cerastium_decalvans", "Cerastium_fontanum_ssp_macrocarpum", "Cerastium_grandiflorum", "Cerastium_illyricum_ssp_comatum", "Cerastium_illyricum_ssp_illyricum", "Cerastium_latifolium", "Cerastium_ligusticum_ssp_ligusticum", "Cerastium_ligusticum_ssp_trichogynum", "Cerastium_moesiacum", "Cerastium_pedunculatum", "Cerastium_rectum_ssp_petricola", "Cerastium_rectum_ssp_rectum", "Cerastium_scaranii", "Cerastium_sylvaticum", "Cerastium_tomentosum", "Cerastium_uniflorum")
#spl4 <-c("Ceratocapnos_claviculata_ssp_claviculata", "Chenopodium_bonus-henricus", "Chrysosplenium_alpinum", "Chrysosplenium_oppositifolium", "Clematis_alpina_ssp_alpina", "Clematis_viticella_ssp_campaniflora", "Cochlearia_aestuaria", "Cochlearia_anglica", "Cochlearia_danica", "Cochlearia_glastifolia", "Cochlearia_pyrenaica_ssp_pyrenaica", "Cochlearia_scotica", "Coincya_longirostra", "Coincya_monensis_ssp_monensis", "Coincya_monensis_ssp_orophila", "Coincya_monensis_ssp_puberula", "Coincya_transtagana", "Consolida_uechtritziana", "Corydalis_intermedia", "Corydalis_paczoskii", "Corydalis_parnassica", "Corydalis_pumila", "Corydalis_solida_ssp_incisa", "Corydalis_uniflora", "Crambe_aspera", "Delphinium_cuneatum", "Delphinium_dubium", "Delphinium_elatum", "Delphinium_elatum_ssp_helveticum", "Delphinium_pictum_ssp_pictum", "Dianthus_arenarius_ssp_borussicus", "Dianthus_arenarius_ssp_pseudosquarrosus", "Dianthus_balbisii_ssp_liburnicus", "Dianthus_barbatus_ssp_compactus", "Dianthus_bessarabicus", "Dianthus_biflorus", "Dianthus_borbasii", "Dianthus_borbasii_ssp_borbasii", "Dianthus_campestris_ssp_laevigatus", "Dianthus_carthusianorum", "Dianthus_carthusianorum_ssp_atrorubens", "Dianthus_carthusianorum_ssp_latifolius", "Dianthus_carthusianorum_ssp_sanguineus", "Dianthus_carthusianorum_ssp_tenuifolius", "Dianthus_ciliatus_ssp_ciliatus", "Dianthus_ciliatus_ssp_dalmaticus", "Dianthus_collinus_ssp_collinus", "Dianthus_collinus_ssp_glabriusculus", "Dianthus_corymbosus", "Dianthus_crassipes", "Dianthus_cruentus_ssp_cruentus", "Dianthus_eugeniae", "Dianthus_fischeri_ssp_pratensis", "Dianthus_furcatus_ssp_furcatus", "Dianthus_furcatus_ssp_geminiflorus", "Dianthus_gallicus", "Dianthus_giganteiformis_ssp_giganteiformis", "Dianthus_giganteiformis_ssp_kladovanus", "Dianthus_giganteiformis_ssp_pontederae", "Dianthus_giganteus_ssp_banaticus", "Dianthus_giganteus_ssp_croaticus", "Dianthus_giganteus_ssp_subgiganteus", "Dianthus_glacialis_ssp_gelidus", "Dianthus_glacialis_ssp_glacialis", "Dianthus_gracilis_ssp_armerioides", "Dianthus_gracilis_ssp_gracilis", "Dianthus_graniticus", "Dianthus_gratianopolitanus", "Dianthus_guttatus_ssp_guttatus", "Dianthus_haematocalyx_ssp_haematocalyx", "Dianthus_henteri", "Dianthus_hispanicus", "Dianthus_hispanicus_ssp_tarraconensis", "Dianthus_humilis", "Dianthus_integer_ssp_integer", "Dianthus_integer_ssp_minutiflorus", "Dianthus_lanceolatus", "Dianthus_langeanus", "Dianthus_laricifolius_ssp_laricifolius", "Dianthus_marschallii", "Dianthus_membranaceus", "Dianthus_microlepis", "Dianthus_moesiacus", "Dianthus_monspessulanus", "Dianthus_nardiformis", "Dianthus_nitidus", "Dianthus_pavonius", "Dianthus_pelviformis", "Dianthus_petraeus_ssp_orbelicus", "Dianthus_petraeus_ssp_petraeus", "Dianthus_pinifolius_ssp_lilacinus", "Dianthus_pinifolius_ssp_serbicus", "Dianthus_plumarius", "Dianthus_pratensis", "Dianthus_pseudoversicolor", "Dianthus_pyrenaicus_ssp_pyrenaicus", "Dianthus_scaber_ssp_scaber", "Dianthus_scaber_ssp_toletanus", "Dianthus_seguieri_ssp_glaber", "Dianthus_seguieri_ssp_seguieri", "Dianthus_serenaeus", "Dianthus_serotinus", "Dianthus_spiculifolius", "Dianthus_stenopetalus", "Dianthus_sternbergii_ssp_sternbergii", "Dianthus_superbus_ssp_alpestris", "Dianthus_sylvestris_ssp_bertisceus", "Dianthus_sylvestris_ssp_nodosus", "Dianthus_sylvestris_ssp_tergestinus", "Dianthus_trifasciculatus", "Dianthus_trifasciculatus_ssp_pseudobarbatus", "Dianthus_tristis", "Diplotaxis_cretacea", "Diplotaxis_ibicensis", "Draba_aizoides_ssp_aizoides", "Draba_aspera", "Draba_dedeana", "Draba_dubia", "Draba_hoppeana", "Draba_lacaitae", "Draba_muralis", "Draba_parnassica", "Draba_scardica", "Draba_tomentosa", "Drymocallis_regis-borisii", "Drypis_spinosa_ssp_jacquiniana", "Drypis_spinosa_ssp_spinosa", "Ephedra_distachya_ssp_helvetica", "Erucastrum_gallicum", "Erucastrum_virgatum_ssp_baeticum", "Erysimum_candicum_ssp_candicum", "Erysimum_carniolicum", "Erysimum_comatum", "Erysimum_corinthium", "Erysimum_crassistylum", "Erysimum_crepidifolium", "Erysimum_drenowskii", "Erysimum_duriaei_ssp_duriaei", "Erysimum_duriaei_ssp_gorbeanum", "Erysimum_duriaei_ssp_pyrenaicum", "Erysimum_graecum", "Erysimum_humile", "Erysimum_jugicola", "Erysimum_kuemmerlei", "Erysimum_linariifolium", "Erysimum_linifolium", "Erysimum_metlesicsii", "Erysimum_montosicola", "Erysimum_nevadense_ssp_gomezcampoi", "Erysimum_nevadense_ssp_mediohispanicum", "Erysimum_odoratum", "Erysimum_pseudorhaeticum", "Erysimum_pusillum_ssp_cephalonicum", "Erysimum_pusillum_ssp_hayekii", "Erysimum_pusillum_ssp_microstylum", "Erysimum_pusillum_ssp_pusillum", "Erysimum_pusillum_ssp_rechingeri", "Erysimum_rhaeticum", "Erysimum_sylvestre_ssp_sylvestre", "Erysimum_transsilvanicum", "Erysimum_virgatum", "Erysimum_witmannii", "Fagus_sylvatica", "Fibigia_lunarioides", "Fumaria_martinii", "Fumaria_petteri_ssp_petteri", "Fumaria_purpurea", "Fumaria_ragusina_ssp_wirtgenii", "Geum_hispidum", "Geum_molle", "Geum_montanum", "Geum_pyrenaicum", "Geum_reptans", "Guiraoa_arvensis", "Gypsophila_fastigiata", "Gypsophila_nana")
#spl5 <-c("Gypsophila_petraea", "Gypsophila_repens", "Gypsophila_spergulifolia", "Gypsophila_struthium_ssp_hispanica", "Gypsophila_struthium_ssp_struthium", "Gypsophila_tomentosa", "Helleborus_bocconei_ssp_bocconei", "Helleborus_bocconei_ssp_intermedius", "Helleborus_cyclophyllus", "Helleborus_dumetorum_ssp_atrorubens", "Helleborus_dumetorum_ssp_dumetorum", "Helleborus_foetidus", "Helleborus_lividus_ssp_corsicus", "Helleborus_multifidus_ssp_istriacus", "Helleborus_multifidus_ssp_multifidus", "Helleborus_multifidus_ssp_serbicus", "Helleborus_niger_ssp_niger", "Helleborus_odorus_ssp_laxus", "Helleborus_odorus_ssp_odorus", "Helleborus_purpurascens", "Helleborus_viridis", "Hepatica_transsilvanica", "Herniaria_alpina", "Herniaria_ciliolata_ssp_robusta", "Herniaria_fruticosa", "Herniaria_latifolia_ssp_latifolia", "Herniaria_lusitanica_ssp_lusitanica", "Herniaria_parnassica_ssp_parnassica", "Herniaria_scabrida_ssp_scabrida", "Hesperis_dinarica", "Hesperis_laciniata_ssp_secundiflora", "Hesperis_sylvestris", "Hesperis_theophrasti", "Hornungia_aragonensis", "Hugueninia_tanacetifolia_ssp_suffruticosa", "Hugueninia_tanacetifolia_ssp_tanacetifolia", "Iberis_ciliata_ssp_ciliata", "Iberis_linifolia_ssp_linifolia", "Iberis_nazarita", "Iberis_procumbens_ssp_procumbens", "Iberis_saxatilis_ssp_cinerea", "Iberis_saxatilis_ssp_saxatilis", "Iberis_semperflorens", "Iberis_spathulata", "Iberis_umbellata", "Isatis_allionii", "Isatis_platyloba", "Jonopsidium_abulense", "Jonopsidium_acaule", "Jovibarba_globifera", "Jovibarba_globifera_ssp_arenaria", "Jovibarba_globifera_ssp_globifera", "Jovibarba_globifera_ssp_hirta", "Jovibarba_heuffelii", "Juniperus_sabina", "Larix_decidua", "Lepidium_cardamines", "Lepidium_heterophyllum", "Lepidium_hirtum_ssp_nebrodense", "Lepidium_hirtum_ssp_oxyotum", "Lepidium_meyeri_ssp_meyeri", "Lepidium_villarsii_ssp_villarsii", "Lunaria_rediviva", "Lychnis_flos-cuculi_ssp_subintegra", "Lychnis_flos-jovis", "Lychnis_viscaria_ssp_atropurpurea", "Malcolmia_graeca_ssp_bicolor", "Malcolmia_graeca_ssp_graeca", "Malcolmia_graeca_ssp_hydraea", "Malcolmia_macrocalyx_ssp_scyria", "Malcolmia_maritima", "Malcolmia_orsiniana_ssp_orsiniana", "Matthiola_fruticulosa_ssp_valesiaca", "Matthiola_incana_ssp_rupestris", "Minuartia_austriaca", "Minuartia_bosniaca", "Minuartia_capillacea", "Minuartia_cherlerioides_ssp_cherlerioides", "Minuartia_confusa", "Minuartia_fastigiata", "Minuartia_graminifolia_ssp_clandestina", "Minuartia_graminifolia_ssp_graminifolia", "Minuartia_hirsuta_ssp_frutescens", "Minuartia_laricifolia_ssp_kitaibelii", "Minuartia_laricifolia_ssp_laricifolia", "Minuartia_rubella", "Minuartia_rupestris", "Minuartia_sedoides", "Minuartia_stellata", "Minuartia_villarii", "Moehringia_bavarica_ssp_bavarica", "Moehringia_ciliata", "Moehringia_intricata", "Moehringia_muscosa", "Moehringia_pendula", "Moricandia_moricandioides_ssp_baetica", "Moricandia_moricandioides_ssp_cavanillesiana", "Moricandia_moricandioides_ssp_moricandioides", "Morisia_monanthos", "Murbeckiella_pinnatifida", "Myrica_faya", "Nigella_arvensis_ssp_arvensis", "Nigella_degenii_ssp_degenii", "Nigella_doerfleri", "Nigella_gallica", "Nigella_hispanica_ssp_atlantica")
#spl2 <-c("Nigella_hispanica_ssp_hispanica", "Ophioglossum_azoricum", "Paeonia_broteroi", "Paeonia_mascula_ssp_russoi", "Paeonia_mascula_ssp_triternata", "Papaver_apulum", "Papaver_purpureomarginatum", "Parietaria_serbica", "Paronychia_albanica_ssp_graeca", "Paronychia_aretioides", "Paronychia_macedonica_ssp_macedonica", "Paronychia_rouyana", "Paronychia_suffruticosa_ssp_suffruticosa", "Peltaria_alliacea", "Peltaria_emarginata", "Petrocallis_pyrenaica", "Petrorhagia_candica", "Petrorhagia_fasciculata", "Petrorhagia_glumacea", "Petrorhagia_illyrica_ssp_illyrica", "Petrorhagia_illyrica_ssp_taygetea", "Petrorhagia_obcordata", "Pilularia_globulifera", "Pinus_cembra", "Pinus_heldreichii", "Pinus_mugo", "Pinus_nigra_ssp_laricio", "Pinus_peuce", "Pinus_rotundata", "Pinus_uncinata_sstr", "Polycnemum_heuffelii", "Polygonum_graminifolium", "Polygonum_romanum", "Polygonum_scoparium", "Potentilla_alba", "Potentilla_alchimilloides", "Potentilla_apennina", "Potentilla_apennina_ssp_apennina", "Potentilla_arctica", "Potentilla_asturica", "Potentilla_aurea", "Potentilla_aurea_ssp_aurea", "Potentilla_brauneana", "Potentilla_detommasii", "Potentilla_frigida", "Potentilla_grandiflora", "Potentilla_haynaldiana", "Potentilla_heptaphylla", "Potentilla_heptaphylla_ssp_australis", "Potentilla_heptaphylla_ssp_heptaphylla", "Potentilla_hirta", "Potentilla_hirta_ssp_hirta", "Potentilla_hirta_ssp_laeta", "Potentilla_montana", "Potentilla_montenegrina", "Potentilla_nitida", "Potentilla_nivalis", "Potentilla_nivalis_ssp_nivalis", "Potentilla_pusilla", "Potentilla_pusilla_group", "Potentilla_pyrenaica", "Potentilla_rigoana", "Potentilla_sterilis", "Potentilla_tabernaemontani", "Potentilla_visianii", "Pritzelago_alpina_ssp_auerswaldii", "Pseudofumaria_lutea", "Pseudostellaria_europaea", "Pulsatilla_halleri_ssp_halleri", "Pulsatilla_halleri_ssp_rhodopaea", "Pulsatilla_montana", "Pulsatilla_montana_ssp_balkana", "Pulsatilla_montana_ssp_jankae", "Pulsatilla_montana_ssp_montana", "Pulsatilla_pratensis", "Pulsatilla_rubra_ssp_hispanica", "Pulsatilla_rubra_ssp_rubra", "Pulsatilla_vernalis", "Pulsatilla_vulgaris_ssp_grandis", "Pulsatilla_vulgaris_ssp_vulgaris", "Quercus_congesta", "Quercus_mas", "Quercus_petraea_sstr", "Quercus_robur_ssp_brutia", "Ranunculus_abnormis", "Ranunculus_aconitifolius", "Ranunculus_aduncus", "Ranunculus_alpestris", "Ranunculus_amplexicaulis", "Ranunculus_bupleuroides", "Ranunculus_carinthiacus", "Ranunculus_carpaticus", "Ranunculus_concinnatus", "Ranunculus_cordiger", "Ranunculus_crenatus", "Ranunculus_fallax_slat", "Ranunculus_fallax_ssp_platycolpoides", "Ranunculus_fallax_ssp_viburgensis", "Ranunculus_ficaria_ssp_bulbilifer", "Ranunculus_ficaria_ssp_calthifolius", "Ranunculus_ficaria_ssp_ficaria", "Ranunculus_flammula_ssp_scoticus", "Ranunculus_fluitans", "Ranunculus_gouanii", "Ranunculus_grenieranus", "Ranunculus_hederaceus", "Ranunculus_hybridus", "Ranunculus_incomparabilis", "Ranunculus_lanuginosus", "Ranunculus_longipes", "Ranunculus_monspeliacus", "Ranunculus_montanus_sstr", "Ranunculus_nigrescens", "Ranunculus_nodiflorus", "Ranunculus_olissiponesis_ssp_olissiponesis", "Ranunculus_ololeucos", "Ranunculus_parnassiifolius", "Ranunculus_parnassiifolius_ssp_heterocarpus", "Ranunculus_platanifolius", "Ranunculus_polyanthemos_ssp_polyanthemoides", "Ranunculus_pratensis", "Ranunculus_pseudomillefoliatus", "Ranunculus_pseudomontanus", "Ranunculus_psilostachys", "Ranunculus_pyrenaeus", "Ranunculus_pyrenaeus_ssp_plantagineus", "Ranunculus_pyrenaeus_ssp_pyrenaeus", "Ranunculus_seguieri_ssp_seguieri", "Ranunculus_serbicus", "Ranunculus_thora", "Ranunculus_venetus", "Reseda_glauca", "Reseda_inodora", "Reseda_jacquinii_ssp_jacquinii", "Reseda_paui_ssp_paui", "Reseda_tymphaea_ssp_tymphaea", "Reseda_undata", "Reseda_virgata", "Ribes_multiflorum_ssp_multiflorum", "Ribes_rubrum", "Ricotia_cretica", "Rorippa_lippizensis", "Rosa_abietina", "Rosa_balsamica", "Rosa_glauca", "Rosa_gorenkensis", "Rosa_inodora_slat", "Rosa_pendulina", "Rosa_sherardii", "Rosa_zalana", "Rumex_cantabricus", "Rumex_hydrolapathum", "Rumex_nivalis", "Rumex_papillaris", "Rumex_rupestris", "Rumex_suffruticosus", "Sagina_glabra", "Sagina_nevadensis", "Salicornia_pusilla", "Salix_alpina", "Salix_appendiculata", "Salix_appenina", "Salix_bicolor", "Salix_breviserrata", "Salix_daphnoides", "Salix_foetida", "Salix_glabra", "Salix_glaucosericea", "Salix_hegetschweileri", "Salix_helvetica", "Salix_laggeri", "Salix_mielichhoferi", "Salix_pyrenaica", "Salix_repens", "Salix_repens_coll", "Salix_retusa", "Salix_salviifolia", "Salix_serpillifolia", "Salix_silesiaca", "Salix_waldsteiniana", "Salsola_genistoides", "Sanguisorba_hybrida", "Sanguisorba_lateriflora", "Saponaria_bellidifolia", "Saponaria_caespitosa", "Saponaria_ocymoides_ssp_ocymoides", "Saponaria_pumila", "Sarcocapnos_baetica_ssp_baetica", "Saxifraga_aphylla", "Saxifraga_aretioides", "Saxifraga_aspera", "Saxifraga_biflora", "Saxifraga_blavii", "Saxifraga_bryoides", "Saxifraga_bulbifera", "Saxifraga_burseriana", "Saxifraga_caesia")
#spl3 <-c("Saxifraga_callosa_ssp_callosa", "Saxifraga_camposii_ssp_leptophylla", "Saxifraga_clusii_ssp_clusii", "Saxifraga_clusii_ssp_lepismigena", "Saxifraga_conifera", "Saxifraga_corsica_ssp_corsica", "Saxifraga_corsica_ssp_cossoniana", "Saxifraga_cotyledon", "Saxifraga_crustata", "Saxifraga_cuneata", "Saxifraga_cuneifolia_ssp_cuneifolia", "Saxifraga_cuneifolia_ssp_robusta", "Saxifraga_diapensioides", "Saxifraga_exarata_ssp_ampullacea", "Saxifraga_federici-augusti_ssp_grisebachii", "Saxifraga_fragilis_ssp_fragilis", "Saxifraga_fragilis_ssp_paniculata", "Saxifraga_glabella", "Saxifraga_granulata_ssp_graniticola", "Saxifraga_haenseleri", "Saxifraga_hirsuta_ssp_hirsuta", "Saxifraga_hirsuta_ssp_paucicrenata", "Saxifraga_hostii_ssp_hostii", "Saxifraga_hypnoides", "Saxifraga_latepetiolata", "Saxifraga_losae", "Saxifraga_marginata", "Saxifraga_moncayensis", "Saxifraga_muscoides", "Saxifraga_mutata_ssp_mutata", "Saxifraga_oppositifolia_ssp_rudolphiana", "Saxifraga_osloensis", "Saxifraga_pedemontana_ssp_cervicornis", "Saxifraga_pedemontana_ssp_cymosa", "Saxifraga_pentadactylis_ssp_pentadactylis", "Saxifraga_pentadactylis_ssp_willkommiana", "Saxifraga_petraea", "Saxifraga_porophylla", "Saxifraga_praetermissa", "Saxifraga_retusa_ssp_retusa", "Saxifraga_rosacea_ssp_rosacea", "Saxifraga_rotundifolia_ssp_chrysospleniifolia", "Saxifraga_scardica", "Saxifraga_sedoides", "Saxifraga_seguieri", "Saxifraga_spathularis", "Saxifraga_squarrosa", "Saxifraga_taygetea", "Saxifraga_trifurcata", "Saxifraga_umbrosa", "Schivereckia_podolica", "Scleranthus_annuus_ssp_delortii", "Scleranthus_perennis_ssp_dichotomus", "Sedum_aetnense", "Sedum_anglicum_ssp_anglicum", "Sedum_anglicum_ssp_pyrenaicum", "Sedum_arenarium", "Sedum_atratum", "Sedum_creticum", "Sedum_grisebachii", "Sedum_monregalense", "Sedum_montanum_ssp_montanum", "Sedum_pedicellatum_ssp_pedicellatum", "Sedum_rupestre", "Sedum_sexangulare", "Sedum_stefco", "Sempervivum_arachnoideum", "Sempervivum_calcareum", "Sempervivum_ciliosum", "Sempervivum_marmoreum", "Sempervivum_montanum", "Sempervivum_montanum_ssp_heterophyllum", "Sempervivum_montanum_ssp_montanum", "Sempervivum_montanum_ssp_stiriacum", "Sempervivum_ruthenicum", "Sempervivum_tectorum", "Sempervivum_vicentei", "Sempervivum_wulfenii_ssp_wulfenii", "Sempervivum_zeleborii", "Silene_acaulis_ssp_bryoides", "Silene_alpestris", "Silene_asterias", "Silene_caesia_ssp_caesia", "Silene_catholica", "Silene_chersonensis", "Silene_chromodonta", "Silene_ciliata", "Silene_ciliata_ssp_arvatica", "Silene_coutinhoi", "Silene_cretacea", "Silene_damboldtiana", "Silene_dinarica", "Silene_donetzica", "Silene_echinata", "Silene_exaltata", "Silene_fabarioides", "Silene_flavescens", "Silene_foetida", "Silene_frivaldszkyana", "Silene_germana", "Silene_hayekiana", "Silene_heuffelii", "Silene_italica_ssp_sicula", "Silene_legionensis", "Silene_lerchenfeldiana", "Silene_longicilia_ssp_longicilia", "Silene_marizii", "Silene_mellifera", "Silene_mollissima", "Silene_multicaulis_ssp_multicaulis", "Silene_multicaulis_ssp_sporadum", "Silene_niederi", "Silene_nodulosa", "Silene_nutans_ssp_dubia", "Silene_nutans_ssp_livida", "Silene_otites_group_ssp_hungarica", "Silene_paradoxa", "Silene_parnassica", "Silene_parnassica_ssp_parnassica", "Silene_portensis", "Silene_pusilla", "Silene_radicosa_ssp_radicosa", "Silene_reichenbachii", "Silene_roemeri", "Silene_rubella_ssp_bergiana", "Silene_rupestris", "Silene_saxifraga_sstr", "Silene_scabrifolia", "Silene_sendtneri", "Silene_sericea", "Silene_sieberi", "Silene_spinescens", "Silene_thessalonica_ssp_thessalonica", "Silene_ungeri", "Silene_uniflora_ssp_thorei", "Silene_uniflora_ssp_uniflora", "Silene_uralensis_ssp_apetala", "Silene_vallesia_ssp_graminea", "Silene_vallesia_ssp_vallesia", "Silene_veselskyi", "Silene_vulgaris_ssp_suffrutescens", "Silene_waldsteinii", "Sisymbrella_aspera_ssp_aspera", "Sisymbrium_assoanum", "Sisymbrium_austriacum", "Sisymbrium_austriacum_ssp_austriacum", "Sisymbrium_austriacum_ssp_chrysanthum", "Sisymbrium_austriacum_ssp_contortum", "Sisymbrium_austriacum_ssp_hispanicum", "Sisymbrium_cavanillesianum", "Sisymbrium_strictissimum", "Sisymbrium_supinum", "Sisymbrium_volgense", "Soleirolia_soleirolii", "Spergularia_segetalis", "Spiraea_cana", "Suaeda_maritima_ssp_pannonica", "Syrenia_talijevii", "Teesdaliopsis_conferta", "Thalictrum_calabricum", "Thalictrum_morisonii_ssp_mediterraneum", "Thalictrum_morisonii_ssp_morisonii", "Thalictrum_simplex_ssp_boreale", "Thalictrum_tuberosum", "Thesium_auriculatum", "Thesium_dollineri_ssp_dollineri", "Thesium_dollineri_ssp_simplex", "Thesium_humifusum", "Thesium_linophyllon", "Thesium_parnassi", "Thesium_pyrenaicum_ssp_grandiflorum", "Thesium_pyrenaicum_ssp_pyrenaicum", "Thesium_rostratum", "Thlaspi_dacicum", "Thlaspi_goesingense", "Thlaspi_jankae", "Thlaspi_microphyllum", "Thlaspi_stenopterum", "Thlaspi_sylvium", "Trichomanes_speciosum", "Trollius_europaeus", "Urtica_atrovirens", "Vella_spinosa", "Waldsteinia_geoides", "Waldsteinia_ternata_ssp_trifolia", "Woodsia_pulchella")
#afeSpeciesList <-c(spl2, spl1)
#head(afeSpeciesList)                  
                    

tr8(s_list, download_list = NULL, gui_config = TURE, synonyms=FALSE)
tr8(splist, download_list = NULL, gui_config = TURE, synonyms=FALSE)
