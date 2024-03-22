### eDNA workshop - Olhao, Jan 2024 - ANC

### Data files
### ----------------------------------
# https://github.com/iobis
# https://docs.google.com/document/d/18dQ-ywtbvS2_wa_kQ9PwB_bTAyCYyXFvK1JgujNa2Wo/edit

setwd("C:/Users/anavc/OneDrive/Desktop/CCMAR/eDNA")

### Read in the eDNA data

# https://github.com/iobis/edna-results?tab=readme-ov-file

library(dplyr)
library(stringr)
library(purrr)

dna_files <- list.files("output", "*DNADerivedData*", full.names = TRUE)
occurrence_files <- list.files("output", "*Occurrence*", full.names = TRUE)

dna <- map(dna_files, read.table, sep = "\t", quote = "", header = TRUE) %>%
  bind_rows() %>%
  mutate_if(is.character, na_if, "")

occurrence <- map(occurrence_files, read.table, sep = "\t", quote = "", header = TRUE) %>%
  bind_rows() %>%
  mutate_if(is.character, na_if, "") %>%
  mutate(
    species = ifelse(taxonRank == "species", scientificName, NA),
    aphiaid = as.numeric(str_replace(scientificNameID, "urn:lsid:marinespecies.org:taxname:", ""))
  ) %>%
  left_join(dna, by = "occurrenceID")

### ---

## Get the Marine World Heritage site shapes

# here is the map: https://samples.ednaexpeditions.org/#/

# download the .gpkg file from 
library(sf)
file <- st_read("marine_world_heritage.gpkg")
file

library(ggplot2)
ggplot()+geom_sf(data = file)

head(file)
unique(file$name)

# select one specific site
library(dplyr)
shark_bay_data <- file %>% filter(name == "Shark Bay, Western Australia")

ggplot()+geom_sf(data = shark_bay_data)

### ----------------------------------

### Data preparation
### ----------------------------------

### Data preparation 1

# go to https://github.com/iobis/edna-species-lists/ and download the sp lists
# lists_full contains both the results from eDNA and the OBIS records
setwd("C:/Users/anavc/OneDrive/Desktop/CCMAR/eDNA")

csv_files <- list.files(path = "edna-species-lists-master/edna-species-lists-master/lists_full/csv", pattern = "\\.csv$", full.names = TRUE)
csv_data_list <- lapply(csv_files, read.csv)

# Function to read, modify, and save each CSV file
process_csv <- function(file_path) {
  data <- read.csv(file_path)
  site_name <- tools::file_path_sans_ext(basename(file_path))
  data$site <- site_name
  write.csv(data, file = file_path, row.names = FALSE)
  return(data)
}

modified_data_list <- lapply(csv_files, process_csv)

# all .csv files have the same column names?
Reduce(intersect, lapply(modified_data_list, colnames)) # yes
# combine them
combined_data <- do.call(rbind, modified_data_list)

write.csv(combined_data, file = "combined_data.csv", row.names = FALSE) # all MWH sites, sampled or not

### ---

### Data preparation 2

setwd("C:/Users/ana/OneDrive/Desktop/CCMAR/eDNA")
data <- read.csv("combined_data.csv", stringsAsFactors=T)

# not all sites have been sampled
length(unique(data$site)) # 50 sites
length(unique(data$site[data$source_dna == TRUE])) # 20 sites
sites_with_true <- unique(data$site[data$source_dna == TRUE])

subset_data <- subset(data, site %in% unique(site[source_dna == TRUE])) # 51,301 obs.
write.csv(subset_data, file = "sampled_sites.csv", row.names = FALSE) # only sampled sites

###

setwd("C:/Users/anavc/OneDrive/Desktop/CCMAR/eDNA")
sampled_sites <- read.csv("sampled_sites.csv", stringsAsFactors=T)

str(sampled_sites)
summary(sampled_sites)
unique(sampled_sites$site) # 20 levels
length(unique(sampled_sites$phylum)) # 66 Phyla
length(unique(sampled_sites$species)) # 31,166 species

# there are some rows without data for phylum - replace them
sampled_sites[sampled_sites$phylum == "", ]
length(sampled_sites[sampled_sites$phylum == "", ]) #21
length(unique(sampled_sites[sampled_sites$phylum == "", ]$species)) #6 spp: 5 Protozoa & 1 Chromista

sampled_sites[which(sampled_sites$phylum == "" & sampled_sites$species == "Solenicola setigera"), "phylum"] <- "Bigyra"
sampled_sites$phylum <- factor(sampled_sites$phylum, levels = c(levels(sampled_sites$phylum), "Protozoa incertae sedis")) # create the level
sampled_sites[which(sampled_sites$phylum == ""), "phylum"] <- "Protozoa incertae sedis"
sampled_sites[sampled_sites$phylum == "", ]
length(unique(sampled_sites[sampled_sites$phylum == "", ]$species))

# Create a new column "kingdom" based on the values in the "phylum" column
sampled_sites$kingdom <- ""

# Assign kingdom values based on phylum values
sampled_sites$kingdom[sampled_sites$phylum %in% c("Acanthocephala", "Annelida", "Arthropoda", "Brachiopoda", "Bryozoa", "Chaetognatha", "Chordata", "Cnidaria", "Ctenophora", "Cycliophora", "Dicyemida", "Echinodermata", "Entoprocta", "Gastrotricha", "Gnathostomulida", "Hemichordata", "Kinorhyncha", "Mollusca", "Nematoda", "Nemertea", "Platyhelminthes", "Porifera", "Priapulida", "Rotifera", "Tardigrada", "Xenacoelomorpha")] <- "Animalia"
sampled_sites$kingdom[sampled_sites$phylum %in% c("Amoebozoa", "Choanozoa", "Euglenozoa", "Sulcozoa")] <- "Protozoa"
sampled_sites$kingdom[sampled_sites$phylum %in% c("Acidobacteria", "Actinobacteria", "Bacteroidetes", "Chlorobi", "Cyanobacteria", "Firmicutes", "Lentisphaerae", "Nitrospirae", "Planctomycetes", "Proteobacteria", "Verrucomicrobia")] <- "Bacteria"
sampled_sites$kingdom[sampled_sites$phylum %in% c("Ascomycota", "Basidiomycota", "Zygomycota")] <- "Fungi"
sampled_sites$kingdom[sampled_sites$phylum %in% c("Bacillariophyta", "Bigyra", "Cercozoa", "Ciliophora", "Cryptophyta", "Foraminifera", "Haptophyta", "Myzozoa", "Ochrophyta", "Oomycota", "Phoronida", "Radiozoa", "Rhodophyta")] <- "Chromista"
sampled_sites$kingdom[sampled_sites$phylum %in% c("Bryophyta", "Charophyta", "Chlorophyta", "Prasinodermatophyta", "Tracheophyta")] <- "Plantae"
sampled_sites$kingdom[sampled_sites$phylum %in% c("Euryarchaeota", "Thaumarchaeota")] <- "Archaea"
sampled_sites$kingdom[sampled_sites$phylum %in% c("Protozoa incertae sedis")] <- "Protozoa incertae sedis"
sampled_sites$kingdom[sampled_sites$phylum %in% c("Bacteria incertae sedis")] <- "Bacteria incertae sedis"

sampled_sites$kingdom<-as.factor(sampled_sites$kingdom)
unique(sampled_sites$kingdom) #9
sampled_sites[sampled_sites$kingdom == "", ]

# exclude Kingdom Archaea, Bacteria, Fungi and Protozoa

library(dplyr)
sampled_sites2 <- sampled_sites %>%
  filter(!(kingdom %in% c("Archaea", "Bacteria", "Fungi", "Protozoa", "Bacteria incertae sedis", "Protozoa incertae sedis")))

write.csv(sampled_sites2, file = "sampled_sites2.csv", row.names = F)

species_countss <- aggregate(species ~ phylum, data = sampled_sites2, FUN = function(x) length(unique(x)))
names(species_countss) <- c("Phylum", "SpeciesCount")
species_countss <- species_countss[order(-species_countss$SpeciesCount), ]
species_countss$Percentage <- sprintf("%.2f%%", (species_countss$SpeciesCount / sum(species_countss$SpeciesCount)) * 100)
species_countss
write.csv(species_countss, file = "species_countss.csv", row.names = FALSE)

sampled_sites2 <- read.csv("sampled_sites2.csv", stringsAsFactors=T)
str(sampled_sites2) # 50,702 obs.
summary(sampled_sites2)
length(unique(sampled_sites2$site)) # 20 sites
length(unique(sampled_sites2$phylum)) # 44 Phyla
length(unique(sampled_sites2$species)) # 30,706 spp

###

# additional cleaning - check the environment of the spp in Worms

specieslist<-unique(sampled_sites2$species)
write.csv(specieslist, file = "allspp.csv", row.names = F)
# Taxon Match tool in Worms

sampled_sites3 <- sampled_sites2[!(sampled_sites2$species%in% c("Chrysochromulina tobinii", "Macrosteles quadrilineatus", "Prototheca ciferrii", "Cairina moschata", "Columba livia", "Oryzaephilus surinamensis", "Ardea cinerea", "Hirundo rustica", "Homo sapiens", "Tyrophagus putrescentiae", "Aptinothrips rufus", "Adineta vaga", "Rhinella horribilis", "Meleagris gallopavo", "Albugo candida", "Megaselia scalaris", "Iguana iguana", "Falco sparverius", "Eulimnadia texana", "Lucania goodei", "Erpobdella punctata", "Pheidole megacephala", "Nitzschia palea", "Frustulia vulgaris", "Halamphora calidilacuna", "Cognettia varisetosa", "Nesodrilus crozetensis", "Trichocera maculipennis", "Opogona omoscopa", "Ceratophysella denticulata", "Hypogastrura purpurescens", "Ptygura brachiata", "Cephalodella forficula", "Cephalodella intuta", "Cephalodella stenroosi", "Colurella hindenburgi", "Dicranophorus forcipatus", "Encentrum mustela", "Encentrum uncinatum", "Lecane tenuiseta", "Lepadella (Lepadella) patella", "Lepadella acuminata", "Lepadella latusinus", "Lepadella minuta", "Lepadella triba", "Lepadella triptera", "Notholca hollowdayi", "Notommata cyrtopus", "Proales fallaciosa", "Trichocerca bidens", "Anas poecilorhyncha", "Microtus arvalis", "Aulacorthum solani", "Poterioochromonas malhamensis", "Kumanoa globospora", "Apus affinis", "Bostrychia hagedash", "Estrilda astrild", "Hydra sinensis", "Hydra vulgaris", "Mesocyclops (Mesocyclops) pehpeiensis", "Pseudocrenilabrus philander", "Tilapia sparrmanii", "Enteromius paludinosus", "Enteromius trimaculatus", "Lacustricola katangae", "Lacustricola myaposae", "Hypseleotris tohizonae", "Marcusenius caudisquamatus", "Marcusenius senegalensis", "Clarias gariepinus", "Clarias theodorae", "Hippopotamus amphibius", "Pipistrellus hesperidus", "Melanoides tuberculata", "Tarebia granifera", "Bulinus umbilicatus", "Aulophorus borellii", "Chaetogaster diastrophus", "Dero obtusa", "Prostoma graecense", "Blattella germanica", "Apis mellifera", "Tholymis tillarga", "Hemidactylus mabouia", "Varanus niloticus", "Musa acuminata", "Clypeolum owenianum", "Neritodryas chimmoi", "Septaria bougainvillei", "Pythia scarabaeus", "Stenomelania persulcata", "Culex quinquefasciatus", "Trebouxia aggregata", "Zapornia pusilla", "Tachybaptus novaehollandiae", "Tyto alba", "Quantenobdella howensis", "Amynthas corticis", "Aporrectodea caliginosa", "Eastoniella howeana", "Eastoniella modesta", "Paraplutellus insularis", "Pericryptodrilus nanus", "Spenceriella difficilis", "Spenceriella hollowayi", "Spenceriella howeana", "Spenceriella saundersi", "Adineta steineri", "Hypotaenidia philippensis", "Limnadopsis birchii", "Limnadopsis tatei", "Leiopotherapon aheneus", "Spirocaecum lafii", "Odontesthes smitti", "Microstomum compositum", "Coxiella gilesi", "Coxiella striatula", "Thiara australis", "Anser cygnoides", "Thermocyclops decipiens", "Bangana dero", "Cirrhinus mrigala", "Labeo bata", "Macaca mulatta", "Actinocyclus kützingii", "Coscinodiscus lacustris", "Stephanodiscus invisitatus", "Pseudopediastrum boryanum", "Torularia atra", "Bufo bufo", "Anas acuta", "Anas strepera", "Anser albifrons", "Aythya ferina", "Aythya fuligula", "Branta leucopsis", "Bucephala clangula", "Cygnus columbianus", "Cygnus cygnus", "Cygnus olor", "Mareca penelope", "Spatula clypeata", "Spatula querquedula", "Tadorna tadorna", "Ciconia ciconia", "Ixobrychus cinnamomeus", "Passer domesticus", "Myocastor coypus", "Erpobdella octoculata", "Aporrectodea limicola", "Brachygluta helferi", "Ochthebius nanus", "Tenebrio molitor", "Lepinotus patruelis", "Polyarthra dolichoptera", "Macrobiotus ripperi", "Navicula minima", "Pinnularia acrosphaeria", "Caridina natalensis", "Coscinodiscus nitidus", "Gomphonema olivaceum", "Synedra acus", "Melosira discigera", "Surirella gemma", "Atheta (Thinobaena) vestita", "Leptocera (Rachispoda) fuscipennis", "Smittia whangaroa", "Artemisia maritima")), ] 
length(unique(sampled_sites3$species)) # 30,537 spp / -169 spp

write.csv(sampled_sites3, file = "sampled_sites3.csv", row.names = F)
sampled_sites3 <- read.csv("sampled_sites3.csv", stringsAsFactors=T)
str(sampled_sites3) # 50,492 obs.
length(unique(sampled_sites3$site)) # 20
length(unique(sampled_sites3$phylum)) # 44 Phyla
length(unique(sampled_sites3$species)) # 30,537 spp

# correct incorrect names
library(dplyr)
corrections <- data.frame(incorrect = c("Holothuria (Mertensiothuria) leucospilota", "Semicossyphus pulcher", "Euvola perula", "Zostera noltei", "Caligus biseriodentatus", "Excorallana tricornis", "Gnathia puertoricensis", "Vasum muricatum", "Cuapetes americanus", "Ernsta citrea", "Ernsta multispiculata", "Ernsta rocasensis", "Raja equatorialis", "Vasum caestus", "Berthella californica", "Berthella stellata", "Harrieta faxoni", "Raja texana", "Vermicularia knorrii", "Synasterope setispara", "Porphyra laciniata", "Ischyrocerus longimanus", "Gnathia antarctica", "Solaster longoi", "Florometra mawsoni", "Amphiura dilatata", "Tritonia antarctica", "Leucettusa vera", "Autolytus maclearanus", "Phalacrocorax aristotelis", "Astarte fusca", "Holothuria (Halodeima) atra", "Holothuria (Microthele) nobilis", "Holothuria (Semperothuria) cinerascens", "Ctenoides barnardi", "Sepia pharaonis", "Conus circumactus", "Nassa francolina", "Maxacteon flammeus", "Deltocyathoides orientalis", "Chrysogorgia admete", "Anchistus custos", "Anchistus pectinis", "Calocarcinus crosnieri", "Paranchistus nobilii", "Holothuria (Halodeima) edulis", "Holothuria (Metriatyla) scabra", "Fowleria punctulata", "Sepia andreana", "Sepia kobiensis", "Bufonaria perelegans", "Neodilatilabrum septimum", "Orania nodosa", "Pardalinops marmorata", "Pardalinops testudinaria", "Okenia pilosa", "Berthella martensi", "Perotrochus caledonicus", "Perotrochus wareni", "Aplustrum amplustre", "Japonactaeon sieboldii", "Sollasipelta punctata", "Holothuria (Microthele) whitmaei", "Modiolus peronianus", "Sepia apama", "Sepia cultrata", "Sepia hedleyi", "Sepia novaehollandiae", "Sepia opipara", "Sepia plangon", "Sepia rozella", "Hinemoa suprasculpta", "Protoperidinium diabolum", "Zostera muelleri", "Bacillaria paxillifera", "Sepia elliptica", "Sepia papuensis", "Sepia smithi", "Natator depressa", "Thalasseus sandvicensis", "Metasepia pfefferi", "Vasum tubiferum", "Chaetoceros neogracile", "Coscinodiscus angstii", "Halamphora coffeaeformis", "Parlibellus cruciculus", "Rhizosolenia setigera", "Cerataulus granulata", "Cruciplacolithus neohelis", "Treptacantha baccata", "Cerianthus lloydii", "Corynopsis alderi", "Paraleptastacus spinicaudus", "Astarte borealis", "Sepia elegans", "Sepia orbignyana", "Ceratium tripos", "Gymnodinium gracile", "Gyrodinium britannicum", "Gyrodinium lachryma", "Poteriodendron petiolatum", "Moevenbergia oculofagi", "Laurencia pinnatifida", "Delibus nudus", "Sepia latimanus", "Ulva fasciata"),
                          correct = c("Holothuria leucospilota", "Bodianus pulcher", "Euvola perulus", "Nanozostera noltei", "Caligus pauliani", "Excorallana tricornis tricornis", "Gnathia virginalis", "Volutella muricata", "Palaemonella americana", "Neoernsta citrea", "Neoernsta multispiculata", "Neoernsta rocasensis", "Rostroraja equatorialis", "Volutella caestus", "Boreoberthella californica", "Pleurehdera stellata", "Heteruropus faxoni", "Rostroraja texana", "Vermicularia lumbricalis", "Synasterope setisparsa", "Erythroglossum laciniatum", "Neoischyrocerus longimanus", "Caecognathia antarctica", "Solaster regularis", "Promachocrinus mawsoni", "Amphiura (Amphiura) atlantica", "Myrella antarctica", "Rowella vera", "Epigamia maclearanus", "Gulosus aristotelis", "Laevastarte fusca fusca", "Holothuria atra", "Holothuria nobilis", "Holothuria cinerascens", "Ctenoides suavis", "Acanthosepion pharaonis", "Conus litoglyphus", "Nassa francolinus", "Punctacteon flammeus", "Peponocyathus orientalis", "Parachrysogorgia admete", "Ensiger custos", "Tympanicheles pectinis", "Calocarcinus africanus", "Polkamenes nobilii", "Holothuria edulis", "Holothuria scabra", "Fowleria variegata", "Doratosepion andreanum", "Doratosepion kobiense", "Bufonaria granosa", "Neodilatilabrum succinctum", "Semiricinula nodosa", "Pardalinops marmoratus", "Pardalinops testudinarius", "Ceratodoris pilosa", "Tomoberthella martensi", "Bouchetitrochus caledonicus", "Bouchetitrochus wareni", "Hydatina amplustre", "Acteon sieboldii", "Daedalopelta punctata", "Holothuria whitmaei", "Modiolus areolatus", "Ascarosepion apama", "Ascarosepion cultratum", "Decorisepia hedleyi", "Ascarosepion novaehollandiae", "Ascarosepion opiparum", "Ascarosepion plangon", "Ascarosepion rozella", "Oscilla suprasculpta", "Protoperidinium diabolus", "Nanozostera muelleri", "Vibrio paxillifer", "Acanthosepion ellipticum", "Ascarosepion papuense", "Acanthosepion smithi", "Natator depressus", "Sterna sandvicensis", "Ascarosepion pfefferi", "Florivasum tubiferum", "Chaetoceros neogracilis", "Ethmodiscus punctiger", "Halamphora coffeiformis", "Stauroneis crucicula", "Sundstroemia setigera", "Cerataulus granulatus", "Stauronertha neohelis", "Gongolaria baccata", "Synarachnactis lloydii", "Podocoryna alderi", "Paraleptastacus spinicauda", "Tridonta borealis", "Rhombosepion elegans", "Rhombosepion orbignyanum", "Tripos muelleri", "Balechina gracilis", "Gyrodinium britannia", "Gyrodinium lacryma", "Bicosoeca petiolata", "Gaziella oculofagi", "Osmundea pinnatifida", "Delius nudus", "Ascarosepion latimanus", "Ulva lactuca"))
all(corrections$incorrect %in% sampled_sites3$species)
unique_incorrect <- unique(corrections$incorrect)
for (incorrect_name in unique_incorrect) {
  if (!(incorrect_name %in% levels(sampled_sites3$species))) {
    sampled_sites3$species <- factor(sampled_sites3$species, levels = c(levels(sampled_sites3$species), incorrect_name))
  }
}
unique_incorrect[unique_incorrect %in% levels(sampled_sites3$species)]

sampled_sites3$species <- as.character(sampled_sites3$species)

for (i in 1:nrow(corrections)) {
  replace_indices <- which(sampled_sites3$species == corrections[i, "incorrect"])
  if (length(replace_indices) > 0) {
    sampled_sites3$species[replace_indices] <- corrections[i, "correct"]
  } else {
    warning(paste("Invalid replacement:", corrections[i, "incorrect"], "->", corrections[i, "correct"]))
  }
}

sampled_sites3$species <- factor(sampled_sites3$species)
sum(is.na(sampled_sites3$species))

sampled_sites3[which(sampled_sites3$species == "Delibus nudus"),]
sampled_sites3[which(sampled_sites3$species == "Delius nudus"),]

sampled_sites3[which(sampled_sites3$species == "Holothuria (Mertensiothuria) leucospilota"),]
sampled_sites3[which(sampled_sites3$species == "Holothuria leucospilota"),]

# check that everything is fine
unique(sampled_sites3[sampled_sites3$phylum == "Chordata",]$class)
unique(sampled_sites3[sampled_sites3$class == "Mammalia",]$order)
unique(sampled_sites3[sampled_sites3$order == "Cetartiodactyla",]$family)
sampled_sites3[which(sampled_sites3$class == "Amphibia"),]
sampled_sites3[sampled_sites3$phylum == "Bryophyta",]

write.csv(sampled_sites3, file = "sampled_sites4.csv", row.names = F)
sampled_sites4 <- read.csv("sampled_sites4.csv", stringsAsFactors=T)

str(sampled_sites4) # 50,492 obs.
length(unique(sampled_sites4$site)) # 20
length(unique(sampled_sites4$phylum)) # 44 Phyla
length(unique(sampled_sites4$species)) # 30,463 spp
length(unique(sampled_sites4[sampled_sites4$source_dna == T,]$species)) # 4,219
length(unique(subset(sampled_sites4, (source_obis == TRUE | source_gbif == TRUE))$species)) # 29,560
length(unique(subset(sampled_sites4, source_dna == TRUE & source_obis == FALSE & source_gbif == FALSE)$species)) # 2,173
length(unique(subset(sampled_sites4, source_dna == FALSE & (source_obis == TRUE | source_gbif == TRUE))$species)) # 28,618

# Obtain the fish species

sampled_sites4 <- read.csv("sampled_sites4.csv", stringsAsFactors=T)
unique(sampled_sites3[sampled_sites3$phylum == "Chordata",]$class)
# check https://github.com/iobis/edna-species-lists/blob/master/scripts/script.R#L18
#fish_classes <- c("Actinopteri", "Cladistii", "Coelacanthi", "Elasmobranchii", "Holocephali", "Myxini", "Petromyzonti", "Teleostei")
#BUT Actinopteri= classes Chondrostei, Holostei, Teleostei
fishes <- sampled_sites4[(sampled_sites4$class %in% c("Chondrostei", "Holostei", "Teleostei", "Cladistii", "Coelacanthi", "Elasmobranchii", "Holocephali", "Myxini", "Petromyzonti")), ]
length(unique(fishes$species)) # 7,109 spp

write.csv(fishes, file = "fishes.csv", row.names = F)

fishes <- read.csv("fishes.csv", stringsAsFactors=T)

str(fishes) # 14,986 obs.
length(unique(fishes$site)) # 20
length(unique(fishes$species)) # 7,109 spp
length(unique(fishes[fishes$source_dna == T,]$species)) # 1,995
length(unique(subset(fishes, (source_obis == TRUE | source_gbif == TRUE))$species)) # 6,949
length(unique(subset(fishes, source_dna == TRUE & source_obis == FALSE & source_gbif == FALSE)$species)) # 636
length(unique(subset(fishes, source_dna == FALSE & (source_obis == TRUE | source_gbif == TRUE))$species)) # 6,452


# Groups for graphs

sampled_sites4 <- read.csv("sampled_sites4.csv", stringsAsFactors=T)

species_count_per_phylum <- sampled_sites4 %>%
  group_by(phylum) %>%
  summarise(unique_species = n_distinct(species))

print(species_count_per_phylum, n=44) # > 1000 spp makes a different group

Annelida <- sampled_sites4[(sampled_sites4$class %in% c("Annelida")), ]
Arthropoda <- sampled_sites4[(sampled_sites4$class %in% c("Arthropoda")), ]
Cnidaria <- sampled_sites4[(sampled_sites4$class %in% c("Cnidaria")), ]
Echinoderms <- sampled_sites4[(sampled_sites4$class %in% c("Echinodermata")), ]
Molluscs <- sampled_sites4[(sampled_sites4$class %in% c("Mollusca")), ]

OtherInverts <- sampled_sites4[(sampled_sites4$class %in% c("Acanthocephala","Brachiopoda","Bryozoa","Chaetognatha","Ctenophora","Cycliophora","Dicyemida","Entoprocta","Gastrotricha","Gnathostomulida",
                                                            "Hemichordata","Kinorhyncha","Nematoda","Nemertea","Phoronida","Platyhelminthes","Porifera","Priapulida","Rotifera","Tardigrada","Xenacoelomorpha")), ]

Chromista <- sampled_sites4[(sampled_sites4$phylum %in% c("Bacillariophyta","Bigyra","Cercozoa","Ciliophora","Cryptophyta","Foraminifera","Haptophyta","Myzozoa","Ochrophyta","Oomycota","Radiozoa")), ]
Plantae <- sampled_sites4[(sampled_sites4$phylum %in% c("Bryophyta","Charophyta","Chlorophyta","Prasinodermatophyta","Rhodophyta","Tracheophyta")), ]

# Chordata
species_count_per_class_within_chordata <- sampled_sites4 %>%
  filter(phylum == "Chordata") %>%
  group_by(class) %>%
  summarise(unique_species = n_distinct(species))

print(species_count_per_class_within_chordata)

Fishes <- sampled_sites4[(sampled_sites4$class %in% c("Chondrostei", "Holostei", "Teleostei", "Cladistii", "Coelacanthi", "Elasmobranchii", "Holocephali", "Myxini", "Petromyzonti")), ]
OtherVerts <- sampled_sites4[(sampled_sites4$class %in% c("Aves","Mammalia","Crocodylia","Amphibia","Ascidiacea","Thaliacea","Leptocardii","Appendicularia") | sampled_sites4$order %in% c("Testudines", "Squamata")), ]
#Birds <- sampled_sites4[(sampled_sites4$class %in% c("Aves")), ]
#Mammals <- sampled_sites4[(sampled_sites4$class %in% c("Mammalia")), ]
#Herps <- sampled_sites4[(sampled_sites4$order %in% c("Testudines", "Squamata") | sampled_sites4$class %in% c("Crocodylia", "Amphibia")), ]
#OtherChordata <- sampled_sites4[(sampled_sites4$class %in% c("Ascidiacea","Thaliacea","Leptocardii","Appendicularia")), ]

# Create a new column "group"
sampled_sites4$group <- ""

sampled_sites4$group[sampled_sites4$phylum %in% c("Annelida")] <- "Annelida"
sampled_sites4$group[sampled_sites4$phylum %in% c("Arthropoda")] <- "Arthropoda"
sampled_sites4$group[sampled_sites4$phylum %in% c("Cnidaria")] <- "Cnidaria"
sampled_sites4$group[sampled_sites4$phylum %in% c("Echinodermata")] <- "Echinodermata"
sampled_sites4$group[sampled_sites4$phylum %in% c("Mollusca")] <- "Mollusca"

sampled_sites4$group[sampled_sites4$phylum %in% c("Acanthocephala","Brachiopoda","Bryozoa","Chaetognatha","Ctenophora","Cycliophora","Dicyemida","Entoprocta","Gastrotricha","Gnathostomulida",
                                                  "Hemichordata","Kinorhyncha","Nematoda","Nemertea","Phoronida","Platyhelminthes","Porifera","Priapulida","Rotifera","Tardigrada","Xenacoelomorpha")] <- "Other Invertebrates"

sampled_sites4$group[sampled_sites4$phylum %in% c("Bacillariophyta","Bigyra","Cercozoa","Ciliophora","Cryptophyta","Foraminifera","Haptophyta","Myzozoa","Ochrophyta","Oomycota","Radiozoa")] <- "Chromista"
sampled_sites4$group[sampled_sites4$phylum %in% c("Bryophyta","Charophyta","Chlorophyta","Prasinodermatophyta","Rhodophyta","Tracheophyta")] <- "Plantae"

sampled_sites4$group[sampled_sites4$class %in% c("Chondrostei", "Holostei", "Teleostei", "Cladistii", "Coelacanthi", "Elasmobranchii", "Holocephali", "Myxini", "Petromyzonti")] <- "Fishes"
sampled_sites4$group[sampled_sites4$class %in% c("Aves","Mammalia","Crocodylia","Amphibia","Ascidiacea","Thaliacea","Leptocardii","Appendicularia") | sampled_sites4$order %in% c("Testudines", "Squamata")] <- "Other Vertebrates"


sampled_sites4$group<-as.factor(sampled_sites4$group)
length(unique(sampled_sites4$group)) # 10

write.csv(sampled_sites4, file = "sampled_sites4.csv", row.names = F)

### ----------------------------------

### Data analyses ###

### Species Richness
### ----------------------------------

setwd("C:/Users/ana/OneDrive/Desktop/CCMAR/eDNA")
sampled_sites4 <- read.csv("sampled_sites4.csv", stringsAsFactors=T)

# order the sites latitudinally
ordered_sites <- c(
  "wadden_sea",
  "gulf_of_porto_calanche_of_piana_gulf_of_girolata_scandola_reserve",
  "everglades_national_park",
  "the_sundarbans",
  "banc_d_arguin_national_park",
  "archipielago_de_revillagigedo",
  "belize_barrier_reef_reserve_system",
  "socotra_archipelago",
  "tubbataha_reefs_natural_park",
  "coiba_national_park_and_its_special_zone_of_marine_protection",
  "cocos_island_national_park",
  "brazilian_atlantic_islands_fernando_de_noronha_and_atol_das_rocas_reserves",
  "aldabra_atoll",
  "lagoons_of_new_caledonia_reef_diversity_and_associated_ecosystems",
  "ningaloo_coast",
  "shark_bay_western_australia",
  "isimangaliso_wetland_park",
  "lord_howe_island_group",
  "peninsula_valdes",
  "french_austral_lands_and_seas")

sampled_sites4$site <- factor(
  sampled_sites4$site,
  levels = ordered_sites,
  labels = c(
    "Wadden Sea",
    "Gulf of Porto Calanche of Piana, Gulf of Girolata, Scandola Reserve",
    "Everglades National Park",
    "The Sundarbans",
    "Banc d'Arguin National Park",
    "Archipiélago de Revillagigedo",
    "Belize Barrier Reef Reserve System",
    "Socotra Archipelago",
    "Tubbataha Reefs Natural Park",
    "Coiba National Park and its Special Zone of Marine Protection",
    "Cocos Island National Park",
    "Brazilian Atlantic Islands: Fernando de Noronha and Atol das Rocas Reserves",
    "Aldabra Atoll",
    "Lagoons of New Caledonia: Reef Diversity and Associated Ecosystems",
    "Ningaloo Coast",
    "Shark Bay, Western Australia",
    "iSimangaliso Wetland Park",
    "Lord Howe Island Group",
    "Peninsula Valdés",
    "French Austral Lands and Seas"))

# order groups
ordered_groups <- c("Plantae", "Chromista", "Other Vertebrates", "Fishes", "Other Invertebrates", "Mollusca", "Echinodermata", "Cnidaria", "Arthropoda", "Annelida")

sampled_sites4$group <- factor(
  sampled_sites4$group,
  levels = ordered_groups,
  labels = c("Plantae", "Chromista", "Other Vertebrates", "Fishes", "Other Invertebrates", "Mollusca", "Echinodermata", "Cnidaria", "Arthropoda", "Annelida"))

# all
library(dplyr)
library(ggplot2)
library(forcats)

species_counts <- sampled_sites4 %>%
  group_by(site, group) %>%
  summarise(species_richness = n_distinct(species))

my_theme=theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),axis.line.x = element_line(linewidth = 1),axis.line.y = element_line(linewidth = 1),axis.title = element_text(size = 12),axis.text = element_text(size = 12))
my_colors <- c("#2ca02c", "#7f7f7f",  "#1f77b4", "#17becf", "#d62728", "#8c564b", "#ff7f0e", "#9467bd", "#e377c2", "#bcbd22")

ggplot(species_counts, aes(x = fct_rev(site), y = species_richness, fill = group)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "World Heritage Site", y = "Species Richness") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 8000, by = 2000),
    labels = seq(0, 8000, by = 2000)) +
  scale_fill_manual(values = my_colors) +
  my_theme + ggtitle("a) All")+ guides(fill = guide_legend(reverse = TRUE))
# barplot_1a (1200x500)
write.csv(species_counts, file= "Output_data/1a.csv", row.names= F)

#OBIS/GBIF
subset <- subset(sampled_sites4, (source_obis == TRUE | source_gbif == TRUE))
species_counts <- subset %>%
  group_by(site, group) %>%
  summarise(species_richness = n_distinct(species))

ggplot(species_counts, aes(x = fct_rev(site), y = species_richness, fill = group)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "World Heritage Site", y = "Species Richness") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 8000, by = 2000),
    labels = seq(0, 8000, by = 2000)) +
  scale_fill_manual(values = my_colors) +
  my_theme + ggtitle("b) OBIS/GBIF")+ guides(fill = guide_legend(reverse = TRUE))
# barplot_1b (1200x500)
write.csv(species_counts, file= "Output_data/1b.csv", row.names= F)

#eDNA
subset <- subset(sampled_sites4, (source_dna == TRUE))
species_counts <- subset %>%
  group_by(site, group) %>%
  summarise(species_richness = n_distinct(species))

ggplot(species_counts, aes(x = fct_rev(site), y = species_richness, fill = group)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "World Heritage Site", y = "Species Richness") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 1000, by = 200),
    labels = seq(0, 1000, by = 200)) +
  scale_fill_manual(values = my_colors) +
  my_theme + ggtitle("c) eDNA")+ guides(fill = guide_legend(reverse = TRUE))
# barplot_1c (1200x500)
write.csv(species_counts, file= "Output_data/1c.csv", row.names= F)

#unique eDNA
subset <- subset(sampled_sites4, (source_dna == TRUE & source_obis == FALSE & source_gbif == FALSE))
species_counts <- subset %>%
  group_by(site, group) %>%
  summarise(species_richness = n_distinct(species))

ggplot(species_counts, aes(x = fct_rev(site), y = species_richness, fill = group)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "World Heritage Site", y = "Species Richness") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 1000, by = 200),
    labels = seq(0, 1000, by = 200)) + 
  scale_fill_manual(values = my_colors) +
  my_theme + ggtitle("d) Unique eDNA")+ guides(fill = guide_legend(reverse = TRUE))
# barplot_1d (1200x500)
write.csv(species_counts, file= "Output_data/1d.csv", row.names= F)

### ---

# Repeat for fishes

setwd("C:/Users/ana/OneDrive/Desktop/CCMAR/eDNA")
fishes <- read.csv("fishes.csv", stringsAsFactors=T)

# order the sites latitudinally
ordered_sites <- c(
  "wadden_sea",
  "gulf_of_porto_calanche_of_piana_gulf_of_girolata_scandola_reserve",
  "everglades_national_park",
  "the_sundarbans",
  "banc_d_arguin_national_park",
  "archipielago_de_revillagigedo",
  "belize_barrier_reef_reserve_system",
  "socotra_archipelago",
  "tubbataha_reefs_natural_park",
  "coiba_national_park_and_its_special_zone_of_marine_protection",
  "cocos_island_national_park",
  "brazilian_atlantic_islands_fernando_de_noronha_and_atol_das_rocas_reserves",
  "aldabra_atoll",
  "lagoons_of_new_caledonia_reef_diversity_and_associated_ecosystems",
  "ningaloo_coast",
  "shark_bay_western_australia",
  "isimangaliso_wetland_park",
  "lord_howe_island_group",
  "peninsula_valdes",
  "french_austral_lands_and_seas")

fishes$site <- factor(
  fishes$site,
  levels = ordered_sites,
  labels = c(
    "Wadden Sea",
    "Gulf of Porto Calanche of Piana, Gulf of Girolata, Scandola Reserve",
    "Everglades National Park",
    "The Sundarbans",
    "Banc d'Arguin National Park",
    "Archipiélago de Revillagigedo",
    "Belize Barrier Reef Reserve System",
    "Socotra Archipelago",
    "Tubbataha Reefs Natural Park",
    "Coiba National Park and its Special Zone of Marine Protection",
    "Cocos Island National Park",
    "Brazilian Atlantic Islands: Fernando de Noronha and Atol das Rocas Reserves",
    "Aldabra Atoll",
    "Lagoons of New Caledonia: Reef Diversity and Associated Ecosystems",
    "Ningaloo Coast",
    "Shark Bay, Western Australia",
    "iSimangaliso Wetland Park",
    "Lord Howe Island Group",
    "Peninsula Valdés",
    "French Austral Lands and Seas"))

# all
library(dplyr)
library(ggplot2)
library(forcats)

species_counts <- fishes %>%
  group_by(site, class) %>%
  summarise(species_richness = n_distinct(species))

my_theme=theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),axis.line.x = element_line(linewidth = 1),axis.line.y = element_line(linewidth = 1),axis.title = element_text(size = 12),axis.text = element_text(size = 12))
my_colors <- c("#2ca02c", "#1f77b4", "#17becf", "#d62728", "#ff7f0e", "#9467bd", "#e377c2", "#bcbd22", "#7f7f7f")

ggplot(species_counts, aes(x = fct_rev(site), y = species_richness, fill = class)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "World Heritage Site", y = "Species Richness") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 3000, by = 500),
    labels = seq(0, 3000, by = 500)) + 
  scale_fill_manual(values = my_colors) + my_theme + ggtitle("a) All")+ guides(fill = guide_legend(reverse = TRUE))
# barplot_2a (1200x500)
write.csv(species_counts, file= "Output_data/2a.csv", row.names= F)

#OBIS/GBIF
subset <- subset(fishes, (source_obis == TRUE | source_gbif == TRUE))
species_counts <- subset %>%
  group_by(site, class) %>%
  summarise(species_richness = n_distinct(species))

ggplot(species_counts, aes(x = fct_rev(site), y = species_richness, fill = class)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "World Heritage Site", y = "Species Richness") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 3000, by = 500),
    labels = seq(0, 3000, by = 500)) +
  scale_fill_manual(values = my_colors) + my_theme + ggtitle("b) OBIS/GBIF")+ guides(fill = guide_legend(reverse = TRUE))
# barplot_2b (1200x500)
write.csv(species_counts, file= "Output_data/2b.csv", row.names= F)

#eDNA
subset <- subset(fishes, (source_dna == TRUE))
species_counts <- subset %>%
  group_by(site, class) %>%
  summarise(species_richness = n_distinct(species))

my_colors <- c("#d62728", "#9467bd", "#bcbd22", "#7f7f7f")

ggplot(species_counts, aes(x = fct_rev(site), y = species_richness, fill = class)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "World Heritage Site", y = "Species Richness") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 1000, by = 200),
    labels = seq(0, 1000, by = 200)) +
  scale_fill_manual(values = my_colors) + my_theme + ggtitle("c) eDNA")+ guides(fill = guide_legend(reverse = TRUE))
# barplot_2c (1200x500)
write.csv(species_counts, file= "Output_data/2c.csv", row.names= F)

#unique eDNA
subset <- subset(fishes, (source_dna == TRUE & source_obis == FALSE & source_gbif == FALSE))
species_counts <- subset %>%
  group_by(site, class) %>%
  summarise(species_richness = n_distinct(species))

my_colors <- c("#d62728", "#7f7f7f")

ggplot(species_counts, aes(x = fct_rev(site), y = species_richness, fill = class)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "World Heritage Site", y = "Species Richness") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 200, by = 25),
    labels = seq(0, 200, by = 25)) +
  scale_fill_manual(values = my_colors) + my_theme + ggtitle("d) Unique eDNA")+ guides(fill = guide_legend(reverse = TRUE))
# barplot_2d (1200x500)
write.csv(species_counts, file= "Output_data/2d.csv", row.names= F)

### ----------------------------------

### Threatened species
### ----------------------------------

setwd("C:/Users/ana/OneDrive/Desktop/CCMAR/eDNA")
sampled_sites4 <- read.csv("sampled_sites4.csv", stringsAsFactors=T)

# order the sites latitudinally
ordered_sites <- c(
  "wadden_sea",
  "gulf_of_porto_calanche_of_piana_gulf_of_girolata_scandola_reserve",
  "everglades_national_park",
  "the_sundarbans",
  "banc_d_arguin_national_park",
  "archipielago_de_revillagigedo",
  "belize_barrier_reef_reserve_system",
  "socotra_archipelago",
  "tubbataha_reefs_natural_park",
  "coiba_national_park_and_its_special_zone_of_marine_protection",
  "cocos_island_national_park",
  "brazilian_atlantic_islands_fernando_de_noronha_and_atol_das_rocas_reserves",
  "aldabra_atoll",
  "lagoons_of_new_caledonia_reef_diversity_and_associated_ecosystems",
  "ningaloo_coast",
  "shark_bay_western_australia",
  "isimangaliso_wetland_park",
  "lord_howe_island_group",
  "peninsula_valdes",
  "french_austral_lands_and_seas")

sampled_sites4$site <- factor(
  sampled_sites4$site,
  levels = ordered_sites,
  labels = c(
    "Wadden Sea",
    "Gulf of Porto Calanche of Piana, Gulf of Girolata, Scandola Reserve",
    "Everglades National Park",
    "The Sundarbans",
    "Banc d'Arguin National Park",
    "Archipiélago de Revillagigedo",
    "Belize Barrier Reef Reserve System",
    "Socotra Archipelago",
    "Tubbataha Reefs Natural Park",
    "Coiba National Park and its Special Zone of Marine Protection",
    "Cocos Island National Park",
    "Brazilian Atlantic Islands: Fernando de Noronha and Atol das Rocas Reserves",
    "Aldabra Atoll",
    "Lagoons of New Caledonia: Reef Diversity and Associated Ecosystems",
    "Ningaloo Coast",
    "Shark Bay, Western Australia",
    "iSimangaliso Wetland Park",
    "Lord Howe Island Group",
    "Peninsula Valdés",
    "French Austral Lands and Seas"))

# 10 categories: LC=least concern, NT=near threatened, VU=vulnerable, DD=data deficient, EN=endangered, CR=critically endangered, LR/cd=Lower Risk/Conservation Dependent, LR/lc=Lower Risk/Least Concern, LR/nt=Lower Risk/Near Threatened, EX=Extinct
sampled_sites4[sampled_sites4$redlist_category == "EX",] # Coregonus oxyrinchus

subsample <- sampled_sites4[sampled_sites4$redlist_category %in% c("VU", "EN", "CR"), ]

ordered_sites <- c("CR", "EN", "VU")

subsample$redlist_category <- factor(
  subsample$redlist_category,
  levels = ordered_sites,
  labels = c("CR", "EN", "VU"))

# all

species_counts <- subsample %>%
  group_by(site, redlist_category) %>%
  summarise(species_richness = n_distinct(species))

my_theme=theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),axis.line.x = element_line(linewidth = 1),axis.line.y = element_line(linewidth = 1),axis.title = element_text(size = 12),axis.text = element_text(size = 12))
my_colors <- c("#d62728", "#ff7f0e", "#bcbd22")

ggplot(species_counts, aes(x = fct_rev(site), y = species_richness, fill = redlist_category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "World Heritage Site", y = "Species Richness") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 200, by = 50),
    labels = seq(0, 200, by = 50)) +
  scale_fill_manual(values = my_colors) +
  my_theme + ggtitle("a) All")+ guides(fill = guide_legend(reverse = TRUE))
# barplot_3a (1200x500)
write.csv(species_counts, file= "Output_data/3a.csv", row.names= F)

#OBIS/GBIF
subset <- subset(subsample, (source_obis == TRUE | source_gbif == TRUE))
species_counts <- subset %>%
  group_by(site, redlist_category) %>%
  summarise(species_richness = n_distinct(species))

ggplot(species_counts, aes(x = fct_rev(site), y = species_richness, fill = redlist_category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "World Heritage Site", y = "Species Richness") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 200, by = 50),
    labels = seq(0, 200, by = 50)) +
  scale_fill_manual(values = my_colors) +
  my_theme + ggtitle("b) OBIS/GBIF")+ guides(fill = guide_legend(reverse = TRUE))
# barplot_3b (1200x500)
write.csv(species_counts, file= "Output_data/3b.csv", row.names= F)

#eDNA
subset <- subset(subsample, (source_dna == TRUE))
species_counts <- subset %>%
  group_by(site, redlist_category) %>%
  summarise(species_richness = n_distinct(species))

ggplot(species_counts, aes(x = fct_rev(site), y = species_richness, fill = redlist_category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "World Heritage Site", y = "Species Richness") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 50, by = 10),
    labels = seq(0, 50, by = 10)) +
  scale_fill_manual(values = my_colors) +
  my_theme + ggtitle("c) eDNA")+ guides(fill = guide_legend(reverse = TRUE))
# barplot_3c (1200x500)
write.csv(species_counts, file= "Output_data/3c.csv", row.names= F)

#unique eDNA
subset <- subset(subsample, (source_dna == TRUE & source_obis == FALSE & source_gbif == FALSE))
species_counts <- subset %>%
  group_by(site, redlist_category) %>%
  summarise(species_richness = n_distinct(species))

ggplot(species_counts, aes(x = fct_rev(site), y = species_richness, fill = redlist_category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "World Heritage Site", y = "Species Richness") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 50, by = 5),
    labels = seq(0, 50, by = 5)) + 
  scale_fill_manual(values = my_colors) +
  my_theme + ggtitle("d) Unique eDNA")+ guides(fill = guide_legend(reverse = TRUE))
# barplot_3d (1200x500)
write.csv(species_counts, file= "Output_data/3d.csv", row.names= F)

### ---

# Repeat for fishes

setwd("C:/Users/ana/OneDrive/Desktop/CCMAR/eDNA")
fishes <- read.csv("fishes.csv", stringsAsFactors=T)

# order the sites latitudinally
ordered_sites <- c(
  "wadden_sea",
  "gulf_of_porto_calanche_of_piana_gulf_of_girolata_scandola_reserve",
  "everglades_national_park",
  "the_sundarbans",
  "banc_d_arguin_national_park",
  "archipielago_de_revillagigedo",
  "belize_barrier_reef_reserve_system",
  "socotra_archipelago",
  "tubbataha_reefs_natural_park",
  "coiba_national_park_and_its_special_zone_of_marine_protection",
  "cocos_island_national_park",
  "brazilian_atlantic_islands_fernando_de_noronha_and_atol_das_rocas_reserves",
  "aldabra_atoll",
  "lagoons_of_new_caledonia_reef_diversity_and_associated_ecosystems",
  "ningaloo_coast",
  "shark_bay_western_australia",
  "isimangaliso_wetland_park",
  "lord_howe_island_group",
  "peninsula_valdes",
  "french_austral_lands_and_seas")

fishes$site <- factor(
  fishes$site,
  levels = ordered_sites,
  labels = c(
    "Wadden Sea",
    "Gulf of Porto Calanche of Piana, Gulf of Girolata, Scandola Reserve",
    "Everglades National Park",
    "The Sundarbans",
    "Banc d'Arguin National Park",
    "Archipiélago de Revillagigedo",
    "Belize Barrier Reef Reserve System",
    "Socotra Archipelago",
    "Tubbataha Reefs Natural Park",
    "Coiba National Park and its Special Zone of Marine Protection",
    "Cocos Island National Park",
    "Brazilian Atlantic Islands: Fernando de Noronha and Atol das Rocas Reserves",
    "Aldabra Atoll",
    "Lagoons of New Caledonia: Reef Diversity and Associated Ecosystems",
    "Ningaloo Coast",
    "Shark Bay, Western Australia",
    "iSimangaliso Wetland Park",
    "Lord Howe Island Group",
    "Peninsula Valdés",
    "French Austral Lands and Seas"))

subsample <- fishes[fishes$redlist_category %in% c("VU", "EN", "CR"), ]

ordered_sites <- c("CR", "EN", "VU")
subsample$redlist_category <- factor(
  subsample$redlist_category,
  levels = ordered_sites,
  labels = c("CR", "EN", "VU"))

# all

species_counts <- subsample %>%
  group_by(site, redlist_category) %>%
  summarise(species_richness = n_distinct(species))

my_theme=theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),axis.line.x = element_line(linewidth = 1),axis.line.y = element_line(linewidth = 1),axis.title = element_text(size = 12),axis.text = element_text(size = 12))
my_colors <- c("#d62728", "#ff7f0e", "#bcbd22")

ggplot(species_counts, aes(x = fct_rev(site), y = species_richness, fill = redlist_category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "World Heritage Site", y = "Species Richness") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 200, by = 50),
    labels = seq(0, 200, by = 50)) +
  scale_fill_manual(values = my_colors) +
  my_theme + ggtitle("a) All")+ guides(fill = guide_legend(reverse = TRUE))
# barplot_4a (1200x500)
write.csv(species_counts, file= "Output_data/4a.csv", row.names= F)

#OBIS/GBIF
subset <- subset(subsample, (source_obis == TRUE | source_gbif == TRUE))
species_counts <- subset %>%
  group_by(site, redlist_category) %>%
  summarise(species_richness = n_distinct(species))

ggplot(species_counts, aes(x = fct_rev(site), y = species_richness, fill = redlist_category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "World Heritage Site", y = "Species Richness") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 200, by = 50),
    labels = seq(0, 200, by = 50)) +
  scale_fill_manual(values = my_colors) +
  my_theme + ggtitle("b) OBIS/GBIF")+ guides(fill = guide_legend(reverse = TRUE))
# barplot_4b (1200x500)
write.csv(species_counts, file= "Output_data/4b.csv", row.names= F)

#eDNA
subset <- subset(subsample, (source_dna == TRUE))
species_counts <- subset %>%
  group_by(site, redlist_category) %>%
  summarise(species_richness = n_distinct(species))

ggplot(species_counts, aes(x = fct_rev(site), y = species_richness, fill = redlist_category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "World Heritage Site", y = "Species Richness") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 50, by = 10),
    labels = seq(0, 50, by = 10)) +
  scale_fill_manual(values = my_colors) +
  my_theme + ggtitle("c) eDNA")+ guides(fill = guide_legend(reverse = TRUE))
# barplot_4c (1200x500)
write.csv(species_counts, file= "Output_data/4c.csv", row.names= F)

#unique eDNA
subset <- subset(subsample, (source_dna == TRUE & source_obis == FALSE & source_gbif == FALSE))
species_counts <- subset %>%
  group_by(site, redlist_category) %>%
  summarise(species_richness = n_distinct(species))

ggplot(species_counts, aes(x = fct_rev(site), y = species_richness, fill = redlist_category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "World Heritage Site", y = "Species Richness") +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 50, by = 5),
    labels = seq(0, 50, by = 5)) + 
  scale_fill_manual(values = my_colors) +
  my_theme + ggtitle("d) Unique eDNA")+ guides(fill = guide_legend(reverse = TRUE))
# barplot_4d (1200x500)
write.csv(species_counts, file= "Output_data/4d.csv", row.names= F)

### ----------------------------------

### Phylogenetic diversity
### ----------------------------------

setwd("C:/Users/ana/OneDrive/Desktop/CCMAR/eDNA")
sampled_sites4 <- read.csv("sampled_sites4.csv", stringsAsFactors=T)

# order the sites latitudinally
ordered_sites <- c(
  "wadden_sea",
  "gulf_of_porto_calanche_of_piana_gulf_of_girolata_scandola_reserve",
  "everglades_national_park",
  "the_sundarbans",
  "banc_d_arguin_national_park",
  "archipielago_de_revillagigedo",
  "belize_barrier_reef_reserve_system",
  "socotra_archipelago",
  "tubbataha_reefs_natural_park",
  "coiba_national_park_and_its_special_zone_of_marine_protection",
  "cocos_island_national_park",
  "brazilian_atlantic_islands_fernando_de_noronha_and_atol_das_rocas_reserves",
  "aldabra_atoll",
  "lagoons_of_new_caledonia_reef_diversity_and_associated_ecosystems",
  "ningaloo_coast",
  "shark_bay_western_australia",
  "isimangaliso_wetland_park",
  "lord_howe_island_group",
  "peninsula_valdes",
  "french_austral_lands_and_seas")

sampled_sites4$site <- factor(
  sampled_sites4$site,
  levels = ordered_sites,
  labels = c(
    "Wadden Sea",
    "Gulf of Porto Calanche of Piana, Gulf of Girolata, Scandola Reserve",
    "Everglades National Park",
    "The Sundarbans",
    "Banc d'Arguin National Park",
    "Archipiélago de Revillagigedo",
    "Belize Barrier Reef Reserve System",
    "Socotra Archipelago",
    "Tubbataha Reefs Natural Park",
    "Coiba National Park and its Special Zone of Marine Protection",
    "Cocos Island National Park",
    "Brazilian Atlantic Islands: Fernando de Noronha and Atol das Rocas Reserves",
    "Aldabra Atoll",
    "Lagoons of New Caledonia: Reef Diversity and Associated Ecosystems",
    "Ningaloo Coast",
    "Shark Bay, Western Australia",
    "iSimangaliso Wetland Park",
    "Lord Howe Island Group",
    "Peninsula Valdés",
    "French Austral Lands and Seas"))

unique_counts <- sampled_sites4 %>%
  group_by(site) %>%
  summarise(
    class_count = n_distinct(ifelse(!is.na(class) & class != "", class, NA)),
    order_count = n_distinct(ifelse(!is.na(order) & order != "", order, NA)),
    family_count = n_distinct(ifelse(!is.na(family) & family != "", family, NA)),
    genus_count = n_distinct(ifelse(!is.na(genus) & genus != "", genus, NA)),
    species_count = n_distinct(ifelse(!is.na(species) & species != "", species, NA))
  )

# Calculate STL score
unique_counts <- mutate(unique_counts, STL = (class_count * 5) + (order_count * 4) + (family_count * 3) + (genus_count * 2))

my_theme=theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),axis.line.x = element_line(linewidth = 1),axis.line.y = element_line(linewidth = 1),axis.title = element_text(size = 12),axis.text = element_text(size = 12))
ggplot(unique_counts, aes(x = fct_rev(site), y = STL)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, fill="#7f7f7f") +
  labs(x = "Site", y = "STL") +
  scale_y_continuous(
    breaks = seq(0, 12000, by = 2000),
    labels = seq(0, 12000, by = 2000)) +
  my_theme +coord_flip()
# barplot_5a
write.csv(unique_counts, file= "Output_data/5a.csv", row.names= F)

# Calculate STL/spp score
unique_counts <- mutate(unique_counts, STLspp = STL / species_count)

ggplot(unique_counts, aes(x = fct_rev(site), y = STLspp)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, fill="#7f7f7f") +
  labs(x = "Site", y = "STL/spp") +
  scale_y_continuous(
    breaks = seq(0, 10, by = 2),
    labels = seq(0, 10, by = 2)) +
  my_theme +coord_flip()
# barplot_5c
write.csv(unique_counts, file= "Output_data/5c.csv", row.names= F)

### ---

# Repeat for fishes

setwd("C:/Users/ana/OneDrive/Desktop/CCMAR/eDNA")
fishes <- read.csv("fishes.csv", stringsAsFactors=T)

# order the sites latitudinally
ordered_sites <- c(
  "wadden_sea",
  "gulf_of_porto_calanche_of_piana_gulf_of_girolata_scandola_reserve",
  "everglades_national_park",
  "the_sundarbans",
  "banc_d_arguin_national_park",
  "archipielago_de_revillagigedo",
  "belize_barrier_reef_reserve_system",
  "socotra_archipelago",
  "tubbataha_reefs_natural_park",
  "coiba_national_park_and_its_special_zone_of_marine_protection",
  "cocos_island_national_park",
  "brazilian_atlantic_islands_fernando_de_noronha_and_atol_das_rocas_reserves",
  "aldabra_atoll",
  "lagoons_of_new_caledonia_reef_diversity_and_associated_ecosystems",
  "ningaloo_coast",
  "shark_bay_western_australia",
  "isimangaliso_wetland_park",
  "lord_howe_island_group",
  "peninsula_valdes",
  "french_austral_lands_and_seas")

fishes$site <- factor(
  fishes$site,
  levels = ordered_sites,
  labels = c(
    "Wadden Sea",
    "Gulf of Porto Calanche of Piana, Gulf of Girolata, Scandola Reserve",
    "Everglades National Park",
    "The Sundarbans",
    "Banc d'Arguin National Park",
    "Archipiélago de Revillagigedo",
    "Belize Barrier Reef Reserve System",
    "Socotra Archipelago",
    "Tubbataha Reefs Natural Park",
    "Coiba National Park and its Special Zone of Marine Protection",
    "Cocos Island National Park",
    "Brazilian Atlantic Islands: Fernando de Noronha and Atol das Rocas Reserves",
    "Aldabra Atoll",
    "Lagoons of New Caledonia: Reef Diversity and Associated Ecosystems",
    "Ningaloo Coast",
    "Shark Bay, Western Australia",
    "iSimangaliso Wetland Park",
    "Lord Howe Island Group",
    "Peninsula Valdés",
    "French Austral Lands and Seas"))

unique_counts <- fishes %>%
  group_by(site) %>%
  summarise(
    class_count = n_distinct(ifelse(!is.na(class) & class != "", class, NA)),
    order_count = n_distinct(ifelse(!is.na(order) & order != "", order, NA)),
    family_count = n_distinct(ifelse(!is.na(family) & family != "", family, NA)),
    genus_count = n_distinct(ifelse(!is.na(genus) & genus != "", genus, NA)),
    species_count = n_distinct(ifelse(!is.na(species) & species != "", species, NA))
  )

# Calculate STL score
unique_counts <- mutate(unique_counts, STL = (class_count * 5) + (order_count * 4) + (family_count * 3) + (genus_count * 2))

my_theme=theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),axis.line.x = element_line(linewidth = 1),axis.line.y = element_line(linewidth = 1),axis.title = element_text(size = 12),axis.text = element_text(size = 12))
ggplot(unique_counts, aes(x = fct_rev(site), y = STL)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, fill="#7f7f7f") +
  labs(x = "Site", y = "STL") +
  scale_y_continuous(
    breaks = seq(0, 3000, by = 1000),
    labels = seq(0, 3000, by = 1000)) +
  my_theme +coord_flip()
# barplot_5b
write.csv(unique_counts, file= "Output_data/5b.csv", row.names= F)

# Calculate STL/spp score
unique_counts <- mutate(unique_counts, STLspp = STL / species_count)

ggplot(unique_counts, aes(x = fct_rev(site), y = STLspp)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, fill="#7f7f7f") +
  labs(x = "Site", y = "STL/spp") +
  scale_y_continuous(
    breaks = seq(0, 10, by = 2),
    labels = seq(0, 10, by = 2)) +
  my_theme +coord_flip()
# barplot_5d
write.csv(unique_counts, file= "Output_data/5d.csv", row.names= F)

####

# repeat for OBIS/GBIF vs eDNA

# Subset 1: source_dna=TRUE
subset_true <- sampled_sites2[sampled_sites2$source_dna == TRUE, ]
unique_counts_true <- subset_true %>%
  group_by(site) %>%
  summarise(
    class_count = n_distinct(ifelse(!is.na(class) & class != "", class, NA)),
    order_count = n_distinct(ifelse(!is.na(order) & order != "", order, NA)),
    family_count = n_distinct(ifelse(!is.na(family) & family != "", family, NA)),
    genus_count = n_distinct(ifelse(!is.na(genus) & genus != "", genus, NA)),
    species_count = n_distinct(ifelse(!is.na(species) & species != "", species, NA))
  )
# Calculate STL score
unique_counts_true <- mutate(unique_counts_true, STL = (class_count * 5) + (order_count * 4) + (family_count * 3) + (genus_count * 2))
# Calculate STL/spp score
unique_counts_true <- mutate(unique_counts_true, STLspp = STL / species_count)

# Subset 2: source_dna=FALSE
subset_false <- sampled_sites2[sampled_sites2$source_dna == FALSE, ]
unique_counts_false <- subset_false %>%
  group_by(site) %>%
  summarise(
    class_count = n_distinct(ifelse(!is.na(class) & class != "", class, NA)),
    order_count = n_distinct(ifelse(!is.na(order) & order != "", order, NA)),
    family_count = n_distinct(ifelse(!is.na(family) & family != "", family, NA)),
    genus_count = n_distinct(ifelse(!is.na(genus) & genus != "", genus, NA)),
    species_count = n_distinct(ifelse(!is.na(species) & species != "", species, NA))
  )
# Calculate STL score
unique_counts_false <- mutate(unique_counts_false, STL = (class_count * 5) + (order_count * 4) + (family_count * 3) + (genus_count * 2))
# Calculate STL/spp score
unique_counts_false <- mutate(unique_counts_false, STLspp = STL / species_count)

unique_counts_false$STL <- -unique_counts_false$STL  # make STL negative
unique_counts_false$STLspp <- -unique_counts_false$STLspp # make STL/spp negative

# Combine the subsets
combined_species_counts <- rbind(unique_counts_true, unique_counts_false)

ggplot(combined_species_counts, aes(x = site, y = STL)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, fill="maroon3") +
  labs(x = "Site",
       y = "STL") +
  my_theme +coord_flip()+geom_hline(yintercept = 0, color = 1, lwd = 1)
#barplot

ggplot(combined_species_counts, aes(x = site, y = STLspp)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, fill="maroon3") +
  labs(x = "Site",
       y = "STL") +
  my_theme +coord_flip()+geom_hline(yintercept = 0, color = 1, lwd = 1)
#barplot

### ----------------------------------

### Functional diversity
### ----------------------------------

setwd("C:/Users/ana/OneDrive/Desktop/CCMAR/eDNA")
fishes <- read.csv("fishes.csv", stringsAsFactors=T)

length(unique(fishes$species)) # 7109 spp
library(rfishbase)

fish_species <- unique(fishes$species)

# Retrieve trait info from FishBase and check which traits have info for more than 50 % of the spp

.5*7109 # 3554.5 = 3555 spp with data as criteria to keep a trait

a <- species(fish_species) # 7058
write.csv(a, file = "FunctionalDiversity/species.csv", row.names = F)
a$Species[duplicated(a$Species)] #no
non_na_counts <- colSums(!is.na(a))
selected_columns <- names(non_na_counts[non_na_counts >= 3555])

b <- reproduction(fish_species) # 4574
write.csv(b, file = "FunctionalDiversity/reproduction.csv", row.names = F)
b$Species[duplicated(b$Species)] #no
non_na_counts <- colSums(!is.na(b))
selected_columns <- names(non_na_counts[non_na_counts >= 3555])

c <- ecology(fish_species) # 4574
write.csv(c, file = "FunctionalDiversity/ecology.csv", row.names = F)
c$Species[duplicated(c$Species)] #yes
non_na_counts <- colSums(!is.na(c))
selected_columns <- names(non_na_counts[non_na_counts >= 3555])

#---

# get the site x spp presence-absence matrix

setwd("C:/Users/ana/OneDrive/Desktop/CCMAR/eDNA")
fishes <- read.csv("fishes.csv", stringsAsFactors=T)

unique_species <- unique(fishes$species)
unique_sites <- unique(fishes$site)

unique_species <- sort(unique_species)

presence_absence_matrix <- matrix(0, nrow = length(unique_sites), ncol = length(unique_species),
                                  dimnames = list(unique_sites, unique_species))

for (i in 1:nrow(fishes)) {
  row_index <- which(unique_sites == fishes[i, "site"])
  col_index <- which(unique_species == fishes[i, "species"])
  presence_absence_matrix[row_index, col_index] <- 1
}

#---

traits <- read.csv("FunctionalDiversity/funcdiversity.csv", stringsAsFactors=T, sep="\t", row.names = 1) # spp x traits matrix

# 1 option - remove spp with NAs for any trait
rows_with_any_na <- row.names(traits)[rowSums(is.na(traits)) > 0]
length(rows_with_any_na) # 4309 spp with any NA value in FishBase
traits_clean <- traits[!row.names(traits) %in% rows_with_any_na, ] # remove them
str(traits_clean) # 2,800 spp
presence_absence_matrix_clean <- presence_absence_matrix[, !colnames(presence_absence_matrix) %in% rows_with_any_na] # remove them also from the site x spp matrix
ncol(presence_absence_matrix_clean) # check, it has to be equal to traits

summary(traits_clean) # there are some details that need to be fixed
traits_clean$BodyShapeI <- as.factor(gsub("other\\(seeremarks\\)", "other", traits_clean$BodyShapeI))
traits_clean$BodyShapeI <- as.factor(gsub("Elongated", "elongated", traits_clean$BodyShapeI))
unique(traits_clean$BodyShapeI)
unique(traits_clean$Environment)
unique(traits_clean$Habitat)
unique(traits_clean$ReproMode)
unique(traits_clean$Fertilization)
unique(traits_clean$RepGuild1)

duplicated(traits_clean) # duplicate rows causes the error: In is.euclid(x.dist) : Zero distance(s)
traits_clean2 <- unique(traits_clean) # delete duplicates
str(traits_clean2) # 2,762 spp
removed_species <- setdiff(rownames(traits_clean), rownames(traits_clean2)) # delete these spp also from the matrix
presence_absence_matrix_clean2 <- presence_absence_matrix_clean[, !colnames(presence_absence_matrix_clean) %in% removed_species]
ncol(presence_absence_matrix_clean2) # check with traits

library(FD)
dist_matrix <- gowdis(traits_clean2)

# Perform PCoA on the distance matrix
pcoa_result <- pcoa(dist_matrix)
eigenvalues <- pcoa_result$values$Eigenvalues
variance_explained <- eigenvalues / sum(eigenvalues)
sum(variance_explained[1:4]) # 0.93
eigenvectors <- pcoa_result$vectors[, 1:4] # get the 4 first eigenvectors

# Get the functional diversity metrics
result <- dbFD(x=eigenvectors, a=presence_absence_matrix_clean2)
fric<-as.data.frame(result$FRic)
feve<-as.data.frame(result$FEve)
fdiv<-as.data.frame(result$FDiv)

write.csv(fric, file= "Output_data/6_fric.csv")
write.csv(feve, file= "Output_data/6_feve.csv")
write.csv(fdiv, file= "Output_data/6_fdiv.csv")

# plot
# order the sites latitudinally
ordered_sites <- c(
  "wadden_sea",
  "gulf_of_porto_calanche_of_piana_gulf_of_girolata_scandola_reserve",
  "everglades_national_park",
  "the_sundarbans",
  "banc_d_arguin_national_park",
  "archipielago_de_revillagigedo",
  "belize_barrier_reef_reserve_system",
  "socotra_archipelago",
  "tubbataha_reefs_natural_park",
  "coiba_national_park_and_its_special_zone_of_marine_protection",
  "cocos_island_national_park",
  "brazilian_atlantic_islands_fernando_de_noronha_and_atol_das_rocas_reserves",
  "aldabra_atoll",
  "lagoons_of_new_caledonia_reef_diversity_and_associated_ecosystems",
  "ningaloo_coast",
  "shark_bay_western_australia",
  "isimangaliso_wetland_park",
  "lord_howe_island_group",
  "peninsula_valdes",
  "french_austral_lands_and_seas")

combined_data <- data.frame(site = rownames(fric), fric = fric[,1], feve = feve[,1], fdiv = fdiv[,1])
ordered_data <- combined_data[match(ordered_sites, combined_data$site), ]

ordered_data$site <- factor(
  ordered_data$site,
  levels = ordered_sites,
  labels = c(
    "Wadden Sea",
    "Gulf of Porto Calanche of Piana, Gulf of Girolata, Scandola Reserve",
    "Everglades National Park",
    "The Sundarbans",
    "Banc d'Arguin National Park",
    "Archipiélago de Revillagigedo",
    "Belize Barrier Reef Reserve System",
    "Socotra Archipelago",
    "Tubbataha Reefs Natural Park",
    "Coiba National Park and its Special Zone of Marine Protection",
    "Cocos Island National Park",
    "Brazilian Atlantic Islands: Fernando de Noronha and Atol das Rocas Reserves",
    "Aldabra Atoll",
    "Lagoons of New Caledonia: Reef Diversity and Associated Ecosystems",
    "Ningaloo Coast",
    "Shark Bay, Western Australia",
    "iSimangaliso Wetland Park",
    "Lord Howe Island Group",
    "Peninsula Valdés",
    "French Austral Lands and Seas"))

# first option - the three metrics together

library(tidyr)
ordered_data_long <- ordered_data %>%
  pivot_longer(cols = c(fric, feve, fdiv),
               names_to = "Metric",
               values_to = "Value")

my_theme=theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),axis.line.x = element_line(linewidth = 1),axis.line.y = element_line(linewidth = 1),axis.title = element_text(size = 12),axis.text = element_text(size = 12))
library(forcats)
ggplot(ordered_data_long, aes(x = fct_rev(site), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Site", y = "Functional diversity") +
  scale_y_continuous(
    breaks = seq(0, 200, by = 50),
    labels = seq(0, 200, by = 50)) +
  my_theme +
  coord_flip()

# alternative to visualize fric and feve/fdiv separately

my_theme=theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),axis.line.x = element_line(linewidth = 1),axis.line.y = element_line(linewidth = 1),axis.title = element_text(size = 12),axis.text = element_text(size = 12))
ggplot(ordered_data, aes(x = fct_rev(site), y = fric)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Site", y = "Functional richness") +
  scale_y_continuous(
    breaks = seq(0, 200, by = 50),
    labels = seq(0, 200, by = 50)) +
  my_theme +
  coord_flip()
# barplot_7a  (1200x500)

ggplot(ordered_data, aes(x = fct_rev(site), y = feve)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Site", y = "Functional eveness") +
  scale_y_continuous(
    breaks = seq(0, 1, by = .2),
    labels = seq(0, 1, by = .2)) +
  my_theme +
  coord_flip()
# barplot_7b  (1200x500)

ggplot(ordered_data, aes(x = fct_rev(site), y = fdiv)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Site", y = "Functional divergence") +
  scale_y_continuous(
    breaks = seq(0, 1, by = .2),
    labels = seq(0, 1, by = .2)) +
  my_theme +
  coord_flip()
# barplot_7c  (1200x500)

library(tidyr)
ordered_data_long <- ordered_data %>%
  pivot_longer(cols = c(feve, fdiv),
               names_to = "Metric",
               values_to = "Value")

library(forcats)
ggplot(ordered_data_long, aes(x = fct_rev(site), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Site", y = "Functional diversity") +
  scale_y_continuous(
    breaks = seq(0, 1, by = .2),
    labels = seq(0, 1, by = .2)) +
  my_theme +
  coord_flip()
# barplot_7bc  (1200x500)

# second option - scale the variables to visualize them because fric is in a much larger scale

library(caret)
process <- preProcess(ordered_data[,c("fric", "feve", "fdiv")], method = c("range"))
scaled_data <- predict(process, ordered_data[,c("fric", "feve", "fdiv")])
colnames(scaled_data) <- c("scaled_fric", "scaled_feve", "scaled_fdiv")

ordered_data_long <- cbind(ordered_data$site,stack(scaled_data))
colnames(ordered_data_long) <- c("site", "value", "metric")


metric_order <- c("scaled_fdiv", "scaled_feve", "scaled_fric")
ordered_data_long$metric <- factor(ordered_data_long$metric, levels = metric_order)

my_colors <- c("#e377c2", "#17becf", "#2ca02c")
ggplot(ordered_data_long, aes(x = fct_rev(site), y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Site", y = "Functional diversity") +
  scale_y_continuous(
    breaks = seq(0, 1, by = .2),
    labels = seq(0, 1, by = .2)) +
  scale_fill_manual(values = my_colors) + my_theme + guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip()
# barplot_7  (1200x500)

### ----------------------------------

### Biogeographic realms
### ----------------------------------

# sqlite database Pieter
library(RSQLite)
setwd("C:/Users/anavc/OneDrive/Desktop/CCMAR/eDNA")
con <- dbConnect(SQLite(), dbname = "database.sqlite")

dbListTables(con) # list of tables in the sqlite

# extract sampled sites
mwhs_cells <- dbReadTable(con, "mwhs_cells")
head(mwhs_cells)
unique(mwhs_cells$site_id)
sampled_sites <- mwhs_cells[mwhs_cells$site_id %in% c(
  "wadden_sea",
  "gulf_of_porto_calanche_of_piana_gulf_of_girolata_scandola_reserve",
  "everglades_national_park",
  "the_sundarbans",
  "banc_d_arguin_national_park",
  "archipielago_de_revillagigedo",
  "belize_barrier_reef_reserve_system",
  "socotra_archipelago",
  "tubbataha_reefs_natural_park",
  "coiba_national_park_and_its_special_zone_of_marine_protection",
  "cocos_island_national_park",
  "brazilian_atlantic_islands_fernando_de_noronha_and_atol_das_rocas_reserves",
  "aldabra_atoll",
  "lagoons_of_new_caledonia_reef_diversity_and_associated_ecosystems",
  "ningaloo_coast",
  "shark_bay_western_australia",
  "isimangaliso_wetland_park",
  "lord_howe_island_group",
  "peninsula_valdes",
  "french_austral_lands_and_seas"), ]
unique(sampled_sites$site_id)

# extract sampled realms
realms_cells <- dbReadTable(con, "realms_cells")
head(realms_cells)
length(unique(realms_cells$realm)) # 29
merged_data <- merge(realms_cells, sampled_sites, by = "h3", all.x = TRUE)
unique(merged_data$realm)

realm_with_sites <- merged_data %>%
  group_by(realm) %>%
  filter(any(!is.na(site_id)))
merged_data2 <- merged_data[merged_data$realm %in% realm_with_sites$realm, ]
unique(merged_data2$realm)

# extract species on sampled realms and sites
occurrence <- dbReadTable(con, "occurrence")
merged_occurrence <- merge(occurrence, merged_data2[, c("h3", "site_id", "realm")], by = "h3", all.x = TRUE)
merged_occurrence2 <- merged_occurrence %>% filter(!is.na(realm))
unique(merged_occurrence2$site_id) # NAs for occurrences out of sites
unique(merged_occurrence2$realm)
length(unique(merged_occurrence2$species)) # 145,945 spp

# 1st percentage
total_species_per_realm <- merged_occurrence2 %>%
  group_by(realm) %>%
  summarize(total_species = n_distinct(species))
total_species_per_realm

site_species_percentage <- merged_occurrence2 %>%
  group_by(site_id, realm) %>%
  summarize(unique_species = n_distinct(species)) %>%
  left_join(total_species_per_realm, by = "realm") %>%
  mutate(percentage_of_realm_species = unique_species / total_species * 100)
print(site_species_percentage,n=32)

# 2nd percentage
sum(is.na(merged_occurrence2$site_id))
sum(is.na(merged_occurrence2$realm))

valid_sites <- merged_occurrence2 %>%
  filter(!is.na(site_id))
head(valid_sites)

unique_species_per_realm <- valid_sites %>%
  group_by(realm) %>%
  summarize(unique_species = n_distinct(species))
unique_species_per_realm

sites_per_realm <- valid_sites %>%
  group_by(realm, site_id) %>%
  summarize() %>%
  summarize(sites = n())
sites_per_realm

total_unique_species_per_realm <- merged_occurrence2 %>%
  group_by(realm) %>%
  summarize(total_unique_species = n_distinct(species))
total_unique_species_per_realm
length(unique(merged_occurrence2$species[merged_occurrence2$realm==11]))
length(unique(merged_occurrence2$species[merged_occurrence2$realm==12]))

combined_info <- valid_sites %>%
  group_by(realm, site_id) %>%
  summarize() %>%
  summarize(sites = n()) %>%
  left_join(total_unique_species_per_realm, by = "realm") %>%
  left_join(unique_species_per_realm, by = "realm") %>%
  mutate(percentage_valid_sites = (unique_species / total_unique_species) * 100)
combined_info

### ----------------------------------

### Similarities between WH sites
### ----------------------------------

setwd("C:/Users/anavc/OneDrive/Desktop/CCMAR/eDNA")
sampled_sites4 <- read.csv("sampled_sites4.csv", stringsAsFactors=T)

# order the sites latitudinally
ordered_sites <- c(
  "wadden_sea",
  "gulf_of_porto_calanche_of_piana_gulf_of_girolata_scandola_reserve",
  "everglades_national_park",
  "the_sundarbans",
  "banc_d_arguin_national_park",
  "archipielago_de_revillagigedo",
  "belize_barrier_reef_reserve_system",
  "socotra_archipelago",
  "tubbataha_reefs_natural_park",
  "coiba_national_park_and_its_special_zone_of_marine_protection",
  "cocos_island_national_park",
  "brazilian_atlantic_islands_fernando_de_noronha_and_atol_das_rocas_reserves",
  "aldabra_atoll",
  "lagoons_of_new_caledonia_reef_diversity_and_associated_ecosystems",
  "ningaloo_coast",
  "shark_bay_western_australia",
  "isimangaliso_wetland_park",
  "lord_howe_island_group",
  "peninsula_valdes",
  "french_austral_lands_and_seas")

sampled_sites4$site <- factor(
  sampled_sites4$site,
  levels = ordered_sites,
  labels = c(
    "WS",
    "GPCP,GG,SR",
    "ENP",
    "TS",
    "Bd'ANP",
    "AR",
    "BBRRS",
    "SA",
    "TRNP",
    "CNPSZMP",
    "CINP",
    "BAI:FNARR",
    "AA",
    "LNC:RDAE",
    "NC",
    "SBB,WA",
    "iSWP",
    "LHIG",
    "PV",
    "FALS"))

# presence-absence matrix
unique_species <- unique(sampled_sites4$species)
unique_sites <- unique(sampled_sites4$site)

unique_species <- sort(unique_species)

presence_absence_matrix <- matrix(0, nrow = length(unique_sites), ncol = length(unique_species),
                                  dimnames = list(unique_sites, unique_species))

for (i in 1:nrow(sampled_sites4)) {
  row_index <- which(unique_sites == sampled_sites4[i, "site"])
  col_index <- which(unique_species == sampled_sites4[i, "species"])
  presence_absence_matrix[row_index, col_index] <- 1
}

# alternative
library(reshape2)
presence_absence_matrix <- dcast(data = sampled_sites4, site ~ species, value.var = "records", fun.aggregate = function(x) { ifelse(sum(x) > 0, 1, 0) })

write.csv(presence_absence_matrix, file = "presence_absence_matrix.csv", row.names = F)


setwd("C:/Users/anavc/OneDrive/Desktop/CCMAR/eDNA")
presence_absence_matrix <- read.csv("presence_absence_matrix.csv", stringsAsFactors=T)

# 1 - Pairwise Jaccard Similarities between Sites
jaccard_similarities <- dist(as.matrix(presence_absence_matrix), method = "binary")
jaccard_similarities <- 1 - as.matrix(jaccard_similarities) # Convert the distances to similarity matrix
jaccard_similarities
jaccard_similarities[lower.tri(jaccard_similarities)] <- NA
heatmap(jaccard_similarities, 
        Rowv = NULL, Colv = NULL, 
        col = colorRampPalette(c("white", "red"))(100),
        scale = "none")
# alternative
jaccard_similarities <- dist(as.matrix(presence_absence_matrix), method = "binary")
jaccard_similarities <- 1 - as.matrix(jaccard_similarities) # Convert the distances to similarity matrix
library(ggplot2)
library(tidyr)
site_names <- rownames(jaccard_similarities)
heatmap_data <- as.data.frame(as.table(jaccard_similarities))
colnames(heatmap_data) <- c("row", "col", "similarity")
heatmap_data$row <- site_names[heatmap_data$row]
heatmap_data$col <- site_names[heatmap_data$col]
heatmap_data <- heatmap_data[heatmap_data$row <= heatmap_data$col, ]
my_theme=theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),axis.line.x = element_line(linewidth = 1),axis.line.y = element_line(linewidth = 1),axis.title = element_text(size = 12),axis.text = element_text(size = 12))
ggplot(heatmap_data, aes(x = col, y = row, fill = similarity)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") + scale_y_discrete(position = "right") +
  my_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# 2 - nMDS
library(vegan)
nmds1 <-metaMDS(presence_absence_matrix, distance = "jaccard", k = 2, maxit = 999,trymax = 500, wascores = T)
nmds1
#Generally, stress < 0.05 provides an excellent represention in reduced dimensions, < 0.1 is great, < 0.2 is good, and stress > 0.3 provides a poor representation

#Shepard test/goodness of fit
goodness(nmds1) # Produces a results of test statistics for goodness of fit for each point
par(mar=c(5,5,1,2))
stressplot(nmds1) # Produces a Shepard diagram
#Shepard plot, which shows scatter around the regression between the interpoint distances in the final configuration (i.e., the distances between each pair of communities) against their original dissimilarities.
#Large scatter around the line suggests that original dissimilarities are not well preserved in the reduced number of dimensions.
plot(nmds1)

library(ggplot2)
site_coordinates <- as.data.frame(scores(nmds1, display = "sites")) # extract NMDS scores (x and y coordinates)
site_coordinates$site <- rownames(site_coordinates)
my_theme=theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),axis.line.x = element_line(linewidth = 1),axis.line.y = element_line(linewidth = 1),axis.title = element_text(size = 12),axis.text = element_text(size = 12))
p <- ggplot(site_coordinates, aes(x = NMDS1, y = NMDS2)) +
  geom_point() +
  geom_text(aes(label = site), vjust = -0.5) +
  labs(x = "NMDS1", y = "NMDS2") +  my_theme
p + theme(plot.margin = margin(1, 1, 1, 1, "cm"))

# 3 - other alternatives, e.g. pvclust

### ----------------------------------

### B-diversity
### ----------------------------------

setwd("C:/Users/anavc/OneDrive/Desktop/CCMAR/eDNA")
presence_absence_matrix <- read.csv("presence_absence_matrix.csv", stringsAsFactors=T)

library(vegan)
a<-nestedbetasor(presence_absence_matrix)
a
oecosimu(presence_absence_matrix, nestedbetasor, "quasiswap", nsimul = 10)

# Calculate beta diversity for each pair of sites
beta_diversity <- beta.pair(presence_absence_matrix, index.family="sorensen")
beta_diversity

library(gplots)
# Heatmap - Sorensen pair-wise dissimilarity
beta_sor_matrix <- as.matrix(beta_diversity$beta.sor)
beta_sor_matrix[lower.tri(beta_sor_matrix)] <- NA
heatmap(beta_sor_matrix, 
        Rowv = NA, Colv = NA, 
        col = colorRampPalette(c("white", "red"))(100),
        scale = "none")

# Heatmap - nestedness
beta_sne_matrix <- as.matrix(beta_diversity$beta.sne)
beta_sne_matrix[lower.tri(beta_sne_matrix)] <- NA
heatmap(beta_sne_matrix, 
        Rowv = NA, Colv = NA, 
        col = colorRampPalette(c("white", "red"))(100),
        scale = "none")

# Heatmap - turnover
beta_sim_matrix <- as.matrix(beta_diversity$beta.sim)
beta_sim_matrix[lower.tri(beta_sim_matrix)] <- NA
heatmap(beta_sim_matrix, 
        Rowv = NA, Colv = NA, 
        col = colorRampPalette(c("white", "red"))(100),
        scale = "none")

# alternative
library(ggplot2)
library(gridExtra)
library(tidyr)

lower_triangular <- function(mat) {
  mat[upper.tri(mat)] <- NA
  return(mat)
}

beta_diversity <- beta.pair(presence_absence_matrix, index.family = "sorensen")

# Extract dissimilarity matrices
beta_sor_matrix <- as.matrix(beta_diversity$beta.sor)
beta_sne_matrix <- as.matrix(beta_diversity$beta.sne)
beta_sim_matrix <- as.matrix(beta_diversity$beta.sim)

# Convert dissimilarity matrices to long format for ggplot
heatmap_data_sor <- as.data.frame(as.table(beta_sor_matrix))
heatmap_data_sne <- as.data.frame(as.table(beta_sne_matrix))
heatmap_data_sim <- as.data.frame(as.table(beta_sim_matrix))

# Set column names
colnames(heatmap_data_sor) <- c("row", "col", "dissimilarity")
colnames(heatmap_data_sne) <- c("row", "col", "dissimilarity")
colnames(heatmap_data_sim) <- c("row", "col", "dissimilarity")

# Set row and column labels
site_names <- rownames(beta_sor_matrix)
heatmap_data_sor$row <- site_names[heatmap_data_sor$row]
heatmap_data_sor$col <- site_names[heatmap_data_sor$col]
heatmap_data_sne$row <- site_names[heatmap_data_sne$row]
heatmap_data_sne$col <- site_names[heatmap_data_sne$col]
heatmap_data_sim$row <- site_names[heatmap_data_sim$row]
heatmap_data_sim$col <- site_names[heatmap_data_sim$col]

# Filter out upper triangle values
heatmap_data_sor <- heatmap_data_sor[heatmap_data_sor$row <= heatmap_data_sor$col, ]
heatmap_data_sne <- heatmap_data_sne[heatmap_data_sne$row <= heatmap_data_sne$col, ]
heatmap_data_sim <- heatmap_data_sim[heatmap_data_sim$row <= heatmap_data_sim$col, ]

color_limits <- range(0, 1)
my_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(linewidth = 1),
        axis.line.y = element_line(linewidth = 1),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

plot_sor <- ggplot(heatmap_data_sor, aes(x = col, y = row, fill = dissimilarity)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", limits = color_limits) +
  scale_y_discrete(position = "right") +
  my_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Sorensen Dissimilarity")

plot_sne <- ggplot(heatmap_data_sne, aes(x = col, y = row, fill = dissimilarity)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", limits = color_limits) +
  scale_y_discrete(position = "right") +
  my_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Nestedness")

plot_sim <- ggplot(heatmap_data_sim, aes(x = col, y = row, fill = dissimilarity)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", limits = color_limits) +
  scale_y_discrete(position = "right") +
  my_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Turnover")

grid.arrange(plot_sor, plot_sne, plot_sim, ncol = 3)

### ----------------------------------
