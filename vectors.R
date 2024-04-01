rm(list=ls())
library(readxl)
library(ggplot2)
install.packages("rnaturalearth")
library(rnaturalearth)
library(sf)
library(dplyr)
install.packages("rnaturalearthdata")
library(rnaturalearthdata)

#Dataframes

all_vectors <- SciDataData_CitationCeccarellietal_2018 <- read_excel("MSc Epidemiología Clínica/Chagas in the Americas - Rev/ChagasEpiReview/data/vectors/SciDataData_CitationCeccarellietal.2018.xls")

main_vectors <- SciDataData_CitationCeccarellietal_2018 %>% 
  filter(scientificName %in% c(
    "Rhodnius prolixus", "Triatoma dimidiata", "Triatoma infestans"))

most_reported_vectors <- SciDataData_CitationCeccarellietal_2018 %>% 
  filter(scientificName %in% c(
    "Triatoma infestans", "Panstrongylus megistus", "Triatoma dimidiata",
    "Triatoma pseudomaculata", "Triatoma sordida", "Rhodnius prolixus",
    "Triatoma brasiliensis", "Panstrongylus geniculatus", "Panstrongylus lutzi",
    "Triatoma mexicana"))

most_important_courajr <- SciDataData_CitationCeccarellietal_2018 %>% 
  filter(scientificName %in% c(
    "Triatoma infestans", "Panstrongylus megistus", "Triatoma dimidiata",
    "Triatoma pseudomaculata", "Triatoma sordida", "Rhodnius prolixus",
    "Triatoma brasiliensis", "Panstrongylus geniculatus", "Triatoma maculata", 
    "Rhodnius ecuadoriensis", "Rhodnius pallescens"))

#Maps

world <- ne_countries(scale = "medium", returnclass = "sf")
americas <- world %>% dplyr::filter(region_un == "Americas")

standard_size <- 3
base_map <- ggplot(data = americas) +
  geom_sf(fill = "lightgrey", color = "white", size = 3) +
  theme_minimal(standard_size * 5) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(x = "", y = "") +
  coord_sf(xlim = c(-120, -35), ylim = c(-50, 50))

# All vectors

all_vectorsmap <-
  base_map +
  geom_point(data = all_vectors,
             aes(x = decimalLongitude, y = decimalLatitude, fill = scientificName),
             size = standard_size * 0.1, alpha = 1, pch = 21, colour = "black")
all_vectorsmap

# Main vectors (Triatoma infestans, Triatoma dimidiata, Rhodnius prolixus)

main_vectorsmap <-
  base_map +
  geom_point(data = main_vectors,
             aes(x = decimalLongitude, y = decimalLatitude, fill = scientificName),
             size = standard_size * 0.7, alpha = 1, pch = 21, colour = "black") +
  labs (fill = "Vectors", shape = "Vectors", alpha = "Vectors", size = "Vectors")
main_vectorsmap

# Vectors with most data
## Triatoma infestans 3881
## Panstrongylus megistus 2726
## Triatoma dimidiata 2121
## Triatoma pseudomaculata 1393
## Triatoma sordida 1392
## Rhodnius prolixus 1215
## Triatoma brasiliensis 1033
## Panstrongylus geniculatus 814
## Panstrongylus lutzi 614
## Triatoma mexicana 584

most_reported_vectorsmap <-
  base_map +
  geom_point(data = most_reported_vectors,
             aes(x = decimalLongitude, y = decimalLatitude, fill = scientificName),
             size = standard_size * 0.7, alpha = 1, pch = 21, colour = "black") +
  labs (fill = "Vectors", shape = "Vectors", alpha = "Vectors", size = "Vectors")
most_reported_vectorsmap

# Most important vectors according to: Coura JR. The main sceneries of Chagas 
# disease transmission. The vectors, blood and oral transmissions--a 
# comprehensive review. Mem Inst Oswaldo Cruz. 2015 May;110(3):277-82. 
# doi: 10.1590/0074-0276140362. Epub 2014 Dec 2. 
# PMID: 25466622; PMCID: PMC4489464.

# Triatoma infestans, Panstrongylus megistus, Rhodnius prolixus, 
# Triatoma dimidiata, Triatoma brasiliensis, Triatoma pseudomaculata, 
# Triatoma sordida, Triatoma maculata, Panstrongylus geniculatus, 
# Rhodnius ecuadoriensis, Rhodnius pallescens

most_important_courajrmap <-
  base_map +
  geom_point(data = most_important_courajr,
             aes(x = decimalLongitude, y = decimalLatitude, fill = scientificName),
             size = standard_size * 0.5, alpha = 1, pch = 21, colour = "black") +
  labs (fill = "Vectors", shape = "Vectors", alpha = "Vectors", size = "Vectors")
most_important_courajrmap



vectorsmaps_all <- cowplot::plot_grid(main_vectorsmap,
                                     most_reported_vectorsmap,
                                     most_important_courajrmap, nrow = 2)
vectorsmaps_all


