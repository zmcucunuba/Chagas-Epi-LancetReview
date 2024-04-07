##################################### Dataset 2022 ############################

library(readxl)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(dplyr)
library(rnaturalearthdata)

rm(list=ls())

dat1 <- read.delim("data/vectors/Vectors in the Americas 2022.csv")
dat2 <- read.delim("data/vectors/Verctors in Argentina 2022.txt")


#Dataframes

dat1f <- dat1 %>%
  select(gbifID, verbatimScientificName, decimalLongitude, decimalLatitude, countryCode) %>%
  rename(id=gbifID,
         scientificname=verbatimScientificName,
         longitude=decimalLongitude,
         latitude=decimalLatitude)

dat2f <- dat2 %>%
  select(id, scientificName, decimalLongitude, decimalLatitude, countryCode) %>%
  rename(id=id,
         scientificname=scientificName,
         longitude=decimalLongitude,
         latitude=decimalLatitude)

dat <- rbind(dat1f, dat2f)
errors <- dat %>% filter(scientificname == "Rhodnius prolixus",
                         countryCode == "BR")
dat <- dat %>% filter(!id %in% errors$id)


main_dom <- dat %>%
  filter(scientificname %in% c(
    "Triatoma infestans", "Triatoma dimidiata", "Rhodnius prolixus"))

main_rep <- dat %>%
  filter(scientificname %in% c(
    "Triatoma gerstaeckeri",
    "Panstrongylus megistus",
    "Triatoma sordida",
    "Triatoma pseudomaculata",
    "Triatoma sanguisuga",
    "Triatoma brasiliensis",
    "Panstrongylus geniculatus"))

#Maps

world <- ne_countries(scale = "medium", returnclass = "sf")
americas <- world %>% dplyr::filter(region_un == "Americas")

standard_size <- 3
base_map <- ggplot(data = americas) +
  geom_sf(fill = "grey95", color = "black", size = 3) +
  theme_minimal(standard_size * 10) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(x = "", y = "") +
  coord_sf(xlim = c(-120, -35), ylim = c(-50, 50)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


main_dommap <-
  base_map +
  geom_point(data = main_dom,
             aes(x = longitude, y = latitude, fill = scientificname),
             size = standard_size * 0.7,
             alpha = 0.8, pch = 21, colour = "black") +
  labs (fill = "") +
  theme(legend.text = element_text(face = "italic")) +
  theme(legend.position = c(0.25, 0.3)) +
  ggtitle("A. Main domiciliated vectors") +
  scale_fill_manual(values = c("Triatoma infestans"="#fee090",
                               "Triatoma dimidiata"="#377eb8",
                               "Rhodnius prolixus"="#4daf4a")) +
  guides(fill = guide_legend(override.aes = list(size = standard_size * 2))) +
  theme(legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(margin = margin(t = 0, r = 0, b = 0, l = -10)),
        strip.text = element_text(size = standard_size * 30, hjust = 0))



main_repmap <-
  base_map +
  geom_point(data = main_rep,
             aes(x = longitude, y = latitude, fill = scientificname),
             size = standard_size * 0.7, alpha = 0.8, pch = 21, colour = "black") +
  labs (fill = "") +
  theme(legend.text = element_text(face = "italic")) +
  theme(
    legend.position = c(0.25, 0.3)) +
  ggtitle("B. Other most reported native vectors") +
  scale_fill_manual(values = c("Triatoma gerstaeckeri"="#FFFF32",
                               "Panstrongylus geniculatus"="#FF5500",
                               "Triatoma sanguisuga"="#b2182b",
                               "Triatoma brasiliensis"="black",
                               "Triatoma sordida"="#AECF00",
                               "Triatoma pseudomaculata"="#984ea3",
                               "Panstrongylus megistus"="#AAF7FF")) +
  guides(fill = guide_legend(override.aes = list(size = standard_size * 2))) +
  theme(legend.key.size = unit(1.5, "cm"),
        legend.text = element_text(margin = margin(t = 0, r = 0, b = 0, l = -10)),
        strip.text.y = element_text(size = standard_size * 100, hjust = 0, face = "bold"))




# 2 panels (main domiciled and most reported)

vectorsmaps <- cowplot::plot_grid(main_dommap, main_repmap,
                                  nrow = 1)

png(filename = "figs/Fig 2.png", width = 900 * 4, height = 480 * 4)

vectorsmaps
dev.off()






################## Most reported triatomines #################################
# Triatoma infestans 7862
# Triatoma gerstaeckeri 4128
# Panstrongylus megistus 2750
# Triatoma dimidiata 2650
# Triatoma sordida 1706
# Triatoma pseudomaculata 1416
# Rhodnius prolixus 1230
# Triatoma sanguisuga 1130
# Triatoma brasiliensis 1085
# Panstrongylus geniculatus 838
