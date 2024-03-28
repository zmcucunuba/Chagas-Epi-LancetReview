

##### DTUs map
rm(list=ls())
library(ggplot2)
library(rnaturalearth)
library(sf)
library(dplyr)
library(sivirep)


dat <- read.csv2("data/dtus/Base_review_modif_2 (2).csv")
dat <- sivirep::limpiar_encabezado(dat)
dat <- dat %>% mutate(lat = as.numeric(latitud), lon = as.numeric(longitud))
dtus_of_interest <- c("TcI", "TcII", "TcIII", "TcIV", "TcV", "TcVI", "Tcbat")
dat <- dat %>% filter(dtu %in% dtus_of_interest)
dat$dtus <- factor(dat$dtu, levels = dtus_of_interest)
dat$tcbat <- if_else(dat$dtu == "Tcbat", "Tcbat", NA)

humans  <- dat %>% filter(source_sample == "Humans")
animals <- dat %>% filter(source_sample == "Reservoir")
vectors <- dat %>% filter(source_sample == "Vector" )


world <- ne_countries(scale = "medium", returnclass = "sf")
americas <- world %>% dplyr::filter(region_un == "Americas")

standard_size <- 2
base_map <- ggplot(data = americas) +
  geom_sf(fill = "lightgrey", color = "white", size = 3) +
  theme_minimal(standard_size * 10) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(x = "", y = "") +
  coord_sf(xlim = c(-120, -35), ylim = c(-50, 50))


colors_plot <- c("#e41a1c", "#377eb8", "#4daf4a",
                 "#984ea3", "pink", "#ffff33", "black")

dtu_vectors_map <-
  base_map +
  geom_point(data = vectors,
             aes(x = lon/10, y = lat/10, fill = dtus, pch = dtus),
             size = standard_size * 1.5, alpha = 1, pch = 21, colour = "black") +
  labs (fill = "DTU") +
  scale_fill_manual(values = colors_plot) +
  scale_shape_manual(values = c(rep(21, 6), 18)) +
  theme(legend.position = "none")

dtu_animals_map <-
  base_map +
  geom_point(data = animals,
             aes(x = lon/10, y = lat/10, fill = dtus, pch = dtus),
             size = standard_size * 1.5, alpha = 1, pch = 21, colour = "black") +
  labs (fill = "DTU") +
  scale_fill_manual(values = colors_plot) +
  scale_shape_manual(values = c(rep(21, 6), 18)) +
  theme(legend.position = "none")

dtu_humans_map <-
  base_map +
  geom_point(data = humans,
             aes(x = lon/10, y = lat/10, fill = dtus, pch = dtus),
             size = standard_size * 1.5, alpha = 1, pch = 21, colour = "black") +
  labs (fill = "DTU") +
  scale_fill_manual(values = colors_plot) +
  scale_shape_manual(values = c(rep(21, 6), 18))

pp <- cowplot::plot_grid(dtu_vectors_map, dtu_animals_map, dtu_humans_map,
                         nrow = 1, labels = c("A. Vectors", "B. Animals", "C. Humans"),
                         rel_widths = c(1, 1, 1.14),
                         label_size = standard_size * 20)



png(filename = "figs/Fig 2.png", width = 1200 * 2, height = 480 * 2)
pp
dev.off()


