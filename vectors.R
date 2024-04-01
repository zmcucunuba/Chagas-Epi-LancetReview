rm(list=ls())
library(readxl)

rpro <- read_csv("data/BD Dist Triatom/R_prolixus.csv")
rpall <- read_csv("data/BD Dist Triatom/R_pallescens.csv")
pgen <- read_csv("data/BD Dist Triatom/P_geniculatus.csv")
tmac <- read_csv("data/BD Dist Triatom/Final_Listo T. maculata.csv")
vec <- read_excel("data/BD Dist Triatom/Envío_5sp_Triatominos_Ceccarelli.xlsx")

table(vec$scientificName)

world <- ne_countries(scale = "medium", returnclass = "sf")
americas <- world %>% dplyr::filter(region_un == "Americas")

standard_size <- 3
base_map <- ggplot(data = americas) +
  geom_sf(fill = "lightgrey", color = "white", size = 3) +
  theme_minimal(standard_size * 5) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(x = "", y = "") +
  coord_sf(xlim = c(-120, -35), ylim = c(-50, 50))


p1 <-
  base_map +
  geom_point(data = rpro,
             aes(x = longitud, y = latitud, fill = especie),
             size = standard_size * 1.5, alpha = 1, pch = 21, colour = "black")

p2 <-
  base_map +
  geom_point(data = rpall,
             aes(x = longitud, y = latitud, fill = especie),
             size = standard_size * 1.5, alpha = 1, pch = 21, colour = "black")

p3 <- base_map +
  geom_point(data = pgen,
             aes(x = longitud, y = latitud, fill = especie),
             size = standard_size * 1.5, alpha = 1, pch = 21, colour = "black")

p4 <- base_map +
  geom_point(data = tmac,
             aes(x = longitud, y = latitud, fill = nombre),
             size = standard_size * 1.5, alpha = 1, pch = 21, colour = "black")



p5 <- base_map +
  geom_point(data = vec,
             aes(x = decimalLongitude, y = decimalLatitude, fill = scientificName),
             size = standard_size * 1.5, alpha = 1, pch = 21, colour = "black")


p_all <- cowplot::plot_grid(p1, p2, p3, p4, p5, nrow = 2)

png(filename = "figs/Fig 2.png", width = 480 * 3.5, height = 480 * 1.5)
p_all
dev.off()
