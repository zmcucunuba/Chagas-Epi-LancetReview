

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
dat$source_sample <- recode(dat$source_sample,
                            Reservoir = "Non-Human Animals",
                            Vector = "Insect Vectors")

species<- c( "Insect Vectors", "Non-Human Animals", "Humans")
dat0 <- dat %>% filter(source_sample %in% species)
dat0$source_sample <- factor(dat0$source_sample, levels = species)
dat0$subtitle <- recode(dat0$source_sample,
                        "Humans" = "C. Humans",
                        "Non-Human Animals" =  "B. Non-Human Animals",
                        "Insect Vectors" = "A. Insect Vectors")

world <- ne_countries(scale = "medium", returnclass = "sf")
americas <- world %>% dplyr::filter(region_un == "Americas")

standard_size <- 5
base_map <- ggplot(data = americas) +
  geom_sf(fill = "grey95", color = "black", size = 3) +
  theme_minimal(standard_size * 10) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(x = "", y = "") +
  coord_sf(xlim = c(-120, -35), ylim = c(-50, 50))



colors_plot <- c(c("#b2182b", "#377eb8", "#4daf4a",
                   ,"#fa9fb5", "#984ea3","black"))



pp <-
  base_map +
  geom_jitter(data = dat0,
              aes(x = lon/10, y = lat/10, fill = subtitle, shape = subtitle),
              size = standard_size * 2, colour = "black", pch = 21) +
  scale_fill_manual(values = colors_plot) +
  labs (fill = "xx", shape = "xx", alpha = "xx") +
  theme(legend.position = "right") +
  facet_wrap(vars(dtus), nrow = 2) +
  theme(panel.spacing.x = unit(0.0001, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(legend.key.size = unit(1, "cm"),
        legend.text = element_text(margin = margin(t = 0, r = 0, b = 0, l = -10)),
        strip.text = element_text(size = standard_size * 10, hjust = 0)) +
  guides(fill = guide_legend(nrow = 1, override.aes = list(size = standard_size * 4)))


png(filename = "figs/Fig 2_x.png", width = 900 * 4, height = 480 * 4)
pp
dev.off()


