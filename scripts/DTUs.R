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

standard_size <- 3
base_map <- ggplot(data = americas) +
  geom_sf(fill = "grey95", color = "black", size = 3) +
  theme_minimal(standard_size * 10) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(x = "", y = "") +
  coord_sf(xlim = c(-118, -32), ylim = c(-50, 50))



colors_plot <- c(c("#fee090", "#377eb8", "#4daf4a",
                   "#b2182b","#fa9fb5", "#984ea3","black"))



pp <-
  base_map +
  # theme_bw(standard_size)+
  geom_jitter(data = dat0,
              aes(x = lon/10, y = lat/10, fill = dtus, shape = dtus, size = dtus),
             colour = "black") +
  scale_fill_manual(values = colors_plot) +
  scale_shape_manual(values = c("TcI" = 21, "TcII" = 21, "TcIII" = 21,
                                "TcIV" = 21, "TcV" = 21, "TcVI" = 21, "Tcbat" = 10)) +
  scale_size_manual(values = c("TcI" = standard_size * 2, "TcII" = standard_size * 2,
                               "TcIII" = standard_size * 2, "TcIV" = standard_size * 2,
                               "TcV" = standard_size * 2, "TcVI" = standard_size * 2,
                               "Tcbat" = standard_size * 4)) +
  labs (fill = "DTUs", shape = "DTUs", alpha = "DTUs", size = "DTUs") +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  facet_wrap(vars(subtitle)) +
  theme(panel.spacing.x = unit(0.0001, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(legend.key.size = unit(1, "cm"),
        legend.text = element_text(margin = margin(t = 0, r = 0, b = 0, l = -10)),
        strip.text = element_text(size = standard_size * 10, hjust = 0)) +
  guides(fill = guide_legend(nrow = 1, override.aes = list(size = standard_size * 6)))

png(filename = "figs/Fig 3.png", width = 900 * 4, height = 480 * 4)
pp
dev.off()


