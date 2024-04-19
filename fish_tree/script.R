library(ape)
library(dplyr)
library(ggtree)
library(ggtreeExtra)
library(ggplot2)
# library(fishualize) # remotes::install_github("mattiaghilardi/fishualize@fix-fishapes")
library(ggimage)
library(stringr)
library(ggnewscale)
library(rcartocolor)

# Read family tree from Fish Tree of Life

family_tree <- read.tree("fish_tree/fish_tree_of_life/family_skeletal.tre")

# Read Fish Tree of Life taxonomy

taxonomy <- read.csv("fish_tree/fish_tree_of_life/PFC_taxonomy.csv")

# Get fishes list from this repository

fishes <- read.csv("fishes.csv", stringsAsFactors = T)

fishes_subset <- fishes %>%
  filter(class %in% c("Teleostei", "Chondrostei", "Holostei"))

#  Family statistics

family_stats <- fishes_subset %>%
  group_by(class, order, family) %>%
  summarize(
    gbif_obis = sum(source_gbif | source_obis),
    gbif = sum(source_gbif),
    obis = sum(source_obis),
    edna = sum(source_dna)
  )

# Group tree by order 

order_families_df <- taxonomy %>% group_by(order, family) %>% summarize() %>% ungroup()
order_families <- list()

for (selected_order in order_families_df$order) {
  families <- order_families_df %>%
    filter(order == selected_order) %>%
    pull(family)
  order_families[[selected_order]] <- families
}

family_tree <- drop.tip(family_tree, setdiff(family_tree$tip.label, family_stats$family))
family_tree <- groupOTU(family_tree, order_families, overlap = "origin") # try overlap origin? connect parameter?

# Fish silhouettes
# Some families are excluded to reduce clutter

shapes_exclude <- c("Batrachoididae", "Bythitidae", "Callionymidae", "Plesiopidae", "Diodontidae", "Platycephalidae", "Syngnathidae", "Monacanthidae", "Engraulidae", "Ctenoluciidae", "Cichlidae", "Opistognathidae", "Gobiesocidae", "Tripterygiidae", "Chaenopsidae", "Clinidae", "Pempheridae", "Ephippidae", "Siganidae", "Haemulidae", "Nemipteridae", "Lethrinidae", "Cheilodactylidae")

shapes <- list.files("fish_tree/shapes", pattern = ".png")
shape_families <- sapply(stringr::str_split(shapes, "_"), head, 1)
shape_df <- data.frame(family = shape_families, shape = paste0("fish_tree/shapes/", shapes)) %>%
  filter(!family %in% shapes_exclude) %>%
  filter(family %in% family_stats$family)

# Figure
# TODO: conflicting edges, see https://groups.google.com/g/bioc-ggtree/c/Q4LnwoTf1DM/m/uqYdYB_VBAAJ

n_colors <- length(order_families)

ggtree(family_tree, aes(color = group), layout = "circular") +
  xlim(-200, NA) +
  # geom_fruit(data = family_stats, aes(x = gbif_obis, y = family, fill = group), geom = geom_bar, stat = "identity") +
  # geom_fruit(data = family_stats, aes(x = edna, y = family, fill = group), geom = geom_bar, stat = "identity") +
  scale_fill_manual(values = c(viridis::viridis_pal(option = "A")(n_colors))) +
  scale_color_manual(values = c("#eeeeee", viridis::viridis_pal(option = "A")(n_colors))) +
  new_scale_fill() +
  geom_fruit(data = family_stats, aes(y = family, fill = edna, alpha = gbif_obis), width = 20, geom = geom_tile, offset = 0.05) +
  geom_fruit(data = family_stats, aes(y = family, fill = edna, alpha = edna), width = 20, geom = geom_tile, offset = 0.08) +
  scale_fill_carto_c(palette = "TealGrn", trans = "log10") +
  geom_tiplab(size = 1.9, offset = 65, color = "black") +
  geom_fruit(data = shape_df, aes(image = shape, y = family), geom = geom_image, offset = 0.45, size = 0.03) +
  theme(legend.position = "none", plot.margin = unit(c(-15, -15, -15, -15), "mm"))

ggsave("fish_tree/tree.png", height = 12, width = 12, dpi = 300)
