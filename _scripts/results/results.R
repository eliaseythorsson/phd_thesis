
theme_set(theme_bw(base_size = 10, base_family = "sans"))

############################
#### Importing the data ####
############################

ids <- read_rds("_data/results/ids.rds")
lsh <- read_rds("_data/results/lsh.rds")
hg <- read_rds("_data/results/hg.rds")
hs <- read_rds("_data/results/hs.rds")
bs <- read_rds("_data/results/bs.rds")
si <- read_rds("_data/results/si.rds")
lg <- read_rds("_data/results/lg.rds")

hg_density_age <- hg %>%
    filter(staff_type == "Læ", age_y >= 0, age_y <= 99) %>%
    ggplot(aes(x = age_y, y = ..count..)) +
    geom_density(fill = "red", alpha = 0.3) +
    labs(y = "Number of contacts", x = "Age (years)") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = scales::trans_breaks(identity, identity, n = 20)) +
    theme_bw()

ggsave(
    filename = paste0("_figures/results/", Sys.Date(), "-density-age.pdf"), 
    plot = hg_density_age,
    width = 12, height = 8, units = "cm", dpi = 600)

ggsave(
    filename = paste0("_figures/results/", Sys.Date(), "-density-age.png"), 
    plot = hg_density_age,
    width = 12, height = 8, units = "cm", dpi = 600)


save.image(file = paste0("_analyses/results/", Sys.Date(), "-04-1-results", ".RData"))
