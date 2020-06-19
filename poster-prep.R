library(tidyverse)
library(rlang)
library(SwissCommunes)
pkgload::load_all()
source("./eRum-poster/swisscommunes_kirill.R")

zuerich <- read_delim(here::here("eRum-poster/data/einwohner.csv"), delim = ";")

zh_mutations <- get_merger_mapping_table(2010, 2020, "ZH")

zuerich_fusioniert <- join_mergers(zuerich, zh_mutations, x_join_year = "JAHR", "BFS", "GEMEINDE")

zuerich_prep <- zuerich_fusioniert %>%
  filter(BFS != BFS_neu)

bfs_neu <- distinct(zuerich_prep, BFS_neu) %>% pull()

zuerich_prep_neu <- zuerich_fusioniert %>%
  filter(BFS %in% bfs_neu) %>%
  bind_rows(zuerich_prep) %>%
  rename(POPULATION = EINWOHNER) %>%
  mutate(YEAR = as.Date(paste0(JAHR,"-01-01")))

zuerich_fus_prep <- zuerich_fusioniert %>%
  group_by(JAHR, BFS_neu, Gemeinde_neu) %>%
  summarize(POPULATION = sum(EINWOHNER)) %>%
  filter(BFS_neu %in% bfs_neu) %>%
  mutate(YEAR = as.Date(paste0(JAHR,"-01-01")))


print(xtable::xtable(zh_mutations), type="html", file="mutation_table.html")

print(htmlTable::htmlTable(zh_mutations), type="html", file="test.html")

p <- ggplot() +
  geom_line(data = zuerich_prep_neu, mapping = aes(x=YEAR, y=POPULATION, color = as.factor(GEMEINDE), group = GEMEINDE), size = 1) +
  geom_point(data = zuerich_prep_neu, mapping = aes(x=YEAR, y=POPULATION, color = as.factor(GEMEINDE), group = GEMEINDE), size = 1) +
  labs(title = 'Not accounting for mergers', color = 'Municipality') +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.key = element_rect(colour = "transparent", fill = "transparent"),
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend panel bg
    plot.title = element_text(hjust = 0.5) # center title
  )

ggsave(here::here("eRum-poster/data/nicht-fusioniert.png"), p, bg = "transparent", width = 6, height = 5)

p

p1 <- ggplot() +
  geom_line(data = zuerich_fus_prep, mapping = aes(x=YEAR, y=POPULATION, color = as.factor(Gemeinde_neu), group = Gemeinde_neu), size = 1) +
  geom_point(data = zuerich_fus_prep, mapping = aes(x=YEAR, y=POPULATION, color = as.factor(Gemeinde_neu), group = Gemeinde_neu), size = 1) +
  labs(title = 'Accounting for mergers', color = 'Municipality') +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.key = element_rect(colour = "transparent", fill = "transparent"),
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend panel bg
    plot.title = element_text(hjust = 0.5) # center title
  )
ggsave(here::here("eRum-poster/data/fusioniert.png"), p1, bg = "transparent", width = 6, height = 5)

p1
