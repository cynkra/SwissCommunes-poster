source("./eRum-poster/swisscommunes_kirill.R")

zuerich <- read_delim("./eRum-poster/einwohner.csv", delim = ";")

zh_mutations <- get_merger_mapping_table(2010, 2019, "ZH")

zuerich_prep <- zuerich %>%
  filter(GEMEINDE == "Horgen" | GEMEINDE == "Hirzel")

zuerich_fusioniert <- join_mergers(zuerich, zh_mutations, x_join_year = "JAHR", "BFS", "GEMEINDE")


zuerich_prep_fus <- zuerich_fusioniert %>%
  filter(Gemeinde_neu == "Horgen") %>%
  group_by(JAHR, BFS_neu, Gemeinde_neu) %>%
  summarize(EINWOHNER = sum(EINWOHNER))


ggplot() +
  geom_line(data = zuerich_prep, mapping = aes(x=JAHR, y=EINWOHNER, color = as.factor(BFS), group = BFS)) +
  geom_line(data = zuerich_prep_fus, mapping = aes(x=JAHR, y=EINWOHNER, color = as.factor(BFS_neu), group = BFS_neu))

START_DATE <- as.Date("1960-01-01")




# Motivation
# Over the years, many municipalities changed
# Keep track
# In time Series, municipalities disappear and appear
# quite a mess if not handled properly
mutations <-
  swc_get_mutations(canton = NULL) %>%
  filter(mAbolitionDate >= !!START_DATE) %>%
  filter(!(mMutationNumber %in% !!IGNORE_MUTATIONS))

mutations_diff <-
  mutations %>%
  filter(mId.x != mId.y | mShortName.x != mShortName.y)


mutations_type <- mutations_diff %>%
  select(mAdmissionMode, mAdmissionDate) %>%
  group_by(mAdmissionMode, mAdmissionDate) %>%
  count()

ggplot(mutations_type, aes(x=mAdmissionDate, y=n, color=as.factor(mAdmissionMode), group = mAdmissionMode)) +geom_line()



# BFS publishes a mutation table
# Pretty useful and informative, but not that easy to handle
# SwissCommunes helps to work with the mutations

# Goal: Functions that help with the mutations
# Function that returns a mapping table between two dates
# Funtion that shows the mutations of a certain year
# usw....


# Result table excert

#
