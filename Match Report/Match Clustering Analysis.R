library(tidyverse)
library(tidymodels)
library(MNTSportScience)
library(cluster)
library(factoextra)
library(ggfortify)
library(PerformanceAnalytics)
library(lubridate)

con <- MNTSportScience::create_amz_mysql_con("sandbox_jwebb")

colsNormal <- c(
  "dist",
  "hml_dist",
  # "hml_cnt",
  "explosive_dist",
  "vel_z12_dist",
  "vel_z34_dist",
  # "vel_z56_dist",
  "vel_z5_dist",
  # "vel_z56_cnt",
  "vel_z6_dist",
  # "vel_z6_cnt",
  "acc_z456_cnt",
  "dec_z456_cnt"
)

colsRelative <- c(
  # "vel_z56_dist_per_cnt",
  # "vel_z6_dist_per_cnt",
  # "hml_dist_per_cnt",
  "hsr_hml_ratio"
  # "hml_dist_ratio"
)

matchData <- tbl(con, "ssDrill") %>% 
    mutate(
      vel_z12_dist = vel_z1_dist + vel_z2_dist,
      vel_z34_dist = vel_z3_dist + vel_z4_dist,
      vel_z56_dist = vel_z5_dist + vel_z6_dist,
      vel_z56_cnt = vel_z5_cnt + vel_z6_cnt,
      vel_z56_dist_per_cnt = vel_z56_dist / vel_z56_cnt,
      vel_z6_dist_per_cnt = vel_z6_dist / vel_z6_cnt,
      hml_dist_per_cnt = hml_dist / hml_cnt,
      hsr_hml_ratio = vel_z56_dist / hml_dist,
      acc_z456_cnt = acc_z4_cnt + acc_z5_cnt + acc_z6_cnt,
      dec_z456_cnt = dec_z4_cnt + dec_z5_cnt + dec_z6_cnt,
      hml_dist_ratio = hml_dist / dist
    ) %>%
    select(
      date = drill_date, 
      drill = drill_title, 
      position, 
      athlete = name_display,
      dur, 
      one_of(colsNormal),
      one_of(colsRelative)
    ) %>% 
    filter(
      position %in% c("CB", "FB", "CM", "WAM", "AM", "FWD"),
      str_detect(drill, "^GAME - "),
      date >= '2020-01-01',
      !str_detect(drill, "ESTIMATE"),
      str_detect(drill, "GAME - FIRST HALF - ") |  str_detect(drill, "GAME - SECOND HALF - ")
    ) %>% 
    collect() %>% 
    mutate(
      opponent = drill %>%
        str_remove_all(., "GAME - FIRST HALF - ") %>%
        str_remove_all(., "GAME - SECOND HALF - ") %>%
        str_remove_all(., "VS") %>% 
        str_remove_all(., " - 1 OF 2") %>% 
        str_remove_all(., " - 2 OF 2") %>% 
        trimws(.),
      match = paste0(opponent, " ", date)
    ) %>% 
    select(match, everything(), -c(date, drill, opponent))

#Correlation Matrix Plot
matchData %>% 
  mutate(across(.cols = one_of(colsNormal), .fns = function(.x){ .x / dur})) %>% 
  select(-c(match, position, athlete, dur)) %>% 
  PerformanceAnalytics::chart.Correlation(., histogram = F)

matchDataRelative <- matchData %>% 
  select(-athlete) %>% 
  group_by(match, position) %>% 
  summarise(.groups = 'keep', 
    across(one_of(c(colsNormal,"dur")), sum),
    across(one_of(colsRelative), mean)
  ) %>% 
  ungroup() %>% 
  mutate(
    across(one_of(colsNormal), function(.x){.x/dur})
  ) %>% 
  select(-dur) %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

matchDataRelativeByTeam <- matchDataRelative %>% 
  select(-position) %>% 
  group_by(match) %>% 
  summarise(across(everything(), mean))

matchDataRelativeByPosition <- matchDataRelative %>% 
  group_by(match, position) %>% 
  summarise(.groups = 'keep', across(where(is.numeric), mean)) %>% 
  ungroup() %>% 
  pivot_longer(., cols = -c(match, position)) %>% 
  mutate(metric = paste0(name, "_", str_to_lower(position))) %>% 
  select(-name, -position) %>% 
  pivot_wider(data = ., id_cols = match, names_from = metric, values_from = value) %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))
  


# Relative By Team Analysis -----------------------------------------------

#Choose K? 
  tibble(k = 1:10) %>%
    mutate(
      kclust = map(k, ~ kmeans(scale(select(matchDataRelativeByTeam, -match)), .x)),
      glanced = map(kclust, glance),
    ) %>%
    unnest(cols = c(glanced)) %>%
    ggplot(aes(k, tot.withinss)) +
    geom_line(alpha = 0.5, size = 1.2, color = "midnightblue") +
    geom_point(size = 2, color = "midnightblue")

kmeanByTeam <- matchDataRelativeByTeam %>% 
  select(-match) %>% 
  scale(.) %>% 
  kmeans(., centers = 5) 

matchDataRelativeByTeamPCA <- kmeanByTeam %>% 
  augment(., matchDataRelativeByTeam) %>% 
  nest(data = everything()) %>% 
  mutate(
    pca = map(data, ~ prcomp(.x %>% select(-match, -.cluster), center = TRUE, scale = TRUE)),
    pca_aug = map2(pca, data, ~augment(.x, data = .y))
  )

#PCA Variance Explained
var_exp <- matchDataRelativeByTeamPCA %>% 
  select(pca_aug) %>% 
  unnest(pca_aug) %>% 
  select(contains("PC")) %>% 
  summarize(across(contains("PC"), var)) %>% 
  pivot_longer(cols = everything(), names_to = 'pc', values_to = "variance") %>% 
  mutate(
    var_exp = variance/sum(variance),
    cum_var_exp = cumsum(var_exp),
    # pcOrder = str_replace(pc, ".fittedPC", "") %>% as.numeric()
    pc = str_replace(pc, ".fitted", "") %>% as_factor(),
  )
  
#Graph Of Variance Explained
var_exp %>% 
  rename(
    `Variance Explained` = var_exp,
    `Cumulative Variance Explained` = cum_var_exp
  ) %>% 
  gather(key = key, value = value, `Variance Explained`:`Cumulative Variance Explained`) %>% 
  ggplot(aes(pc, value, group = key)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~key, scales = "free_y") +
  theme_bw() +
  lims(y = c(0, 1)) +
  labs(y = "Variance", title = "Variance explained by each principal component")

#PC1 PC2 Graph
matchDataRelativeByTeamPCA %>%
  mutate(
    pca_graph = map2(
      .x = pca,
      .y = data,
      ~ autoplot(.x, 
         loadings = T, 
         loadings.label = T,
         loadings.label.repel = F,
         data = .y, 
         label = F,
         colour = '.cluster', 
         shape = '.cluster',
         label.label = "match",
         label.repel = F,
         frame = T
        )
    )
  ) %>%
  pull(pca_graph)

matchDataRelativeByTeamPCA %>% 
  select(data) %>% 
  unnest(cols = c(data)) %>% 
  pivot_longer(., cols = c(colsNormal, colsRelative), names_to = 'metric') %>% 
  ggplot() +
    geom_boxplot(aes(y = value, x = .cluster)) +
    facet_wrap(facets = ~ metric, scales = "free")





  
  

# Relative By Position Analysis -----------------------------------------------

#Choose K? 
tibble(k = 1:10) %>%
  mutate(
    kclust = map(k, ~ kmeans(scale(select(matchDataRelativeByPosition, -match)), .x)),
    glanced = map(kclust, glance),
  ) %>%
  unnest(cols = c(glanced)) %>%
  ggplot(aes(k, tot.withinss)) +
  geom_line(alpha = 0.5, size = 1.2, color = "midnightblue") +
  geom_point(size = 2, color = "midnightblue")

kmeanByPosition <- matchDataRelativeByPosition %>% 
  select(-match) %>% 
  scale(.) %>% 
  kmeans(., centers = 7) %>% 
  augment(., matchDataRelativeByPosition)

matchDataRelativeByPositionPCA <- kmeanByPosition %>% 
  nest(data = everything()) %>% 
  mutate(
    pca = map(data, ~ prcomp(.x %>% select(-match, -.cluster), center = TRUE, scale = TRUE)),
    pca_aug = map2(pca, data, ~augment(.x, data = .y))
  )

#PCA Variance Explained
var_exp <- matchDataRelativeByPositionPCA %>% 
  select(pca_aug) %>% 
  unnest(pca_aug) %>% 
  select(contains("PC")) %>% 
  summarize(across(contains("PC"), var)) %>% 
  pivot_longer(cols = everything(), names_to = 'pc', values_to = "variance") %>% 
  mutate(
    var_exp = variance/sum(variance),
    cum_var_exp = cumsum(var_exp),
    pc = str_replace(pc, ".fitted", "") %>% as_factor(),
  )

#Graph Of Variance Explained
var_exp %>% 
  rename(
    `Variance Explained` = var_exp,
    `Cumulative Variance Explained` = cum_var_exp
  ) %>% 
  gather(key = key, value = value, `Variance Explained`:`Cumulative Variance Explained`) %>% 
  ggplot(aes(pc, value, group = key)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~key, scales = "free_y") +
  theme_bw() +
  lims(y = c(0, 1)) +
  labs(y = "Variance", title = "Variance explained by each principal component")

#PC1 PC2 Graph
matchDataRelativeByPositionPCA %>%
  mutate(
    pca_graph = map2(
      .x = pca,
      .y = data,
      ~ autoplot(.x, 
         loadings = T, 
         loadings.label = T,
         loadings.label.repel = T,
         data = .y, 
         label = F,
         colour = '.cluster', 
         shape = '.cluster',
         label.label = "match",
         label.repel = F,
         frame = T
      ) 
    )
  ) %>%
  pull(pca_graph)


matchDataRelativeByPositionPCA %>% 
  select(data) %>% 
  unnest(cols = c(data)) %>% 
  pivot_longer(., cols = -c(.cluster, match), names_to = 'metric') %>% 
  ggplot() +
  geom_boxplot(aes(y = value, x = .cluster)) +
  facet_wrap(facets = ~ metric, scales = "free")



