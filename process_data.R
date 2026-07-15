library(tidyverse)

# 1. Tidy and transform the data
billboard_long <- billboard %>%
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank",
    values_drop_na = TRUE
  ) %>%
  mutate(week = as.integer(week))

# 2. Identify songs that reached #1 rank
no1_songs <- billboard_long %>%
  filter(rank == 1) %>%
  select(artist, track) %>%
  distinct()

# 3. Filter full trajectories for only those #1 songs
no1_trajectories <- billboard_long %>%
  inner_join(no1_songs, by = c("artist", "track")) %>%
  mutate(label = paste0(artist, " - '", track, "'"))

# 4. Highlight iconic/representative tracks to guide the viewer's eye
highlight_songs <- c(
  "Destiny's Child - 'Independent Women Pa...'",
  "Lonestar - 'Amazed'",
  "Santana - 'Maria, Maria'",
  "Creed - 'With Arms Wide Open'",
  "Aaliyah - 'Try Again'"
)

top_songs <- no1_trajectories %>%
  mutate(
    highlight = if_else(label %in% highlight_songs, label, "Other #1 Hits")
  )

# Save the dataset to a file
write_rds(top_songs, "top_songs.rds")
