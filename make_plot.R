library(tidyverse)

# 1. Read the saved tibble
plot_data <- read_rds("top_songs.rds")

# 2. Extract final positions for labels and adjust to prevent overlap (from index.qmd)
label_data <- plot_data %>%
  filter(highlight != "Other #1 Hits") %>%
  group_by(highlight) %>%
  filter(week == max(week)) %>%
  ungroup() %>%
  mutate(
    display_label = case_when(
      highlight == "Destiny's Child - 'Independent Women Pa...'" ~ "Destiny's Child\n'Independent Women'",
      highlight == "Lonestar - 'Amazed'" ~ "Lonestar\n'Amazed'",
      highlight == "Santana - 'Maria, Maria'" ~ "Santana\n'Maria, Maria'",
      highlight == "Creed - 'With Arms Wide Open'" ~ "Creed\n'With Arms Wide Open'",
      highlight == "Aaliyah - 'Try Again'" ~ "Aaliyah\n'Try Again'",
      TRUE ~ highlight
    ),
    y_adj = case_when(
      highlight == "Santana - 'Maria, Maria'" ~ rank + 5,
      highlight == "Destiny's Child - 'Independent Women Pa...'" ~ rank - 4,
      highlight == "Aaliyah - 'Try Again'" ~ rank + 1,
      TRUE ~ rank
    ),
    x_adj = week + 1
  )

# 3. Define custom color palette (from index.qmd)
highlight_colors <- c(
  "Destiny's Child - 'Independent Women Pa...'" = "#FF2E93", 
  "Lonestar - 'Amazed'" = "#00F5FF",                         
  "Santana - 'Maria, Maria'" = "#FF8A00",                    
  "Creed - 'With Arms Wide Open'" = "#00FF66",               
  "Aaliyah - 'Try Again'" = "#B5179E",                       
  "Other #1 Hits" = "#475569"                                
)

# 4. Build the billboard plot and assign it to billboard_plot
billboard_plot <- ggplot(plot_data, aes(x = week, y = rank, group = label)) +
  geom_line(
    data = filter(plot_data, highlight == "Other #1 Hits"),
    aes(color = highlight),
    linewidth = 0.5,
    alpha = 0.25
  ) +
  geom_line(
    data = filter(plot_data, highlight != "Other #1 Hits"),
    aes(color = highlight),
    linewidth = 1.2,
    alpha = 1.0
  ) +
  geom_point(
    data = label_data,
    aes(color = highlight),
    size = 2.5
  ) +
  geom_text(
    data = label_data,
    aes(x = x_adj, y = y_adj, label = display_label, color = highlight),
    hjust = 0,
    size = 3.2,
    fontface = "bold",
    lineheight = 0.85
  ) +
  scale_y_reverse(
    limits = c(100, 1),
    breaks = c(1, 10, 25, 50, 75, 100),
    expand = c(0.02, 0.02)
  ) +
  scale_x_continuous(
    limits = c(1, 78),
    breaks = seq(0, 70, by = 10),
    expand = c(0.02, 0.02)
  ) +
  scale_color_manual(values = highlight_colors, guide = "none") +
  labs(
    title = "Rise & Fall of the Billboard #1 Hits (Year 2000)",
    subtitle = "Highlighting the longevity and trajectory of iconic tracks that reached the top spot.",
    x = "Weeks on Chart",
    y = "Rank (reversed)",
    caption = "Source: tidyr::billboard | Design by Antigravity"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "#0B0F19", color = NA),
    panel.background = element_rect(fill = "#0B0F19", color = NA),
    text = element_text(color = "#F8FAFC"),
    plot.title = element_text(size = 16, face = "bold", color = "#F8FAFC", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 11, color = "#94A3B8", margin = margin(b = 20)),
    plot.caption = element_text(size = 8, color = "#64748B", margin = margin(t = 20)),
    panel.grid.major = element_line(color = "#1E293B", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#94A3B8", size = 10),
    axis.title = element_text(color = "#F8FAFC", size = 11, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    plot.margin = margin(20, 120, 20, 20)
  )

# 5. Save the plot using ggsave
ggsave("billboard.png", plot = billboard_plot, width = 10, height = 6.5, dpi = 300)
