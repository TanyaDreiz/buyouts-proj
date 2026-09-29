#Create stacked line chart to show costs across CBGs
plot_df <- clean_county_capped_alice %>%
  rename(cen_label = `cen_label`) %>%
  pivot_longer(
    cols = c(sum_cost_geoid20, sum_costcap_geoid20, alice_cost),
    names_to = "program",
    values_to = "cost"
  ) %>%
  mutate(
    program = factor(
      program,
      levels = c("sum_cost_geoid20", "sum_costcap_geoid20", "alice_cost"),
      labels = c("County-wide", "Capped", "Income-based Eligibility")
    ),
    cost_mil = cost / 1e6   
  )

#Order block groups by CBGs
bg_order <- plot_df %>%
  distinct(cen_label) %>%
  arrange(parse_number(as.character(cen_label))) %>%
  pull(cen_label)


plot_df <- plot_df %>%
  mutate(cen_label = factor(cen_label, levels = bg_order))

ggplot(plot_df, aes(x = cen_label, y = cost_mil, color = program, group = program)) +
  geom_line(linewidth = 0.7) +
  scale_y_continuous(labels = label_number()) +
  labs(
    x = "Census block group",
    y = "Total costs ($2024, mil)",
    color = "Buyout program"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 8),
    legend.position = "top"
  )
ggsave(
  filename = file.path("output", "figures", "buyout_costs.png"),
  width = 10, height = 8, units = "in", dpi = 300
)
