#Create a bar chart
# One row per block group, then sum the three costs
geoid_level <- clean_county_capped_alice %>%
  distinct(geoid20, .keep_all = TRUE) %>%
  select(geoid20, rpa, sum_cost_geoid20, sum_costcap_geoid20, alice_cost)

scenario_labels <- c(
  sum_cost_geoid20    = "County-wide buyout",
  sum_costcap_geoid20 = "Capped buyout",
  alice_cost          = "Income-based buyout"
)

scenario_colors <- c(
  "County-wide buyout"  = "#264653",
  "Capped buyout"       = "#2a9d8f",
  "Income-based buyout" = "#e9c46a"
)

# Panel A data: county totals
totals <- geoid_level %>%
  summarise(across(all_of(names(scenario_labels)), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "scenario", values_to = "cost") %>%
  mutate(scenario = factor(scenario_labels[scenario], levels = scenario_labels))
# Panel B data: totals per RPA
by_rpa <- geoid_level %>%
  group_by(rpa) %>%
  summarise(across(all_of(names(scenario_labels)), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-rpa, names_to = "scenario", values_to = "cost") %>%
  mutate(
    scenario = factor(scenario_labels[scenario], levels = scenario_labels),
    rpa = factor(as.character(rpa), levels = names(rpa_colors))
  )

plot_totals <- ggplot(totals, aes(x = scenario, y = cost, fill = scenario)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = label_number(scale = 1e-6, accuracy = 1)(cost)),
            vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = scenario_colors) +
  scale_y_continuous(labels = label_number(scale = 1e-6),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(x = NULL, y = "Total cost ($2024, mil)") +
  theme_classic(base_size = 12) +
  theme(legend.position = "none")

plot_by_rpa <- ggplot(by_rpa, aes(x = rpa, y = cost, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = scenario_colors) +
  scale_y_continuous(labels = label_number(scale = 1e-6),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "Total cost ($2024, mil)", fill = NULL) +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cost_comparison <- (plot_totals / plot_spacer() / plot_by_rpa) +
  plot_layout(heights = c(10, 1, 10)) +
  plot_annotation(tag_levels = list(c("A", "", "B")))

print(cost_comparison)
ggsave(file.path("output", "figures", "cost_comparison.png"),
       cost_comparison, width = 10, height = 8, units = "in", dpi = 300)
