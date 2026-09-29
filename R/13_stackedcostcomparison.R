scenario_colors <- c(
  "County-wide"              = "#264653",
  "Capped"                   = "#2a9d8f",
  "Income-based Eligibility" = "#e9c46a"
)


p_cost <- ggplot(plot_df, aes(x = cen_label, y = cost_mil, color = program, group = program)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = scenario_colors) +
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

print(p_cost)
ggsave(
  filename = file.path("output", "figures", "buyout_costs_updatedcols1.png"),
  width = 10, height = 8, units = "in", dpi = 300
)
