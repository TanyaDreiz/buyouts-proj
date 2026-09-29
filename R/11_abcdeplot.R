#Work on abcde figures

#Add in geoid20 and districts where NA values came up using manual search in Arc
allcounty_capped_geography <- allcounty_capped_geography %>%
  mutate(
    geoid20 = as.character(geoid20),
    district = case_when(
      is.na(district) & tmk == "38001002" ~ "EAST HONOLULU",
      is.na(district) & tmk == "49007030" ~ "KOOLAUPOKO",
      is.na(district) & tmk %in% c("53009033", "53009036") ~ "KOOLAULOA",
      TRUE ~ district
    ),
    geoid20 = case_when(
      is.na(geoid20) & tmk == "38001002" ~ "150030002001",
      is.na(geoid20) & tmk == "49007030" ~ "150030103033",
      is.na(geoid20) & tmk == "53009033" ~ "150030102031",
      is.na(geoid20) & tmk == "53009036" ~ "150030102031",
      TRUE ~ geoid20
    )
  )

#Get parcel counts per geoid and rpa
allcounty_capped_geography <- allcounty_capped_geography %>%
  add_count(geoid20, name = "n_geoid20") %>%
  add_count(district, name = "n_district")

#Get a sum of buyout costs- countywide
allcounty_capped_geography <- allcounty_capped_geography %>%
  group_by(geoid20) %>%
  mutate(sum_cost_geoid20 = sum(public_costce_disc, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(district) %>%
  mutate(sum_cost_district = sum(public_costce_disc, na.rm = TRUE)) %>%
  ungroup()

#Get a sum of buyout costs- capped
allcounty_capped_geography <- allcounty_capped_geography %>%
  group_by(geoid20) %>%
  mutate(sum_costcap_geoid20 = sum(cap_disc, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(district) %>%
  mutate(sum_costcap_district = sum(cap_disc, na.rm = TRUE)) %>%
  ungroup()

#Deduplicate 
allcounty_dedup <- allcounty_capped_geography %>%
  distinct(geoid20, .keep_all = TRUE)

#Clean up dataframe
clean_county_capped <- allcounty_dedup[, c("public_costce_disc",
                                                      "cap_disc", "geoid20", "district",
                                                      "n_geoid20", "n_district",
                                                      "sum_cost_geoid20", "sum_cost_district",
                                                      "sum_costcap_geoid20", "sum_costcap_district")]
clean_county_capped_alice <- clean_county_capped_alice %>%
  mutate(
    rpa = case_match(
      rpa,
      "KOʻOLAU LOA"       ~ "Koʻolau Loa",
      "KOʻOLAU POKO"      ~ "Koʻolau Poko",
      "East Honolulu"     ~ "East Honolulu",
      "PUC"               ~ "PUC",
      "ʻEWA"              ~ "ʻEwa",
      "WAIʻANAE"          ~ "Waiʻanae",
      "North Shore Oʻahu" ~ "North Shore Oʻahu",
      .default = NA_character_
    )
  )
#Set RPA colors in R 
rpa_colors <- c("Koʻolau Loa" = "#EDC948", 
                "Koʻolau Poko" = "#76B7B2", 
                "East Honolulu" = "#4E79A7",
                "PUC" = "#B07AA1",
                "ʻEwa" = "#F28E2B",
                "Waiʻanae" = "#59A14F",
                "North Shore Oʻahu" = "#8C5643")

#Define the order of RPAs (East to West, starting from Koolauloa)
rpa_order <- c("Koʻolau Loa", "Koʻolau Poko", "East Honolulu",
               "PUC", "ʻEwa", "Waiʻanae",
               "North Shore Oʻahu")

#Add ALICE total and cost 
clean_county_capped_alice <- clean_county_capped %>%
  left_join(
    alice_buyouts %>%
      mutate(geoid20 = format(geoid20, scientific = FALSE, trim = TRUE)) %>%
      select(geoid20, total_ce_owner_alice, alice_cost, block_group, tract),
    by = "geoid20"
  )

clean_county_capped_alice <- clean_county_capped_alice %>%
  rename(n_alice = total_ce_owner_alice)

clean_county_capped_alice <- clean_county_capped_alice %>%
  rename(rpa = district)

#Add easier to read census labels, based on order of rpas
clean_county_capped_alice <- clean_county_capped_alice %>%
  mutate(
    cen_label = sprintf("%02d", match(geoid20, unique(geoid20))),
    cen_label = factor(cen_label, levels = unique(cen_label))
  )

#Start ABCDE plot
#Change numeric to character
clean_county_capped_alice <- clean_county_capped_alice %>%
  mutate(cen_label = as.numeric(as.character(cen_label)))

bg <- clean_county_capped_alice %>%
  group_by(cen_label, rpa) %>%
  summarise(
    n_impacted  = first(n_geoid20),
    cost_county = first(sum_cost_geoid20),
    cost_capped = first(sum_costcap_geoid20),
    n_income    = first(total_ce_owner_alice),
    cost_income = first(alice_cost),
    .groups = "drop"
  ) %>%
  mutate(rpa = as.character(rpa))


make_panel <- function(data, y, ylab, money = FALSE, show_x = FALSE) {
  ggplot(data, aes(x = cen_label, y = {{ y }}, fill = rpa)) +
    geom_col() +
    scale_x_continuous(
      breaks = seq_len(max(data$cen_label, na.rm = TRUE)),
      labels = if (show_x) waiver() else NULL,
      expand = expansion(add = 0.5)
    ) +
    scale_y_continuous(
      labels = if (money) label_number(scale = 1e-6, accuracy = 1) else label_comma(),
      breaks = breaks_pretty(n = 4),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_fill_manual(values = rpa_colors, breaks = names(rpa_colors)) +
    labs(
      x = if (show_x) "Census block group" else NULL,
      y = ylab,
      fill = "Regional planning area"
    ) +
    theme_classic(base_size = 11) +
    theme(
      axis.text.y  = element_text(size = 9),
      axis.title.y = element_text(size = 9, lineheight = 0.9),
      axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1)
    )
}

plot_a <- make_panel(bg, n_impacted,  "Total impacted\nparcels")
plot_b <- make_panel(bg, cost_county, "Cost county-wide\nbuyout ($2024, mil)", money = TRUE)
plot_c <- make_panel(bg, cost_capped, "Cost capped\nbuyout ($2024, mil)", money = TRUE)
plot_d <- make_panel(bg, n_income,    "Total income-based\nparcels")
plot_e <- make_panel(bg, cost_income, "Cost income-based\nbuyout ($2024, mil)",
                     money = TRUE, show_x = TRUE)

combined_plot <- (plot_a / plot_spacer() /
                    plot_b / plot_spacer() /
                    plot_c / plot_spacer() /
                    plot_d / plot_spacer() /
                    plot_e) +
  plot_layout(
    guides = "collect",
    heights = c(10, 1, 10, 1, 10, 1, 10, 1, 10)
  ) +
  plot_annotation(tag_levels = list(c("A", "", "B", "", "C", "", "D", "", "E")))


print(combined_plot)
ggsave(
  filename = file.path("output", "figures", "abcde_plot.png"),
  plot = combined_plot,
  width = 10, height = 8, units = "in", dpi = 300
)
