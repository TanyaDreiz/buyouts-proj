#Using ACS B11001 Householder Type dataframe to create multi and single family
#household estimates 

total_hh <- hh_type_df %>%
  filter(variable == "B11001_001") %>%
  select(GEOID, NAME, total_hh = estimate, total_hh_moe = moe)

family_hh <- hh_type_df %>%
  filter(variable == "B11001_002") %>%
  select(GEOID, family_est = estimate, family_moe = moe)

nonfam_not_alone <- hh_type_df %>%
  filter(variable == "B11001_009") %>%
  select(GEOID, nonfam_est = estimate, nonfam_moe = moe)

living_alone <- hh_type_df %>%
  filter(variable == "B11001_008") %>%
  select(GEOID, living_alone_est = estimate, living_alone_moe = moe)

# Join them all together by GEOID
hh_composition <- total_hh %>%
  left_join(family_hh, by = "GEOID") %>%
  left_join(nonfam_not_alone, by = "GEOID") %>%
  left_join(living_alone, by = "GEOID")

# Combine family + nonfamily-not-alone into multi-person household
combine_moe <- function(moe_a, est_a, moe_b, est_b) {
  a <- ifelse(est_a == 0, moe_a, moe_a^2)
  b <- ifelse(est_b == 0, moe_b, moe_b^2)
  sqrt(a + b)
}

hh_composition <- hh_composition %>%
  mutate(
    multi_person_est = family_est + nonfam_est,
    multi_person_moe = round(combine_moe(family_moe, family_est, nonfam_moe, nonfam_est), 1),
    multi_person_share = round(multi_person_est / total_hh, 4),
    living_alone_share = round(living_alone_est / total_hh, 4)
  ) %>%
  select(GEOID, NAME, total_hh, total_hh_moe,
         multi_person_est, multi_person_moe, multi_person_share,
         living_alone_est, living_alone_moe, living_alone_share)

#Split NAME into block group and tract
hh_composition <- hh_composition %>%
  mutate(
    block_group = str_extract(NAME, "(?<=Block Group )\\d+"),
    tract       = str_extract(NAME, "(?<=Census Tract )[\\d.]+")
  )
