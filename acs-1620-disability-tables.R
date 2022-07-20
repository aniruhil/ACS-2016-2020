library(tidyverse)
library(tidycensus)

load_variables(2020, "acs5") -> myvars

get_acs(
  geography = "state",
  table = "B18101",
  year = 2020
  ) %>%
  left_join(
    myvars[, c(1:3)],
    by = c("variable" = "name")
  ) -> b18101_state

b18101_state %>%
  mutate(
    sex = case_when(
      grepl("Male", label) ~ "Male",
      grepl("Female", label) ~ "Female"
      ),
    disability = case_when(
      grepl("With a disability", label) ~ "With a Disability",
      grepl("No disability", label) ~ "No Disability"
      ),
    age = stringr::str_remove_all(
      label,
      "Estimate!!Total:|!!Male:|!!Male:!!|!!Female:|!!Female:!!|:!!No disability|:!!With a disability|!!|:"
      )
    ) %>%
  filter(
    !is.na(sex),
    !is.na(disability)
    ) -> tab01_df

tab01_df %>%
  group_by(NAME) %>%
  mutate(
    total_popn = sum(estimate)
    ) %>%
  group_by(NAME, disability) %>%
  mutate(
    total = sum(estimate)
    ) %>%
  ungroup() %>%
  select(NAME, disability, total_popn, total) %>%
  distinct() %>%
  mutate(
    prop = (total / total_popn)
  ) %>%
  group_by(
    NAME, total_popn
  ) %>%
  select(NAME, total_popn, disability, prop) %>%
  pivot_wider(
    names_from = disability,
    values_from = prop
  ) -> tab01




