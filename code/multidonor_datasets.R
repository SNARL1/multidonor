if (!require(librarian)){
  install.packages("librarian")
}
librarian::shelf(RPostgres, DBI, rstudioapi, dplyr, tidyr, readr, lubridate, magrittr, here)

# Retrieve data from database
source("code/db_connect.R") 
relocate_pg <- tbl(con, "relocate") %>% 
  collect()
relocate_frog_pg <- tbl(con, "relocate_frog") %>% 
  collect()

ves_counts1 <- tbl(con, "visit") %>% 
  filter(site_id %in% c(10223, 10225, 70470, 70481, 72695, 72773) & visit_date > "2018-01-01") %>% 
  rename(visit_id = id,
         visit_comment = comment) %>% 
  inner_join(tbl(con, "survey"), by = "visit_id") %>% 
  rename(survey_id = id,
         survey_comment = comment) %>% 
  filter(survey_type == "visual") %>% 
  left_join(tbl(con, "visual_survey"), by = "survey_id") %>% # lines 21-24 ensure inclusion of surveys with counts = 0
  filter(visit_status == "suitable" &
        (species == "ramu" | is.na(species) == TRUE) &
        (visual_animal_state == "alive" | is.na(visual_animal_state) == TRUE)) %>% 
  select(site_id, visit_date, visual_life_stage, location, count) %>% 
  collect()

cmr_captures1 <- tbl(con, "visit") %>% 
  filter(site_id %in% c(10223, 10225, 70470, 70481, 72695, 72773) & visit_date > "2018-01-01") %>% 
  rename(visit_id = id,
         visit_comment = comment) %>% 
  inner_join(tbl(con, "survey"), by = "visit_id") %>% 
  rename(survey_id = id,
         survey_comment = comment) %>% 
  left_join(tbl(con, "capture_survey"), by = "survey_id") %>% 
  filter(visit_status == "suitable" & 
         survey_type == "cmr" & 
         (species == "ramu" | is.na(species) == TRUE) &
         (capture_animal_state != "dead" | is.na(capture_animal_state) == TRUE)) %>% 
  select(site_id, visit_date, pit_tag_ref) %>% 
  collect()
source("code/db_disconnect.R")

# Retrieve unappended data from "pCloudDrive/MLRG/code/ErrorChecking/2021/cleaned_data/relocate_semiclean_220121" -  not appended yet pending missing zoo data
relocate_csv <- read_csv(here("data", "raw", "relocate.csv"))
relocate_frog_csv <- read_csv(here("data", "raw", "relocate_frog.csv"))

# Create datasets for tables
relocate_csv <- relocate_csv %>% 
  mutate(collect_siteid = as.character(collect_siteid),
         release_siteid1 = as.integer(release_siteid1),
         release_siteid2 = as.integer(release_siteid2))
relocate <- bind_rows(relocate_pg, relocate_csv) %>% 
  filter(release_siteid1 %in% c(70470, 70481, 10223, 10225)) %>% 
  mutate(collect_siteid = as.integer(collect_siteid))

join_id <- relocate %>% 
  select(id)

relocate_frog_csv <- relocate_frog_csv %>% 
  mutate(pit_tag_ref = as.character(pit_tag_ref))
relocate_frog <- bind_rows(relocate_frog_pg, relocate_frog_csv) %>% 
  inner_join(join_id, by = c("relocate_id" = "id"))

relocate1 <- relocate_frog %>% 
  count(relocate_id) %>% 
  inner_join(relocate, by = c("relocate_id" = "id")) %>% 
  mutate(n_release = n, 
         collect_year = year(collect_date),
         release_year = year(release_date)) %>% 
  rename(id = relocate_id,
         site_id = release_siteid1) %>% 
    select(site_id, collect_siteid, collect_year, zoo, release_year, n_release) %>% 
  arrange(site_id, collect_siteid, release_year)
write_csv(relocate1, here("data", "clean", "relocate_details_tbl.csv"))

relocate_sum <- relocate1 %>% 
  mutate(collect_siteid = replace(collect_siteid, collect_siteid == 70284, 70567)) %>% # combine 2 adjacent sites
  group_by(site_id, collect_siteid) %>% 
  summarize(release_total = sum(n_release)) %>% 
  ungroup() %>% 
  mutate(jurisdiction = case_when(site_id == 10223 | site_id == 10225 ~ "kings_canyon",
                                  site_id == 70470 | site_id == 70481 ~ "yosemite")) %>% 
  relocate(jurisdiction, .before = site_id)
write_csv(relocate_sum, here("data", "clean", "relocate_sum_tbl.csv"))

# Create datasets for frog count figure
ves_counts2 <- ves_counts1 %>% 
  mutate(site_id = replace(site_id, site_id == 72773, 70470), # combine adjacent sites
         site_id = replace(site_id, site_id == 72695, 70481)) %>% 
  group_by(site_id, visit_date, visual_life_stage) %>% 
  summarize(total_count = sum(count)) %>% 
  ungroup() %>% 
  complete(nesting(site_id, visit_date), visual_life_stage, fill = list(total_count = 0)) %>%  # make implicit "zero" counts explicit for each possible life stage
  ungroup() %>% 
  arrange(site_id, visit_date, visual_life_stage) 
write_csv(ves_counts2, here("data", "clean", "ves_counts.csv"))

cmr_captures2 <- cmr_captures1 %>% 
  mutate(site_id = replace(site_id, site_id == 72773, 70470), # combine adjacent sites
         site_id = replace(site_id, site_id == 72695, 70481)) %>% 
  distinct(site_id, visit_date) %>% 
  arrange(site_id, visit_date) %>% 
  group_by(site_id) %>% 
  mutate(period = cumsum(c(1L, diff(visit_date) > 2))) %>% # 1L creates constant with an integer value of 1 (as starting value for vector), cumsum/diff assign period values based on magnitude of difference between adjacent visit_date values
  ungroup() %>% 
  inner_join(cmr_captures1, by = c("site_id", "visit_date"))
cmr_captures_perioddate <- cmr_captures2 %>% 
  group_by(site_id, period) %>% 
  summarize(date_min = min(visit_date))
cmr_counts <- cmr_captures2 %>% 
  distinct(site_id, period, pit_tag_ref) %>% 
  count(site_id, period) %>% 
  inner_join(cmr_captures_perioddate, by = c("site_id", "period")) %>% 
  relocate(date_min, .before = period)
write_csv(cmr_counts, here("data", "clean", "cmr_counts.csv"))

# Create datasets for bar chart
relocate_source_prop <- relocate_sum %>% 
  group_by(site_id) %>% 
  summarize(release_total_site = sum(release_total)) %>% 
  inner_join(relocate_sum, by = "site_id") %>% 
  relocate(release_total_site, .after = release_total) %>% 
  mutate(
    group = "released", 
    proportion = release_total / release_total_site) %>% 
  select(site_id, collect_siteid, group, proportion)

capture_pittags <- cmr_captures2 %>% 
  distinct(site_id, pit_tag_ref) # Produces one more than when using `distinct(pit_tag_ref)`. Need to find out why. 

capture_source_prop <- relocate %>% 
  inner_join(relocate_frog, by = c("id" = "relocate_id")) %>% 
  select(release_siteid1, collect_siteid, pit_tag_ref) %>% 
  inner_join(capture_pittags, by = "pit_tag_ref") %>% 
  select(site_id, collect_siteid, pit_tag_ref) %>% 
  mutate(collect_siteid = replace(collect_siteid, collect_siteid == 70284, 70567)) %>% 
  count(site_id, collect_siteid) %>% 
  group_by(site_id) %>% 
  summarize(capture_total_site = sum(n)) %>% 
  inner_join(capture_source, by = "site_id") %>% 
  relocate(capture_total_site, .after = n) %>% 
  mutate(
    group = "captured",
    proportion = n / capture_total_site) %>% 
  select(site_id, collect_siteid, group, proportion)
relocate_capture_prop <- bind_rows(relocate_source_prop, capture_source_prop)
write_csv(relocate_capture_prop, here("data", "clean", "relocate_capture_prop.csv"))



  
