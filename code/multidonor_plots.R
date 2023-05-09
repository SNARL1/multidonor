# Create figures

if (!require(librarian)){
  install.packages("librarian")
}
librarian::shelf(readr, dplyr, ggplot2, magrittr, here, patchwork)

cmr_counts <- read_csv(here("data", "clean", "cmr_counts.csv"))
p_adults <- cmr_counts %>% 
  ggplot(aes(x = date_min, y = n)) +
  geom_line(group = 1) +
  geom_point() +
  labs(x = "Visit date", y = "Number of adults captured") +
  facet_grid(. ~ site_id) +
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("A")

ves_counts <- read_csv(here("data", "clean", "ves_counts.csv")) 
p_tads <- ves_counts %>% 
  filter(visual_life_stage == "tadpole") %>% 
  ggplot(aes(x = visit_date, y = total_count)) +
  geom_line(group = 1) +
  geom_point() +
  labs(x = "Visit date", y = "Tadpole count") +
  facet_grid(. ~ site_id) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("B")

p_subs <- ves_counts %>% 
  filter(visual_life_stage == "subadult") %>% 
  ggplot(aes(x = visit_date, y = total_count)) +
  geom_line(group = 1) +
  geom_point() +
  labs(x = "Visit date", y = "Subadult count") +
  facet_grid(. ~ site_id) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("C")

p_adults / p_tads / p_subs
ggsave(here("out", "survey_counts.png"), height = 9, width = 6.5, units = "in")