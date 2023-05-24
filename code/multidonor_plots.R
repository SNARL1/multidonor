if (!require(librarian)){
  install.packages("librarian")
}
librarian::shelf(readr, dplyr, ggplot2, forcats, magrittr, here, patchwork, viridis)

# Create frog count figure
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

# Create donor pie chart  ***REMOVE
pie_donor_10223 <- relocate_plot %>% 
  filter(site_id == 10223) %>% 
  mutate(collect_siteid = as.factor(collect_siteid)) %>% 
  ggplot(aes(x = "", y = release_prop, fill = collect_siteid)) +
  geom_col() +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("yellow", "orange", "red")) +
  geom_text(aes(label = paste0(round(release_prop * 100), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "10223") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = -5))

# Create bar chart of proportion of frogs released vs captured by collect_siteid
relocate_capture_prop <- read_csv(here("data", "clean", "relocate_capture_prop.csv"))
stackbar_relocatecapture1 <- relocate_capture_prop %>% 
  filter(site_id == 10223 | site_id == 10225) %>% 
  mutate(collect_siteid = as.factor(collect_siteid)) %>% 
  ggplot(aes(x = factor(group, level = c("released", "captured")), y = proportion, fill = collect_siteid)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis(discrete = TRUE, na.value = "lightgray") +
  facet_wrap(~site_id) +
  labs(x = "Group", y = "Proportion of frogs", fill = "donor\npopulation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, vjust = 0.7))
stackbar_relocatecapture2 <- relocate_capture_prop %>% 
  filter(site_id == 70470 | site_id == 70481) %>% 
  mutate(collect_siteid = as.factor(collect_siteid)) %>% 
  ggplot(aes(x = factor(group, level = c("released", "captured")), y = proportion, fill = collect_siteid)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis(discrete = TRUE, na.value = "darkgray") +
  facet_wrap(~site_id) +
  labs(x = "Group", y = "Proportion of frogs", fill = "donor\npopulation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, vjust = 0.7))
stackbar_relocatecapture1 + stackbar_relocatecapture2
ggsave(here("out", "stackbar_relocatecapture.png"), height = 5, width = 8, units = "in")
