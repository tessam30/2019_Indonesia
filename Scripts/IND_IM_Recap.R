# Clean up the portoflion data


library(tidyverse)
library(readxl)

pf <- read_excel(file.path(datapath, "IM_RECAP.xlsx"))

# Create a summary of each Province to match the IM totals
# Need to count the IMS per Province
# Tally the numbers per Indicator type per success

pf_long <- 
  pf %>% 
  group_by(Province, Type) %>% 
  mutate(IM_count = n()) %>% 
  ungroup() %>% 
  gather(value = "value", key = "Results", `GoI Political Will`:`Other`) %>% 
  group_by(Province, Results, Type) %>% 
  mutate(result_total = sum(value)) %>% 
  ungroup() %>% 
  mutate(group = case_when(
    Results %in% c("GoI Political Will", "GoI Budget") ~ "Committment",
    Results %in% c("GoI Capacity", "CSO Capacity") ~ "Capacity",
    Results %in% c("Effective Design & Implementation", "Effective Targeting") ~ "USAID Technical Support",
    Results %in% c("Coordination with key stakeholders") ~ "Coordination",
    TRUE ~ "Other"
  ),
  percent = (result_total / IM_count)) 

# Show correct counts
pf_long %>% 
  group_by(Province, Results, Type, group) %>% 
  summarise(results = sum(value)) %>% 
  #filter(Type == "Successes") %>% 
  spread(Province, results) %>% 
  arrange(Type, group)

# Show correct percentages
pf_long %>% 
  group_by(Province, Results, Type, group) %>% 
  summarise(results = mean(percent)) %>% 
  spread(Province, results) %>% 
  arrange(Type, group)

# Show IM list 
pf_long %>% 
  group_by(IM) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  print(n = Inf)

# Proof of conept plot 
pf_long %>% 
  #filter(Type == "Successes") %>% 
  ggplot(aes(x = Results, y = (value)/IM_count)) + geom_col() +
  coord_flip() + theme_minimal() +
  facet_wrap(Province ~ Type)

# Proof of conept plot 
pf_long %>% 
  mutate(sortvar = fct_reorder(Province, IM_count, .desc = TRUE)) %>% 
  #filter(Type == "Successes") %>% 
  ggplot(aes(x = Results, y = value)) + geom_col() +
  coord_flip() + theme_minimal() +
  facet_wrap(sortvar ~ Type)

# Create two extracts, one with Success and Challenges spread, 
# other with them stacked

pf_long %>% 
  group_by(Type, Office, IM, Province, group) %>% 
  summarise(value = mean(value)) %>% 
  spread(Province, value)

write_csv(pf_long, file.path(datapath, "IM_Recap_long.csv"))

