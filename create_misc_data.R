# assemble state codes & names
state_names <- fips_codes %>% 
  mutate(statefips = as.numeric(state_code)) %>% 
  group_by(statefips) %>% 
  summarize(statename = first(state_name), stateabb = first(state)) 

# grab 2-digit QCEW employment
# source:
qcew_2digit <- read_csv("qcew_naics_2digit.csv") %>% 
  transmute(
    statefips = as.numeric(str_sub(area_fips, end = 2)),
    sector = industry_code,
    emp = emp
  ) %>% 
  inner_join(state_names, by = "statefips") %>% 
  select(statefips, stateabb, statename, sector, emp)


