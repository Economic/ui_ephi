# ephi data
ephi <- read_csv("ephi.csv")

# state industry predictions
# when should the start period be?
predictions <- state_industry_predictions %>% 
  # going to eliminate mar 14 for these calculations 
  filter(endweek >= ymd("2020-03-21")) %>% 
  # sum ic across weeks
  group_by(stateabb, statename, statefips, sector, sectorname) %>% 
  summarize(ic = sum(ic), emp = first(emp)) %>% 
  ungroup()

# Table 1. National industry-specific and overall changes
ephi_industry_pt1 <- predictions %>% 
  group_by(sector) %>% 
  summarize(sectorname = first(sectorname), emp_change = sum(ic), emp = sum(emp)) %>% 
  inner_join(ephi, by = "sector") %>% 
  transmute(
    sectorname = sectorname,
    emp_change = emp_change,
    ephi_rate = ephi_rate,
    ephi_change = emp_change * ephi_rate,
    emp_change_share = emp_change / emp
  )

ephi_industry <- predictions %>% 
  summarize(emp_change = sum(ic), emp = sum(emp)) %>% 
  transmute(
    sectorname = "All industries",
    emp_change = emp_change,
    emp_change_share = emp_change / emp
  ) %>%
  bind_rows(ephi_industry_pt1) %>% 
  mutate(ephi_change = ifelse(is.na(ephi_change), sum(ephi_change, na.rm=TRUE), ephi_change)) %>% 
  mutate(ephi_rate = ifelse(is.na(ephi_rate), ephi_change / emp_change, ephi_rate)) %>% 
  mutate(order = ifelse(sectorname == "All industries", 99, 0)) %>% 
  arrange(order, sectorname) %>% 
  select(-order)

write_csv(ephi_industry, "ephi_industry.csv")  

# Table 2 / Figure A: state-specific changes
ephi_state <- predictions %>% 
  inner_join(ephi, by = "sector") %>% 
  mutate(ephi_change = ephi_rate * ic) %>% 
  group_by(statename) %>% 
  summarize(
    emp = sum(emp), 
    emp_change = sum(ic), 
    ephi_change = sum(ephi_change)
  ) %>% 
  mutate(emp_change_share = emp_change / emp) %>% 
  select(statename, emp_change, ephi_change, emp_change_share) %>% 
  mutate(statename = ifelse(statename == "U.S. Virgin Islands", "Virgin Islands", statename))

write_csv(ephi_state, "ephi_state.csv")
