# grab total IC from state-level reports to DOL
# data from https://oui.doleta.gov/unemploy/csv/ar539.csv
state_total_ic <- read_csv("ar539.csv") %>%
  transmute(
    stateabb = st,
    endweek = mdy(rptdate),
    # add initial claims and WS initial claims
    # see https://oui.doleta.gov/dmstree/handbooks/402/402_4/4024c6/4024c6.pdf#ETA539-ar539
    ic_headline = c3 + c7
  ) %>% 
  filter(endweek >= mdy("03-14-20"))


# process new data and add in PUA claims
# pua_claims <- read_csv("initialclaims_PUAclaims_may14.csv") %>% 
#   select(statename = STATE, pua_ic0502 = may2_pua_claims, pua_ic0509 = may9_pua_claims) %>% 
#   pivot_longer(-statename, names_to = "endweek", names_prefix = "pua_ic", values_to = "pua_ic") %>% 
#   mutate(month = str_sub(endweek,1,2), day = str_sub(endweek,3,4)) %>% 
#   mutate(endweek = mdy(paste(month, day, "2020", sep = "-"))) %>% 
#   inner_join(state_names, by = "statename") %>% 
#   select(stateabb, endweek, pua_ic)

new_ic <- read_csv("initialclaims_PUAclaims_may14.csv") %>% 
  select(statename = STATE, ic_headline = may9_advance) %>% 
  mutate(endweek = ymd("2020-05-23")) %>% 
  inner_join(state_names, by = "statename") %>% 
  select(stateabb, endweek, ic_headline)

# add new data for week ending 05-23-20
state_total_ic <- state_total_ic %>% 
  bind_rows(new_ic) %>% 
#  left_join(pua_claims) %>% 
#  mutate(pua_ic = ifelse(is.na(pua_ic), 0, pua_ic)) %>% 
#  mutate(ic_headline = ic_headline + pua_ic) %>% 
  select(stateabb, endweek, ic_headline)

# process UI data by industry
# use semi-consistent data from
# https://raw.githubusercontent.com/Economic/ui_state_detailed/master/output/state_ui_industry_recoded.csv
# WY - mixture of 2digit and high level <- drop this
state_ui_industry <- read_csv("state_ui_industry_recoded.csv") %>%
  # begin at week ending 3/14
  filter(endweek >= ymd("2020-03-14")) %>% 
  # end at week ending 5/2 because we have not collected balanced panel since then
  filter(endweek <= ymd("2020-05-02")) %>% 
  # drop Wyoming because of non 2digit coding
  filter(stateabb != "WY") %>% 
  # drop NM because not available through 5/2
  filter(stateabb != "NM")

# grab sector names
sectornames <- state_ui_industry %>% 
  group_by(sector, sectorname) %>% 
  summarize(n()) %>% 
  filter(sector != "22-23") %>% 
  select(sector, sectorname)

# assemble ic / emp shares to use as the basis of shocks
# sum all claims across states
shocks <- state_ui_industry %>% 
  group_by(endweek, sector) %>% 
  summarize(ic = sum(ic), emp = sum(emp)) %>% 
  ungroup() %>% 
  transmute(
    endweek = endweek,
    sector = sector,
    basis_shock_industry = ic / emp
  )

# now define 05-09 through 05-23 shocks = 05-02 shock
shocks_0502 <- shocks %>% 
  filter(endweek == ymd("2020-05-02"))

shocks_0509 <- shocks_0502 %>% 
  mutate(endweek = ymd("2020-05-09")) 

shocks_0516 <- shocks_0502 %>% 
  mutate(endweek = ymd("2020-05-16"))   

shocks_0523 <- shocks_0502 %>% 
  mutate(endweek = ymd("2020-05-23")) 

shocks <- shocks %>% 
  bind_rows(shocks_0509, shocks_0516, shocks_0523)

# NY doesn't separate construction & utilities
# allocate NY claims between these sectors using allocation in other states
# accounting for different industry mix in NY
# first grab con util shocks
shocks_conutil <- shocks %>% 
  filter(sector == "22" | sector == "23")

# grab state-level headline numbers for states
state_headlines <- qcew_2digit %>% 
  group_by(stateabb) %>% 
  summarize(emp_state_total = sum(emp)) %>% 
  inner_join(state_total_ic, by="stateabb")

ny_headlines <- state_headlines %>% 
  filter(stateabb == "NY")

# allocate shocks to NY
ny_conutil <- state_ui_industry %>% 
  filter(stateabb == "NY" & sector == "22-23") %>% 
  transmute(
    stateabb = stateabb,
    endweek = endweek,
    ic_headline = ic,
  ) %>% 
  inner_join(qcew_2digit, by = "stateabb") %>%
  filter(sector == "22" | sector == "23") %>% 
  select(stateabb, endweek, ic_headline, emp, sector) %>% 
  inner_join(shocks_conutil, by = c("endweek", "sector")) %>% 
  group_by(stateabb, endweek) %>% 
  mutate(state_shock_industry = basis_shock_industry * ic_headline / sum(basis_shock_industry * emp)) %>% 
  ungroup() %>% 
  transmute(
    stateabb = stateabb,
    statename = "New York",
    statefips = 36,
    sector = sector,
    sectorname = ifelse(sector == "22", "Utilities", "Construction"),
    endweek = endweek,
    ic = state_shock_industry * emp,
    emp = emp,
    ic_share_emp = state_shock_industry
  ) %>% 
  inner_join(ny_headlines, by = c("stateabb", "endweek")) %>% 
  mutate(ic_share_sumic = ic / ic_headline)

# replace the aggregated 22-23 sector for NY with
# new separated 22 and 23 sectors
state_ui_industry_adj <- state_ui_industry %>% 
  filter(!(stateabb == "NY" & sector == "22-23")) %>% 
  rbind(., ny_conutil)

# allocate shocks everywhere
state_industry_predictions <- state_total_ic %>% 
  inner_join(qcew_2digit, by = "stateabb") %>%
  select(stateabb, endweek, ic_headline, emp, sector) %>% 
  inner_join(shocks, by = c("endweek", "sector")) %>% 
  group_by(stateabb, endweek) %>% 
  mutate(state_shock_industry = basis_shock_industry * ic_headline / sum(basis_shock_industry * emp)) %>% 
  ungroup() %>% 
  inner_join(state_names, by = "stateabb") %>% 
  mutate(
    ic = state_shock_industry * emp,
    ic_share_sumic = ic / ic_headline
  ) %>% 
  rename(ic_share_emp = state_shock_industry) %>% 
  inner_join(state_headlines, by = c("stateabb", "endweek")) %>% 
  rename(ic_headline = ic_headline.x) %>% 
  select(-ic_headline.y) %>% 
  inner_join(sectornames, by = "sector") %>% 
  select(stateabb, statename, statefips, sector, sectorname, endweek, ic, ic_share_sumic, ic_headline, emp, ic_share_emp, emp_state_total)

