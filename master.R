library(tidyverse)
library(scales)
library(lubridate)
library(tigris)

# source("process_cps.R")

# inputs:
# qcew_naics_2digit.csv
source("create_misc_data.R")

# inputs: 
#ar539.csv
#state_ic_0509.csv
#state_ui_industry_recoded.csv
source("create_ic_industry.R")

source("analyze_ephi.R")

# publish draft tables
rmarkdown::render("index.Rmd")
draft_server <- Sys.getenv("DRAFT_SERVER")
system(paste0("~/.local/bin/aws s3 cp index.html ", draft_server, "ui_ephi/"))