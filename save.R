library(here)
library(tidyverse)
library(lubridate)

source(here::here("systems", "zfish_main", "download_attachments.R"))
source(here::here("systems", "zfish_juvenile", "juvenile.R"))
source(here::here("systems", "axo_main", "axo_main.R"))
source(here::here("systems", "davis_axo_main", "davis_axo_main.R"))
source(here::here("systems", "davis_axo_breeder", "davis_axo_breeder.R"))
source(here::here("systems", "atk", "get_atk_daily_log.R"))
source(here::here("systems", "stand_alone_systems", "read.R"))
systems = splice(zfish_main = zfish_main, zfish_juvenile = zfish_juvenile, davis_axo_main = davis_axo_main, davis_axo_breeder = davis_axo_breeder, axo_main = axo_main, atk = atk, dwp)
save(systems, file = "systems.Rdata")





