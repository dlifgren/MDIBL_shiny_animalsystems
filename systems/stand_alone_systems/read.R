library(readxl)

path <- here::here("systems", "stand_alone_systems", "data", "2021_Daily_Water_Parameters.xlsx")

dwp <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_xlsx, path = path, .name_repair = "universal")%>%
  keep(names(.) %in% c("Zfish Backup 1","Zfish Backup 2","Zfish Quarantine","Axo 2","Axo 3"))%>%
  map(~select(.x, c("DATE", "pH", "TEMP...C.", "CONDUCTIVITY")))%>%
  map(~rename(.x, date = DATE, Conductivity = CONDUCTIVITY, Water_Temp = TEMP...C.))


