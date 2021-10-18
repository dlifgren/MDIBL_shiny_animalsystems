path = here::here("systems", "zfish_juvenile", "/")

files <- path%>%
  list.files()%>%
  .[str_detect(., ".csv")]

files <- paste0(path, files) 
  
zfish_juvenile <- map(files, read_csv, col_names = FALSE)%>%
  reduce(dplyr::union)%>%
  rename(parameter = X2, date_time = X1, value = X3)%>%
  distinct(across(c(parameter, date_time)), .keep_all = TRUE)%>%
  mutate(parameter = recode(parameter, "1" = "pH", "2" = "Conductivity", "3" = "Water_Temp", "4" = "Water_level", "5" = "Room_Temp"))%>%
  filter(parameter != "NA")%>%
  pivot_wider(names_from = parameter, values_from = value)%>%
  mutate(date = mdy_hms(date_time))%>%
  select(pH, Conductivity, Water_Temp, Room_Temp, date)%>%
  filter(date >= "2020-08-01")


