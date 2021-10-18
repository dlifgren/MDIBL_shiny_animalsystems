path = here::here("systems", "davis_axo_main", "/")

files <- path%>%
  list.files()%>%
  .[str_detect(., ".csv")]

files <- paste0(path, files) 

davis_axo_main <- map(files, read_csv, col_names = FALSE)%>%
  reduce(dplyr::union)%>%
  rename(parameter = X2, date_time = X1, value = X3)%>%
  mutate(parameter = recode(parameter, "1" = "pH", "2" = "Conductivity", "3" = "Water_Temp", "4" = "Water_level", "5" = "Room_Temp"))%>%
  mutate(date = mdy_hms(date_time))%>%
  filter(parameter != "NA", date >= "2020-01-01")%>%
  pivot_wider(names_from = parameter, values_from = value)%>%
  select(pH, Conductivity, Water_Temp, Room_Temp, date)
  