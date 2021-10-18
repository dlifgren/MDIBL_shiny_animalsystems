library(readxl)


path <- here::here("systems", "weekly_water_chemistry", "Weekly Water Chemistry.xlsx")

wwc <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_xlsx, path = path, .name_repair = "universal")%>%
  keep(names(.) %in% c("Fish Room","Fish Room Juvenile","Backup 1","Backup 2","Quarantine", "ATK", "Main Axolotl",
                       "Davis Axolotl - Main ", "Davis Axolotl - Breeder ", "Davis Axolotl - A2 ", "Davis Axolotl - A3 "))%>%
  map(~rename(.x, date_wwc = Date, nh3 = NH3..ppm., no2 = NO2..ppm., no3 = NO3..ppm., pH_wwc = pH,
              hardness = Hardness..mg.L...multiply.by.17.1, alkalinity = Alkalinity..ppm., 
              oxygen_dissolved = Dissolved.O2..mg.L., initials = Initials))%>%
  map(~select(.x, date_wwc:initials))%>%
  map(~mutate(.x, hardness = as.double(hardness),
              alkalinity = as.double(alkalinity),
              Cu..ppm. = NULL))



names(wwc) <- c("zfish_main", "zfish_juvenile", "Zfish Backup 1", "Zfish Backup 2", "Zfish Quarantine", "atk", "axo_main",
                "davis_axo_main", "davis_axo_breeder", "Axo 2", "Axo 3")


merge_wwc <- function(var){
  systems[[var]]%>%
    mutate(date_date = date(date))%>%
    left_join(wwc[[var]], by = c("date_date" = "date_wwc"))%>%
    select(-date_date)
}
