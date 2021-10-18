library(RDCOMOutlook)
library(RDCOMClient)
library(tidyverse)
path = here::here("systems", "zfish_main","data", "/")



t = search_emails(search_term = "Daily Report", scope = "subject")%>%
  filter(from_name == "aquatouch@aquaneering.com")

t$filename = paste0(path,t$received,t$attachments)
t$filename = gsub(':','',gsub(' ','_',t$filename))

download_unzip_rename <- function(x, y) {
  file.rename(save_attachments(x,target_dir = path),y
  )
}

dfs = t[1:nrow(t),]

for (i in seq_along(dfs$email)) {
  print(i)
  email <- dfs$email[[i]]
  filename <- dfs$filename[[i]]
  download_unzip_rename(email, filename)
}

temp = list.files(path)

zfish_main = do.call(bind_rows, lapply(paste0(path,temp), read_csv))%>%
  
  unite("date_time", c(Date, Time), sep = " ")%>%
  mutate(date= mdy_hms(date_time))%>%
  select(PH1, COND1, WATER_TEMP, ROOM_TEMP, FBB1, FF1, date)%>%
  rename(pH = PH1, 
         Conductivity = COND1, 
         Water_Temp = WATER_TEMP, 
         Room_Temp = ROOM_TEMP, 
         FBB_Pressure = FBB1,
         FF_Pressure = FF1)
  
 
