library(dplyr)

load("./DATA_RAW/Dane_BRCA_comp.rda")

model_data <- dane_brca_comp %>%
  select(-SALARY, -SUBREGION) %>%
  rename(DISTRICT_ID = TERYT4,
         IS_CITY = MnPP,
         SALARY = SALARY_std,
         ALCOHOL = ALKOHOL,
         LATITUDE = Latitude,
         LONGITUDE = Longitude,
         TARGET = NEW) %>%
  mutate(LATITUDE = as.numeric(paste(LATITUDE)),
         LONGITUDE = as.numeric(paste(LONGITUDE)))
  
levels(model_data$GENDER) <- c("F", "M")

save(model_data, file = "./DATA/model_data.rda")

