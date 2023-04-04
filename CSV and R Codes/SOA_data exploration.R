setwd("/Users/apple/Desktop/ACTL4001 Project")
library(dplyr)
library(ggplot2)
library(readr)
library(cluster)
library(factoextra)
library(tidyverse)
library(stringi)

# hazard event 
hazards = read.csv("SOA_hazards.csv")
# hazards$Property.Damage = parse_number(hazards$Property.Damage)
hazards$Region = as.character(hazards$Region)
hazards = hazards %>%
  filter(hazards$Property.Damage > 0 | hazards$Injuries > 0 | hazards$Fatalities > 0)

for (i in 1:nrow(hazards)) {
  # hazards[i,"Hazard.Event"] = unlist(strsplit( hazards[i,"Hazard.Event"], split='/', fixed=TRUE))[1]
  if (stri_detect_fixed(hazards[i,"Hazard.Event"], "Hurricane") == TRUE ) {
    hazards[i,"Hazard.Event"] = "Hurricane" 
  } else if ( stri_detect_fixed(hazards[i,"Hazard.Event"], "Coastal") == TRUE ) {
    hazards[i,"Hazard.Event"] = "Coastal" 
  } else if (stri_detect_fixed((hazards[i,"Hazard.Event"]), "Flooding") == TRUE){
    hazards[i,"Hazard.Event"] = "Flooding" 
  } else if (stri_detect_fixed((hazards[i,"Hazard.Event"]), "Storm") == TRUE || 
             stri_detect_fixed((hazards[i,"Hazard.Event"]), "Hail") == TRUE ||
             stri_detect_fixed((hazards[i,"Hazard.Event"]), "Wind") == TRUE ||
             stri_detect_fixed((hazards[i,"Hazard.Event"]), "Lightning") == TRUE  ||
             stri_detect_fixed((hazards[i,"Hazard.Event"]), "Winter Weather") == TRUE){
    hazards[i,"Hazard.Event"] = "Severe Weather" 
  } else if (stri_detect_fixed((hazards[i,"Hazard.Event"]), "Drought") == TRUE || 
             stri_detect_fixed((hazards[i,"Hazard.Event"]), "Heat") == TRUE) {
             hazards[i,"Hazard.Event"] = "Drought/Heat" 
  }
}

# combine anything with flooding & costal 
# combine storm/hail/wind/lightening as they represent extreme weather conditions cause meteorological hazards. 
hazards.agg <- hazards %>%
  dplyr::select(-c(Year, Region))

hazards.agg2 =  hazards.agg %>% 
  group_by(Hazard.Event) %>% 
  summarize(Property.Damage.agg = sum(Property.Damage), 
         Injuries.agg = sum(Injuries),Fatalities.agg = sum(Fatalities),
         Freq = n()) %>%
  mutate(Property.Damage.avg = Property.Damage.agg/Freq , 
         Injuries.avg = Injuries.agg/Freq,Fatalities.avg =Fatalities.agg/Freq) %>%
  remove_rownames %>% column_to_rownames(var="Hazard.Event")

# drop fog/landslide due to its scarcity 
hazards.agg2 = hazards.agg2[!(row.names(hazards.agg2) %in% c("Fog","Landslide")),]

df = scale(hazards.agg2)
k4 <- kmeans(df[,c("Property.Damage.avg", "Fatalities.avg","Injuries.avg","Freq")], centers = 3)

distance <- get_dist(df)
fviz_cluster(k4, df, geom = "text")

# df %>%
#   as_tibble() %>%
#   mutate(cluster = k4$cluster,
#          state = row.names(df)) %>%
#   ggplot(aes(Fatalities.avg,Property.Damage.avg, color = factor(cluster), label = state)) +
#   geom_text()


census = read.csv("SOA_census.csv") 
rate = read.csv("SOA_rates.csv")
