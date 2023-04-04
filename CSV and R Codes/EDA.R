library(dplyr)
library(ggplot2)
library(tidyr)




#data extraction and cleaning
#__________________________________________________________________________________________________________________________________________________

#warning: entries may not be classed as numeric.
census <- type.convert(read.csv('SOA_census.csv', row.names = 1), as.is = T)
hazards <- data.frame(read.csv('SOA_hazards.csv'))
rates <- data.frame(read.csv('SOA_rates.csv'))



property.value <- census[-c(1:30), ] %>% #remove unwanted rows
  mutate(across(Region.1:Region.6, ~. *100))#convert from decimal to percentage

PVD_df <- data.frame(
  Region_1 = c(property.value[1,2], sum(property.value[c(2:4), 2]), sum(property.value[c(5:7), 2]), sum(property.value[c(8:10), 2]), sum(property.value[c(11:13), 2])),
  Region_2 = c(property.value[1,3], sum(property.value[c(2:4), 3]), sum(property.value[c(5:7), 3]), sum(property.value[c(8:10), 3]), sum(property.value[c(11:13), 3])), 
  Region_3 = c(property.value[1,4], sum(property.value[c(2:4), 4]), sum(property.value[c(5:7), 4]), sum(property.value[c(8:10), 4]), sum(property.value[c(11:13), 4])),
  Region_4 = c(property.value[1,5], sum(property.value[c(2:4), 5]), sum(property.value[c(5:7), 5]), sum(property.value[c(8:10), 5]), sum(property.value[c(11:13), 5])),
  Region_5 = c(property.value[1,6], sum(property.value[c(2:4), 6]), sum(property.value[c(5:7), 6]), sum(property.value[c(8:10), 6]), sum(property.value[c(11:13), 6])),
  Region_6 = c(property.value[1,7], sum(property.value[c(2:4), 7]), sum(property.value[c(5:7), 7]), sum(property.value[c(8:10), 7]), sum(property.value[c(11:13), 7])),
  row.names = c("<50", "50-199", "200-399", "400-999", ">1000")
)

PVD_df <- tibble::rownames_to_column(PVD_df, "PVD") #change format: rownames to column so ggplot2 can plot
  
PVD_final <- pivot_longer(PVD_df, cols=2:7, names_to = "Region", values_to = "Percent")
#let's look at region 1

(plot_PVD <- ggplot(PVD_final, aes(x = factor(PVD, level = c("<50", "50-199", "200-399", "400-999", ">1000")), y = Percent, fill = Region)) +
    geom_bar(stat ="identity", position = position_dodge(), colour = "black") +
    scale_fill_brewer() +
    theme_minimal() +
  ggtitle("Property Value Distribution by Region") +
  xlab("Property Value (Ꝕ1000)") +
  ylab("Percentage of Homes in Value Category")
)


  










  


