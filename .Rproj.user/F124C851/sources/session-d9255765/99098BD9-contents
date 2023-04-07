
library(dplyr)
library(ggplot2)
library(tidyr)
library(MASS)
library(EnvStats)
library(goftest)
library(actuar)
library(fitdistrplus)
# library(purrr)

options(scipen=999)
set.seed(123)

hazards <- read.csv('SOA_hazards.csv') %>% arrange(Year) %>% filter(Property.Damage != 0)
census <- type.convert(read.csv('SOA_census.csv', row.names = 1), as.is = T)

#_______DATA SET UP______________________________________________________________________________________________________####
census[,] <- sapply(census[, ], as.numeric) 
str(census)

clusters <- hazards %>%
  mutate(Hazard.Event = recode(Hazard.Event,
                               "Hail/ Wind" = "Severe Weather",                                                         
                               "Winter Weather"= "Severe Weather",                                                 
                               "Wind" = "Severe Weather",                                                          
                               "Hail/ Severe Storm/Thunder Storm" = "Severe Weather",                                  
                               "Severe Storm/Thunder Storm" = "Severe Weather",                                         
                               "Hail" = "Severe Weather",                                                         
                               "Severe Storm/Thunder Storm/ Wind" = "Severe Weather",                                 
                               "Lightning" = "Severe Weather",                                                          
                               "Lightning/ Wind" = "Severe Weather",                                                    
                               "Tornado"  = "Tornado",                                                          
                               "Coastal/ Wind" = "Severe Weather",                                                    
                               "Hurricane/Tropical Storm" = "Hurricane",                                           
                               "Flooding" = "Flooding",                                                            
                               "Hail/ Lightning/ Wind" = "Severe Weather",                                             
                               "Hail/ Severe Storm/Thunder Storm/ Wind" = "Severe Weather",                             
                               "Lightning/ Severe Storm/Thunder Storm" = "Severe Weather",                             
                               "Wildfire" = "Wildfire",                                                          
                               "Hail/ Lightning/ Severe Storm/Thunder Storm" = "Severe Weather",                      
                               "Hail/ Lightning/ Severe Storm/Thunder Storm/ Wind" = "Severe Weather",                
                               "Tornado/ Wind" = "Tornado",                                                      
                               "Hail/ Tornado/ Wind" = "Tornado",                                                
                               "Lightning/ Severe Storm/Thunder Storm/ Wind" = "Severe Weather",                        
                               "Severe Storm/Thunder Storm/ Winter Weather" = "Severe Weather",                         
                               "Wind/ Winter Weather" = "Severe Weather",                                              
                               "Coastal/ Hurricane/Tropical Storm/ Wind" = "Hurricane",                            
                               "Severe Storm/Thunder Storm/ Wind/ Winter Weather" = "Severe Weather",                  
                               "Flooding/ Severe Storm/Thunder Storm" = "Flooding",                               
                               "Coastal" = "Coastal",                                                            
                               "Hail/ Lightning" = "Severe Weather",                                                  
                               "Flooding/ Lightning/ Severe Storm/Thunder Storm" = "Flooding",                   
                               "Heat" = "Drought/ Heat",                                                             
                               "Drought/ Heat" = "Drought/ Heat",                                                    
                               "Flooding/ Severe Storm/Thunder Storm/ Wind" = "Flooding",                        
                               "Flooding/ Wind" = "Flooding",                                                     
                               "Drought" = "Drought/ Heat",                                                          
                               "Hurricane/Tropical Storm/ Severe Storm/Thunder Storm" = "Hurricane",               
                               "Coastal/ Flooding" = "Coastal",                                                  
                               "Flooding/ Lightning" = "Flooding",                                               
                               "Coastal/ Severe Storm/Thunder Storm/ Wind" = "Coastal",                          
                               "Coastal/ Severe Storm/Thunder Storm" = "Coastal",                              
                               "Coastal/ Flooding/ Severe Storm/Thunder Storm/ Wind" = "Coastal",               
                               "Lightning/ Tornado/ Wind" = "Tornado",                                           
                               "Flooding/ Lightning/ Wind" = "Flooding",                                         
                               "Flooding/ Hail"= "Flooding",                                                   
                               "Hail/ Tornado" = "Tornado",                                                      
                               "Hail/ Severe Storm/Thunder Storm/ Wind/ Winter Weather" = "Severe Weather",            
                               "Coastal/ Hurricane/Tropical Storm/ Severe Storm/Thunder Storm/ Wind" = "Hurricane",
                               "Flooding/ Hail/ Wind"= "Flooding",                                               
                               "Fog" = "Fog",                                                              
                               "Severe Storm/Thunder Storm - Wind" = "Severe Weather"
  )) %>% filter(Hazard.Event != "Fog")

summary(clusters)
unique(clusters$Hazard.Event)

#_______HAZARD ANALYSIS__________________________________________________________________________________________________####
# frequency by hazard to determine the level of hazard
frequency_hazard <- clusters %>% group_by(clusters$Hazard.Event) %>% summarise(events.pa = n(), prop_damage = sum(Property.Damage), 
                                                                               damage_per_event = sum(Property.Damage)/n(), risk_level = ifelse(damage_per_event <= 1000000,"minor",
                                                                                                                                                ifelse(damage_per_event<= 10000000,"medium","major")))
# find the mean of losses for each risk level
(minor_mean <- frequency_hazard %>% group_by(risk_level) %>% filter(risk_level == "minor") %>% summarise(dam = mean(damage_per_event))) #517881
(medium_mean <- frequency_hazard %>% group_by(risk_level) %>% filter(risk_level == "medium") %>% summarise(dam = mean(damage_per_event))) #4345546
(major_mean <- frequency_hazard %>% group_by(risk_level) %>% filter(risk_level == "major") %>% summarise(dam = mean(damage_per_event))) #134796018

# assign weighting for each risk
damage_table <- rbind(minor_mean, medium_mean, major_mean)
damage_table

weighting_minor <- as.numeric(damage_table[1,2]/sum(damage_table[,2]))
weighting_medium <- as.numeric(damage_table[2,2]/sum(damage_table[,2]))
weighting_major <- as.numeric(damage_table[3,2]/sum(damage_table[,2]))

sum(weighting_minor+weighting_medium+weighting_major)

# frequency by region
frequency_region <- clusters %>% group_by(Region,Year) %>% dplyr::summarise(tot_minor = sum(Hazard.Event == "Severe Weather"),
                                                                            tot_medium = (sum(Hazard.Event == "Coastal")+sum( Hazard.Event == "Drought/ Heat")+
                                                                                            sum(Hazard.Event == "Flooding")+sum(Hazard.Event == "Tornado")),
                                                                            tot_major = (sum(Hazard.Event == "Wildfire")+sum( Hazard.Event == "Hurricane")),
                                                                            tot_event = tot_minor+ tot_medium+ tot_major,
                                                                            prop_damage = sum(Property.Damage), .groups='drop')



## quantify the region to low risk, med risk and high risk
# checking per region the no of minor, med and major risk and the prop damage
analyse_reg1 <- frequency_region %>% filter(Region==1) %>% summarise(Region = "Region 1", minor = sum(tot_minor), medium = sum(tot_medium),
                                                                     major = sum(tot_major), sum_damage = sum(prop_damage))
analyse_reg2 <- frequency_region %>% filter(Region==2) %>% summarise(Region ="Region 2",minor = sum(tot_minor), medium = sum(tot_medium),
                                                                     major = sum(tot_major), sum_damage = sum(prop_damage))
analyse_reg3 <- frequency_region %>% filter(Region==3) %>% summarise(Region ="Region 3",minor = sum(tot_minor), medium = sum(tot_medium),
                                                                     major = sum(tot_major), sum_damage = sum(prop_damage))
analyse_reg4 <- frequency_region %>% filter(Region==4) %>% summarise(Region ="Region 4",minor = sum(tot_minor), medium = sum(tot_medium),
                                                                     major = sum(tot_major), sum_damage = sum(prop_damage))
analyse_reg5 <- frequency_region %>% filter(Region==5) %>% summarise(Region ="Region 5",minor = sum(tot_minor), medium = sum(tot_medium),
                                                                     major = sum(tot_major), sum_damage = sum(prop_damage))
analyse_reg6 <- frequency_region %>% filter(Region==6) %>% summarise(Region ="Region 6",minor = sum(tot_minor), medium = sum(tot_medium),
                                                                     major = sum(tot_major), sum_damage = sum(prop_damage))

region_analysis <- rbind(analyse_reg1,analyse_reg2,analyse_reg3,analyse_reg4,analyse_reg5,analyse_reg6)

# find the mean and median for each classes of event per region
(minor_med <- median(region_analysis$minor)) #379
(medium_med <- median(region_analysis$medium)) #117.5
(major_med <- median(region_analysis$major)) #11

(minor_mean <- mean(region_analysis$minor)) #369.833
(medium_mean <- mean(region_analysis$medium)) #121
(major_mean <- mean(region_analysis$major)) #14.333

damage_r1 <- region_analysis %>% filter(Region == "Region 1") %>% summarise(damage = minor*weighting_minor + medium*weighting_medium + major*weighting_major)
damage_r2 <- region_analysis %>% filter(Region == "Region 2") %>% summarise(damage = minor*weighting_minor + medium*weighting_medium + major*weighting_major)
damage_r3 <- region_analysis %>% filter(Region == "Region 3") %>% summarise(damage = minor*weighting_minor + medium*weighting_medium + major*weighting_major)
damage_r4 <- region_analysis %>% filter(Region == "Region 4") %>% summarise(damage = minor*weighting_minor + medium*weighting_medium + major*weighting_major)
damage_r5 <- region_analysis %>% filter(Region == "Region 5") %>% summarise(damage = minor*weighting_minor + medium*weighting_medium + major*weighting_major)
damage_r6 <- region_analysis %>% filter(Region == "Region 6") %>% summarise(damage = minor*weighting_minor + medium*weighting_medium + major*weighting_major)

(reg_damage_analysis <- rbind(damage_r1, damage_r2, damage_r3, damage_r4, damage_r5, damage_r6))
# damage
# <dbl>
# 1                           15.1 
# 2                           40.4 
# 3                           19.3 
# 4                           15.2 
# 5                           14.0 
# 6                            9.72

quantile(reg_damage_analysis$damage,c(.33, .67))
#     33%      67% 
# 14.74977 16.65021 

region_risk <- ifelse(reg_damage_analysis$damage <= quantile(reg_damage_analysis$damage,c(.33)), "Low Risk", 
                      ifelse(reg_damage_analysis$damage <= quantile(reg_damage_analysis$damage,c(.67)), "Medium Risk","High Risk"))

# Result
# Region 1: Medium risk
# Region 2: High risk
# Region 3: High risk
# Region 4: Medium risk
# Region 5: Low risk
# Region 6: Low risk


#_______CENSUS DATA ANALYSIS (IN PROGRESS)_____________________________________________________________________________________________####
pop_2021 <- census[1,]
pop_2020 <- census[2,]
gdp_2020 <- census[28,]
temp_housing_cost_wdis <- census[30,]
median_mthly_housing_cost<- census[9,]
person_per_hh <- census[19,]
no_hh <- census[18,]
housing_units <- census[6,]
owner_occupied_housing_units <- census[7,]


## property value
property.value <- census[-c(1:30), ] %>% #remove unwanted rows
  mutate(across(Region.1:Region.6, ~. *100))#convert from decimal to percentage

PVD_df <- data.frame(
  Region_1 = c(property.value[1,1], sum(property.value[c(2:4), 1]), sum(property.value[c(5:7), 1]), sum(property.value[c(8:10), 1]), sum(property.value[c(11:13), 1])),
  Region_2 = c(property.value[1,2], sum(property.value[c(2:4), 2]), sum(property.value[c(5:7), 2]), sum(property.value[c(8:10), 2]), sum(property.value[c(11:13), 2])),
  Region_3 = c(property.value[1,3], sum(property.value[c(2:4), 3]), sum(property.value[c(5:7), 3]), sum(property.value[c(8:10), 3]), sum(property.value[c(11:13), 3])), 
  Region_4 = c(property.value[1,4], sum(property.value[c(2:4), 4]), sum(property.value[c(5:7), 4]), sum(property.value[c(8:10), 4]), sum(property.value[c(11:13), 4])),
  Region_5 = c(property.value[1,5], sum(property.value[c(2:4), 5]), sum(property.value[c(5:7), 5]), sum(property.value[c(8:10), 5]), sum(property.value[c(11:13), 5])),
  Region_6 = c(property.value[1,6], sum(property.value[c(2:4), 6]), sum(property.value[c(5:7), 6]), sum(property.value[c(8:10), 6]), sum(property.value[c(11:13), 6])), 
  row.names = c("<50", "50-199", "200-399", "400-999", ">1000"))

PVD_df <- tibble::rownames_to_column(PVD_df, "PVD") #change format: rownames to column so ggplot2 can plot

PVD_final <- pivot_longer(PVD_df, cols=2:7, names_to = "Region", values_to = "Percent")
#let's look at region 1

(plot_PVD <- ggplot(PVD_final, aes(x = factor(PVD, level = c("<50", "50-199", "200-399", "400-999", ">1000")), y = Percent, fill = Region)) +
    geom_bar(stat ="identity", position = position_dodge(), colour = "black") +
    scale_fill_brewer() +
    theme_minimal() +
    ggtitle("Property Value Distribution by Region") +
    xlab("Property Value (P1000)") +
    ylab("Percentage of Homes in Value Category")
)

# find out how many person live alone and with fam
(person_with_fam <- person_per_hh*no_hh)
# Region.1 Region.2 Region.3 Region.4 Region.5 Region.6
# 2016-2020  6059259  4070224  4608368  1008870  1251120 291637.8

(person_living_alone <- pop_2020-person_with_fam )
# Region.1 Region.2 Region.3 Region.4 Region.5 Region.6
# 247149 142124.3 385396.1     1806    15552  16246.2

# find out no of houses for rent
(rent_house <- housing_units - owner_occupied_housing_units )
# Region.1 Region.2 Region.3 Region.4 Region.5 Region.6
# Rent Units   887823   661218   889440   162868   201141    32787

# no of person per house in the region
(no_person_per_house <- pop_2020/housing_units) 
# Region.1 Region.2 Region.3 Region.4 Region.5 Region.6
#2.258826 1.669095 2.257032 2.035404 2.235598 2.272542

# check if there is enough no of accommodation for voluntary relocation
(person_per_hh)
# assume one house can have as much people as persons in a hh
# assume those living alone is renting

# no of available space for rent 
tot_rental <- person_per_hh*rent_house
tot_rental_occupied <- person_living_alone
tot_rental_available <- tot_rental - tot_rental_occupied

# population
pop_table <- t(rbind(pop_2020, gdp_2020, temp_housing_cost_wdis, median_mthly_housing_cost))
colnames(pop_table) <- c("Population_2020", "GDP_2020( P1,000 )", "Temp Housing Cost After Disaster", "Median Mthly Housing Cost (P pp per mth)" )


cens_df <- (rbind(pop_2020, person_with_fam, person_living_alone, rent_house, tot_rental_occupied, tot_rental_available))
rownames(cens_df) <- c("Population 2020", "Person with fam", "Person live alone", "No House for rent", "Rental Occupied", "Rental Available")
cens_df

#_______FREQUENCY________________________________________________________________________________________________________#####

# frequency by year

frequency <- hazards %>% group_by(Year) %>% summarise(events.pa = n())

#poisson
(fit_poisson <- fitdistr(frequency$events.pa, "Poisson"))

(ks_poisson <- ks.test(unique(frequency$events.pa), "ppois", lambda = 49.704918)) # good fit
(ad_poisson <- ad.test(frequency$events.pa, "ppois", lambda = 49.704918)) # good fit
(cvm_poisson <- cvm.test(frequency$events.pa, "ppois", lambda = 49.704918)) # good fit

#negative binomial
(fit_nbin <- fitdistr(frequency$events.pa, "negative binomial"))

(ks_nbin <- ks.test(unique(frequency$events.pa), "pnbinom", size = 1.948557, mu = 49.704918)) 
(ad_nbin <- ad.test(frequency$events.pa, "pnbinom", size = 1.948557, mu = 49.704918))
(cvm_nbin <- cvm.test(frequency$events.pa, "pnbinom", size = 1.948557, mu = 49.704918))

#_______SEVERITY_________________________________________________________________________________________________________####

severity <- hazards %>% dplyr::select(Property.Damage) #select() function in dplyr clashes with MASS

(severity_plot <- ggplot(severity, aes(x = Property.Damage)) + geom_histogram(bins = 100) + xlim(c(0, 1000000))) #very skewed distribution

(FG <- egamma(severity$Property.Damage, method = "mle")) 

(FGP <- ggplot(severity, aes(sample = Property.Damage)) +
    stat_qq(distribution = qgamma, dparams = c(shape = 1.001714e+00, rate = 1/6.534646e+06), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Gamma") +
    xlab("Theoretical") +
    ylab("Sample")
)

#mle gamma fit highly underestimates damages - try log of gamma

sevlog <- severity %>% mutate(Property.Damage = log(Property.Damage))
(FGL <- egamma(sevlog$Property.Damage, method = "mle")) 

(FGLP <- ggplot(sevlog, aes(sample = Property.Damage)) +
    stat_qq(distribution = qgamma, dparams = c(shape = 14.8283973, rate = 1/0.7073224), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Ln(Damages) ~ Gamma") +
    xlab("Theoretical") +
    ylab("Sample")
)

#much better fit! Now try lognormal


(FL <- fitdistr(severity$Property.Damage, "lognormal"))

(FLP <- ggplot(severity, aes(sample = Property.Damage)) +
    stat_qq(distribution = qlnorm, dparams = c(meanlog = 10.48845700, sdlog = 2.61966147), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Lognormal") +
    xlab("Theoretical") +
    ylab("Sample")
)

#lognormal also systematically underestimates damages


#now split by region:
sev_r1 <- hazards  %>% filter(Region == "1") %>% dplyr::select(Property.Damage) %>% mutate(Property.Damage = log(Property.Damage))  
sev_r2 <- hazards  %>% filter(Region == "2") %>% dplyr::select(Property.Damage) %>% mutate(Property.Damage = log(Property.Damage))
sev_r3 <- hazards  %>% filter(Region == "3") %>% dplyr::select(Property.Damage) %>% mutate(Property.Damage = log(Property.Damage))
sev_r4 <- hazards  %>% filter(Region == "4") %>% dplyr::select(Property.Damage) %>% mutate(Property.Damage = log(Property.Damage))
sev_r5 <- hazards  %>% filter(Region == "5") %>% dplyr::select(Property.Damage) %>% mutate(Property.Damage = log(Property.Damage))
sev_r6 <- hazards  %>% filter(Region == "6") %>% dplyr::select(Property.Damage) %>% mutate(Property.Damage = log(Property.Damage))

(FG1 <- egamma(sev_r1$Property.Damage, method = "mle")) 
(FG2 <- egamma(sev_r2$Property.Damage, method = "mle")) 
(FG3 <- egamma(sev_r3$Property.Damage, method = "mle")) 
(FG4 <- egamma(sev_r4$Property.Damage, method = "mle")) 
(FG5 <- egamma(sev_r5$Property.Damage, method = "mle")) 
(FG6 <- egamma(sev_r6$Property.Damage, method = "mle")) 


(FGP1 <- ggplot(sev_r1, aes(sample = Property.Damage)) +
    stat_qq(distribution = qgamma, dparams = c(shape = 13.9447973, scale = 0.7622203), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Log Gamma R1") +
    xlab("Theoretical") +
    ylab("Sample")
)
(FGP2 <- ggplot(sev_r2, aes(sample = Property.Damage)) +
    stat_qq(distribution = qgamma, dparams = c(shape = 17.9265132, scale = 0.6116642), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Log Gamma R2") +
    xlab("Theoretical") +
    ylab("Sample")
)
(FGP3 <- ggplot(sev_r3, aes(sample = Property.Damage)) +
    stat_qq(distribution = qgamma, dparams = c(shape = 14.2931748, scale = 0.7238527), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Log Gamma R3") +
    xlab("Theoretical") +
    ylab("Sample")
)
(FGP4 <- ggplot(sev_r4, aes(sample = Property.Damage)) +
    stat_qq(distribution = qgamma, dparams = c(shape = 15.7694419, scale = 0.6494495), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Log Gamma R4") +
    xlab("Theoretical") +
    ylab("Sample")
)
(FGP5 <- ggplot(sev_r5, aes(sample = Property.Damage)) +
    stat_qq(distribution = qgamma, dparams = c(shape = 15.0962915, scale = 0.6756838), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Log Gamma R5") +
    xlab("Theoretical") +
    ylab("Sample")
)

(FGP6 <- ggplot(sev_r6, aes(sample = Property.Damage)) +
    stat_qq(distribution = qgamma, dparams = c(shape = 11.5229591, scale = 0.8736234), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Log Gamma R6") +
    xlab("Theoretical") +
    ylab("Sample")
)

#ad test the above


(ad1 <- ad.test(sev_r1$Property.Damage, "pgamma", shape = 13.9447973, scale = 0.7622203))
(ad2 <- ad.test(sev_r2$Property.Damage, "pgamma", shape = 17.9265132, scale = 0.6116642))
(ad3 <- ad.test(sev_r3$Property.Damage, "pgamma", shape = 14.2931748, scale = 0.7238527))
(ad4 <- ad.test(sev_r4$Property.Damage, "pgamma", shape = 15.7694419, scale = 0.6494495))
(ad5 <- ad.test(sev_r5$Property.Damage, "pgamma", shape = 15.0962915, scale = 0.6756838))
(ad6 <- ad.test(sev_r6$Property.Damage, "pgamma", shape = 11.5229591, scale = 0.8736234))

#not great, but good enough 

#_______Monte-Carlo simulation: average aggregate loss in n_years, using MLE parameters_____________________________####

n_years <- 10 #feel free to adjust: e.g. if you want aggregate loss in 5 years, put n_years <- 5
n_iterations <- 100000 #number of Monte_Carlo simulations: also feel free to adjust.
size <- 1.948557
mu <- 49.704918
shapelog <- 14.829296
ratelog <- 1.413602

sev <- c()

for (i in 1:n_iterations) {
  
  for (i in 1:n_years) {
    freq_vec <- rnbinom(n_years, size = size, mu = mu)
    sev_obs <- sum(rlgamma(freq_vec[i], shapelog = shapelog, ratelog = ratelog))
  }
  
  sev <- append(sev, sev_obs)
  
}

#sanity check: mean aggregate loss in n years / cumulative historical property damages: should be about = n_years / 60
(mean(sev)/sum(hazards$Property.Damage))
#I got 0.06579576. For n_years = 10, this should be about 0.1667
#Possible reason for small value: 
#the historical losses include some years where total losses are extreme, which isn't captured by just taking mean(sev)

sev_df <- data.frame(sev)
(sev_hist <- ggplot(sev_df, aes(x = sev)) + 
    geom_histogram(bins = 100) +
    xlim(c(0, 1000000000)) + ylim(c(0,15000)) +
    ggtitle("Aggregate Loss across Instances") +
    xlab("Aggregate Loss in N Years") +
    ylab("Sample")
)

#_______PRICING___________________________________________________________________________####

# AVG Loss
Aggregate_loss <- mean(sev) #2358713725
Loss_pp <- mean(sev)/ sum(pop_2020)
#130.3319












