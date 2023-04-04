#Simulation

library(dplyr)
library(ggplot2)
library(tidyr)
library(MASS)
library(EnvStats)
library(goftest)
library(actuar)
library(fitdistrplus)

hazards <- read.csv('SOA_hazards.csv') %>% arrange(Year) %>% filter(Property.Damage != 0)
frequency <- hazards %>% group_by(Year) %>% summarise(events.pa = n())
emissions <- read.csv('SOA_emissions.csv')


freq1 <- hazards %>% filter(Region == 1) %>% group_by(Year) %>% summarise(events.pa = n())
freq2 <- hazards %>% filter(Region == 2) %>% group_by(Year) %>% summarise(events.pa = n())
freq3 <- hazards %>% filter(Region == 3) %>% group_by(Year) %>% summarise(events.pa = n())
freq4 <- hazards %>% filter(Region == 4) %>% group_by(Year) %>% summarise(events.pa = n())
freq5 <- hazards %>% filter(Region == 5) %>% group_by(Year) %>% summarise(events.pa = n())
freq6 <- hazards %>% filter(Region == 6) %>% group_by(Year) %>% summarise(events.pa = n())

lsev1 <- hazards %>% filter(Region == 1) %>% mutate(Property.Damage = log(Property.Damage))
lsev2 <- hazards %>% filter(Region == 2) %>% mutate(Property.Damage = log(Property.Damage))
lsev3 <- hazards %>% filter(Region == 3) %>% mutate(Property.Damage = log(Property.Damage))
lsev4 <- hazards %>% filter(Region == 4) %>% mutate(Property.Damage = log(Property.Damage))
lsev5 <- hazards %>% filter(Region == 5) %>% mutate(Property.Damage = log(Property.Damage))
lsev6 <- hazards %>% filter(Region == 6) %>% mutate(Property.Damage = log(Property.Damage))

#fit historical frequency per region
(nbin1 <- fitdist(freq1$events.pa, "nbinom")) 
(nbin2 <- fitdist(freq2$events.pa, "nbinom"))
(nbin3 <- fitdist(freq3$events.pa, "nbinom"))
(nbin4 <- fitdist(freq4$events.pa, "nbinom"))
(nbin5 <- fitdist(freq5$events.pa, "nbinom"))
(nbin6 <- fitdist(freq6$events.pa, "nbinom"))

#check fit using qq plots
(P1 <- ggplot(freq1, aes(sample = events.pa)) +
    stat_qq(distribution = qnbinom, dparams = c(size = nbin1$estimate[[1]], mu = nbin1$estimate[[2]]), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Negative Binomial Fit: Region 1") +
    xlab("Theoretical") +
    ylab("Empirical")
) 
(P2 <- ggplot(freq2, aes(sample = events.pa)) +
    stat_qq(distribution = qnbinom, dparams = c(size = nbin2$estimate[[1]], mu = nbin2$estimate[[2]]), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Negative Binomial Fit: Region 2") +
    xlab("Theoretical") +
    ylab("Empirical")
) 
(P3 <- ggplot(freq3, aes(sample = events.pa)) +
    stat_qq(distribution = qnbinom, dparams = c(size = nbin3$estimate[[1]], mu = nbin3$estimate[[2]]), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Negative Binomial Fit: Region 3") +
    xlab("Theoretical") +
    ylab("Empirical")
) 
(P4 <- ggplot(freq4, aes(sample = events.pa)) +
    stat_qq(distribution = qnbinom, dparams = c(size = nbin4$estimate[[1]], mu = nbin4$estimate[[2]]), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Negative Binomial Fit: Region 4") +
    xlab("Theoretical") +
    ylab("Empirical")
) 
(P5 <- ggplot(freq5, aes(sample = events.pa)) +
    stat_qq(distribution = qnbinom, dparams = c(size = nbin5$estimate[[1]], mu = nbin5$estimate[[2]]), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Negative Binomial Fit: Region 5") +
    xlab("Theoretical") +
    ylab("Empirical")
) 
(P6 <- ggplot(freq6, aes(sample = events.pa)) +
    stat_qq(distribution = qnbinom, dparams = c(size = nbin6$estimate[[1]], mu = nbin6$estimate[[2]]), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Negative Binomial Fit: Region 6") +
    xlab("Theoretical") +
    ylab("Empirical")
) 
#all good! 
#but frequency per year will increase because of climate change
#multiply the mu parameter by each RAF to get a table of different mus, depending on the climate scenario


freq_RAF1 <- emissions %>% mutate(
  SSP1..2.6 = SSP1..2.6*nbin1$estimate[[2]],
  SSP2.3.4 = SSP2.3.4*nbin1$estimate[[2]],
  SSP3.6.0 = SSP3.6.0*nbin1$estimate[[2]],
  SSP5.Baseline = SSP5.Baseline*nbin1$estimate[[2]],
  .keep = "unused"
)
freq_RAF2 <- emissions %>% mutate(
  SSP1..2.6 = SSP1..2.6*nbin2$estimate[[2]],
  SSP2.3.4 = SSP2.3.4*nbin2$estimate[[2]],
  SSP3.6.0 = SSP3.6.0*nbin2$estimate[[2]],
  SSP5.Baseline = SSP5.Baseline*nbin2$estimate[[2]],
  .keep = "unused"
)
freq_RAF3 <- emissions %>% mutate(
  SSP1..2.6 = SSP1..2.6*nbin3$estimate[[2]],
  SSP2.3.4 = SSP2.3.4*nbin3$estimate[[2]],
  SSP3.6.0 = SSP3.6.0*nbin3$estimate[[2]],
  SSP5.Baseline = SSP5.Baseline*nbin3$estimate[[2]],
  .keep = "unused"
)
freq_RAF4 <- emissions %>% mutate(
  SSP1..2.6 = SSP1..2.6*nbin4$estimate[[2]],
  SSP2.3.4 = SSP2.3.4*nbin4$estimate[[2]],
  SSP3.6.0 = SSP3.6.0*nbin4$estimate[[2]],
  SSP5.Baseline = SSP5.Baseline*nbin4$estimate[[2]],
  .keep = "unused"
)
freq_RAF5 <- emissions %>% mutate(
  SSP1..2.6 = SSP1..2.6*nbin5$estimate[[2]],
  SSP2.3.4 = SSP2.3.4*nbin5$estimate[[2]],
  SSP3.6.0 = SSP3.6.0*nbin5$estimate[[2]],
  SSP5.Baseline = SSP5.Baseline*nbin5$estimate[[2]],
  .keep = "unused"
)
freq_RAF6 <- emissions %>% mutate(
  SSP1..2.6 = SSP1..2.6*nbin6$estimate[[2]],
  SSP2.3.4 = SSP2.3.4*nbin6$estimate[[2]],
  SSP3.6.0 = SSP3.6.0*nbin6$estimate[[2]],
  SSP5.Baseline = SSP5.Baseline*nbin6$estimate[[2]],
  .keep = "unused"
)



#fit severity to log-gamma distribution

gamm1 <- fitdist(lsev1$Property.Damage, "gamma")
gamm2 <- fitdist(lsev2$Property.Damage, "gamma")
gamm3 <- fitdist(lsev3$Property.Damage, "gamma")
gamm4 <- fitdist(lsev4$Property.Damage, "gamma")
gamm5 <- fitdist(lsev5$Property.Damage, "gamma")
gamm6 <- fitdist(lsev6$Property.Damage, "gamma")


(G1 <- ggplot(lsev1, aes(sample = Property.Damage)) +
    stat_qq(distribution = qgamma, dparams = c(shape = gamm1$estimate[[1]], rate = gamm1$estimate[[2]]), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Log-Gamma Fit: Region 1") +
    xlab("Theoretical") +
    ylab("Empirical")
)
(G2 <- ggplot(lsev2, aes(sample = Property.Damage)) +
    stat_qq(distribution = qgamma, dparams = c(shape = gamm2$estimate[[1]], rate = gamm2$estimate[[2]]), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Log-Gamma Fit: Region 2") +
    xlab("Theoretical") +
    ylab("Empirical")
)
(G3 <- ggplot(lsev3, aes(sample = Property.Damage)) +
    stat_qq(distribution = qgamma, dparams = c(shape = gamm3$estimate[[1]], rate = gamm3$estimate[[2]]), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Log-Gamma Fit: Region 3") +
    xlab("Theoretical") +
    ylab("Empirical")
)
(G4 <- ggplot(lsev4, aes(sample = Property.Damage)) +
    stat_qq(distribution = qgamma, dparams = c(shape = gamm4$estimate[[1]], rate = gamm4$estimate[[2]]), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Log-Gamma Fit: Region 4") +
    xlab("Theoretical") +
    ylab("Empirical")
)
(G5 <- ggplot(lsev5, aes(sample = Property.Damage)) +
    stat_qq(distribution = qgamma, dparams = c(shape = gamm5$estimate[[1]], rate = gamm5$estimate[[2]]), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Log-Gamma Fit: Region 5") +
    xlab("Theoretical") +
    ylab("Empirical")
)
(G6 <- ggplot(lsev6, aes(sample = Property.Damage)) +
    stat_qq(distribution = qgamma, dparams = c(shape = gamm6$estimate[[1]], rate = gamm6$estimate[[2]]), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Log-Gamma Fit: Region 6") +
    xlab("Theoretical") +
    ylab("Empirical")
)


#MONTE CARLO: AGGREGATE LOSS PER YEAR, FOR EACH REGION
n_iterations <- 10000 #number of Monte_Carlo simulations: 

sev1 <- c()
sev2 <- c()
sev3 <- c()
sev4 <- c()
sev5 <- c()
sev6 <- c()

for (i in 1:n_iterations) {
  freq_vec <- rnbinom(1, size = nbin1$estimate[[1]], mu = nbin1$estimate[[2]])
  sev_pa <- sum(exp(rgamma(freq_vec, shape = gamm1$estimate[[1]], rate = gamm1$estimate[[2]])))
  sev1 <- append(sev1, sev_pa)
}

for (i in 1:n_iterations) {
  freq_vec <- rnbinom(1, size = nbin2$estimate[[1]], mu = nbin2$estimate[[2]])
  sev_pa <- sum(exp(rgamma(freq_vec, shape = gamm2$estimate[[1]], rate = gamm2$estimate[[2]])))
  sev2 <- append(sev2, sev_pa)
}

for (i in 1:n_iterations) {
  freq_vec <- rnbinom(1, size = nbin3$estimate[[1]], mu = nbin3$estimate[[2]])
  sev_pa <- sum(exp(rgamma(freq_vec, shape = gamm3$estimate[[1]], rate = gamm3$estimate[[2]])))
  sev3 <- append(sev3, sev_pa)
}

for (i in 1:n_iterations) {
  freq_vec <- rnbinom(1, size = nbin4$estimate[[1]], mu = nbin4$estimate[[2]])
  sev_pa <- sum(exp(rgamma(freq_vec, shape = gamm4$estimate[[1]], rate = gamm4$estimate[[2]])))
  sev4 <- append(sev4, sev_pa)
}

for (i in 1:n_iterations) {
  freq_vec <- rnbinom(1, size = nbin5$estimate[[1]], mu = nbin5$estimate[[2]])
  sev_pa <- sum(exp(rgamma(freq_vec, shape = gamm5$estimate[[1]], rate = gamm5$estimate[[2]])))
  sev5 <- append(sev5, sev_pa)
}

for (i in 1:n_iterations) {
  freq_vec <- rnbinom(1, size = nbin6$estimate[[1]], mu = nbin6$estimate[[2]])
  sev_pa <- sum(exp(rgamma(freq_vec, shape = gamm6$estimate[[1]], rate = gamm6$estimate[[2]])))
  sev6 <- append(sev6, sev_pa)
}
  
sev1_df <- data.frame(sev1)
sev2_df <- data.frame(sev2)
sev3_df <- data.frame(sev3)
sev4_df <- data.frame(sev4)
sev5_df <- data.frame(sev5)
sev6_df <- data.frame(sev6)

(sev_hist1 <- ggplot(sev1_df, aes(x = sev1)) + 
    geom_histogram(bins = 100) +
    xlim(c(0, 1e+09)) +
    ggtitle("Aggregate Loss across Instances in Region 1") +
    xlab("Aggregate Loss in 1 Year") +
    ylab("Count")
)
(sev_hist2 <- ggplot(sev2_df, aes(x = sev2)) + 
    geom_histogram(bins = 100) +
    xlim(c(0, 1e+09)) +
    ggtitle("Aggregate Loss across Instances in Region 2") +
    xlab("Aggregate Loss in 1 Year") +
    ylab("Count")
)
(sev_hist3 <- ggplot(sev3_df, aes(x = sev3)) + 
    geom_histogram(bins = 100) +
    xlim(c(0, 1e+09)) +
    ggtitle("Aggregate Loss across Instances in Region 3") +
    xlab("Aggregate Loss in 1 Year") +
    ylab("Count")
)
(sev_hist4 <- ggplot(sev4_df, aes(x = sev4)) + 
    geom_histogram(bins = 100) +
    xlim(c(0, 1e+09)) +
    ggtitle("Aggregate Loss across Instances in Region 4") +
    xlab("Aggregate Loss in 1 Year") +
    ylab("Count")
)
(sev_hist5 <- ggplot(sev5_df, aes(x = sev5)) + 
    geom_histogram(bins = 100) +
    xlim(c(0, 1e+09)) +
    ggtitle("Aggregate Loss across Instances in Region 5") +
    xlab("Aggregate Loss in 1 Year") +
    ylab("Count")
)
(sev_hist6 <- ggplot(sev6_df, aes(x = sev6)) + 
    geom_histogram(bins = 100) +
    xlim(c(0, 1e+09)) +
    ggtitle("Aggregate Loss across Instances in Region 6") +
    xlab("Aggregate Loss in 1 Years") +
    ylab("Count")
)


#Percentiles: help compute VaR
perc <- c(0.25, 0.5, 0.75, 0.8, 0.85, 0.9, 0.95, 0.98, 0.99, 0.995, 0.999)
Percentile <- c(perc, "Mean", "SD", "Max")
gross1 <- c(quantile(sev1, probs = perc), mean(sev1), sd(sev1), max(sev1))
gross2 <- c(quantile(sev2, probs = perc), mean(sev2), sd(sev2), max(sev2))
gross3 <- c(quantile(sev3, probs = perc), mean(sev3), sd(sev3), max(sev3))
gross4 <- c(quantile(sev4, probs = perc), mean(sev4), sd(sev4), max(sev4))
gross5 <- c(quantile(sev5, probs = perc), mean(sev5), sd(sev5), max(sev5))
gross6 <- c(quantile(sev6, probs = perc), mean(sev6), sd(sev6), max(sev6))

(results.table <- data.frame(Percentile, gross1, gross2, gross3, gross4, gross5, gross6))



(hazards_sorted <- hazards %>% group_by(Year) %>% summarise(sum(Property.Damage)))

#seems to underestimate aggregate loss
#may need to either exclude older years, or adjust damages for inflation


