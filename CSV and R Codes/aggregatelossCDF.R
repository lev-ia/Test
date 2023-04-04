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

#reminder: best fit for frequency per year is a negative-binomial distribution with size = 1.948557 and mu = 49.704918

#fit damages to log-gamma distribution

fitdist(hazards$Property.Damage, "lgamma") #shapelog = 14.829296, ratelog = 1.413602

(FLGP <- ggplot(hazards, aes(sample = Property.Damage)) +
    stat_qq(distribution = qlgamma, dparams = c(shapelog = 14.829296, ratelog = 1.413602), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Log-Gamma") +
    xlab("Theoretical") +
    ylab("Sample")
)

#zoom in on histogram
(FLGP2 <- ggplot(hazards, aes(sample = Property.Damage)) +
    stat_qq(distribution = qlgamma, dparams = c(shapelog = 14.829296, ratelog = 1.413602), colour = "#00BFC4", size = 1) +
    geom_abline(slope=1, intercept = 0) +
    theme_bw() +
    theme(axis.title = element_text(size = 13.5)) +
    ggtitle("Log-Gamma") +
    xlab("Theoretical") +
    ylab("Sample") +
    xlim(0, 1e+09) +
    ylim(0, 1e+09)
) 

#decent fit 

#gF_aggloss: aggregate loss cumulative distribution

model.freq <- expression(data = rnbinom(size = 1.948557, mu = 49.704918))
model.sev <- expression(data = rlgamma(shapelog = 14.829296, ratelog = 1.413602))

F_aggloss <- aggregateDist("simulation", nb.simul = 100000, model.freq, model.sev)
mean(F_aggloss)
mean(F_aggloss)/sum(hazards$Property.Damage) 

#calculate VaR: in 1 year
VaR(F_aggloss)

CTE(F_aggloss) #TVaR, can choose confidence level using argument conf.level = c(0.9, 0.95, 0.99)



