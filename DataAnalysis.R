### Analysis File: Domestic Surveillance and Political Imprisonment - Evidence from the former GDR
### Author: Christoph Valentin Steinert

# Load dataset 
rm(list = ls())
## set working directory
gdr_data <- read.csv("gdr_prepared.csv")

# Load packages
library(texreg)
library(dplyr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(Hmisc)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggeffects)
library(cem)
library(optimx)
library(MASS)
library(lme4)
library(lfe)
library(caret)
library(extrafont)
library(miceadds)
library(lme4)
library(lmerTest)
library(ivpack)
library(Amelia)
library(performance)
library(lmtest)
library(AER)
library(stargazer)

# Plot average development of political imprisonment over time
pimpyear <- aggregate(parrests ~ year, gdr_data, sum)
ggplot(data = pimpyear, aes(x= year, y = parrests)) + geom_line() + labs(y = "# of Political Imprisonments") + 
  ggtitle("Political imprisonments over time") +   
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Year")

# Plot county-level developments of political imprisonment over time
sho <- filter(gdr_data, year != 1983)
ggplot(data = sho, aes(x= year, y = parrests, color = county)) + geom_line(aes(color = county)) + labs(y = "# of Political Imprisonments") + 
  ggtitle("Political imprisonments over time") +   
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Year") ## Massive spike in Leipzig

# Plot average development of OPKs over time
opks_time <- filter(gdr_data, !is.na(opks_clean))
see_miss <- count(opks_time, county)
opks_time_small <- filter(see_miss, n == 6)
opks_full <- left_join(opks_time_small, opks_time, by = "county")
opksyear <- aggregate(opks_clean ~ year, opks_full, sum)
ggplot(data = opksyear, aes(x= year, y = opks_clean)) + geom_line() + labs(y = "# of OPKs") + 
  ggtitle("OPKs over time") +   
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Year")

# Plot county-level developments of OPKs over time
ggplot(data = opks_full, aes(x= year, y = opks_clean, color = county)) + geom_line(aes(color = county)) + labs(y = "# of OPKs") + 
  ggtitle("OPKs over time") +   
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Year") 

# Plot average development of OVs over time
ovs_time <- filter(gdr_data, !is.na(ovs_clean))
see_miss <- count(ovs_time, county)
ovs_time_small <- filter(see_miss, n == 6)
ovs_full <- left_join(ovs_time_small, ovs_time, by = "county")
ovsyear <- aggregate(ovs_clean ~ year, ovs_full, sum)
ggplot(data = ovsyear, aes(x= year, y = ovs_clean)) + geom_line() + labs(y = "# of OVs") + 
  ggtitle("OVs over time") +   
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Year")

# Plot county-level developments of OVs over time
ggplot(data = ovs_full, aes(x= year, y = ovs_clean, color = county)) + geom_line(aes(color = county)) + labs(y = "# of OVs") + 
  ggtitle("OVs over time") +   
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Year") 

# Plot density of key outcome variable, i.e. political imprisonment
#### THE FOLLOWING COMMAND REPRODUCES FIGURE 1 IN THE ONLINE APPENDIX ####
## pdf(file = "density_polimprisonment.pdf")
plot(density(gdr_data$parrests, na.rm = T), main="", 
     xlab="# of political imprisonment (per county-years)", xlim = c(0, 200),  bty="n")
for(i in 1:1){
  abline(v=gdr_data$parrests[order(gdr_data$parrests, decreasing=TRUE)][i])
  text(x=gdr_data$parrests[order(gdr_data$parrests, decreasing=TRUE)][i], y=0.05,  
       labels=gsub(" _", "", gdr_data$county_year[order(gdr_data$parrests, decreasing=TRUE)][i]), pos=4, cex=0.8)
}
## dev.off()

# Plot measures against each other to see distribution of ratios
#### THE FOLLOWING COMMAND REPRODUCES FIGURE 2 IN THE ONLINE APPENDIX ####
## pdf(file = "scatterplot_imsopks.pdf")
ggplot(data = gdr_data) + geom_point(mapping = aes(x = opks_per_ht, y = ims_per_ht)) + labs(y = "IMs per 100.000 citizens") + 
  labs(x = "OPKs per 100.000 citizens") + xlim(0, 175) + ylim(0, 750) + theme_minimal() 
## dev.off()

# Create measures that captures share of opks/total_ims 
gdr_data$imsopks <- (gdr_data$sum_im_clean / gdr_data$opks_clean) 
gdr_data$imsopks_impute <- (gdr_data$sum_im_impute / gdr_data$opks_impute) 

# Create measures that captures share of opks/ims(only) 
gdr_data$imsopks_only <- (gdr_data$ims_clean / gdr_data$opks_clean) 
gdr_data$imsopks_only_impute <- (gdr_data$ims_impute / gdr_data$opks_impute)

# Incidence of silent repression
gdr_data$zersetz_incidence <- (gdr_data$zersetz_and_prevent) / (gdr_data$pop_county) * 1000000
gdr_data$zersetz_incidence_impute <- (gdr_data$zersetz_and_prevent_impute) / (gdr_data$pop_county) * 1000000

# Scatterplots
#### THE FOLLOWING COMMAND REPRODUCES FIGURE 3a IN THE ARTICLE ####
## pdf(file = "bivariate.pdf")
ggplot(gdr_data, aes(lag(total_ims_per_ht), pris_per_ht)) + labs(y = "Pol. imprisonments per 100.000 citizens") + 
  labs(x = "Informants per 100.000 citizen (lagged)")  +  geom_smooth(method = 'lm', fill = "grey") +  theme_minimal()
## dev.off()
#### THE FOLLOWING COMMAND REPRODUCES FIGURE 3b IN THE ARTICLE ####
## pdf(file = "bivariate_decomposition.pdf")
ggplot(gdr_data, aes(lag(total_ims_per_ht), zersetz_incidence)) + labs(y = "Decomposition measures per 1 million citizens") + 
  labs(x = "Informants per 100.000 citizen (lagged)")  +  geom_smooth(method = 'lm', fill = "grey") +  theme_minimal() 
## dev.off()

# Since Leipzig (1988) is a clear outlier, create subdata without this observation
without_leipzig_88 <- filter(gdr_data, county_year != "Leipzig _ 1988")

# Scatterplots without outlier "Leipzig 1988"
ggplot(without_leipzig_88, aes(lag(total_ims_per_ht), pris_per_ht)) +  
  geom_smooth(method = 'lm')
ggplot(without_leipzig_88, aes(lag(imsopks), pris_per_ht)) + 
  geom_smooth(method = 'lm')
ggplot(without_leipzig_88, aes(lag(imsopks_impute), pris_per_ht)) + 
  geom_smooth(method = 'lm')

# Test for multicollinearity 
keyvars <- dplyr::select(gdr_data, pris_per_ht, imsopks_impute, city_county, percentage_construction_workers,
                         flats_per_ht, hospital_beds_per_ht, theater_guests_per_ht, poland, percentage_female,
                         percentage_employed_persons, industrial_productivity, districtcapital, cities, percentage_farmers,
                         dist_Berlin, protests53)
keyvars$county <- NULL

# Correlation Matrix
res <- cor(keyvars, use = "complete.obs")
round(res, 2)

#  Variance Inflation Factors
model_full <- lm(pris_per_ht ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) +
                   lag(flats_per_ht) + lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) +
                   poland + factor(year) + lag(percentage_female) + lag(percentage_employed_persons) +
                   lag(industrial_productivity) + lag(percentage_farmers) + protests53 +
                   districtcapital + cities + dist_Berlin, data = gdr_data)
car::vif(model_full)

# exclude percentage farmers, district capital, cities, flats_per_ht, percentage_employed, percentage_female 
model_reduced_1 <- lm(pris_per_ht ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) +
                        lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) +
                        poland + factor(year)  + protests53 +
                        lag(industrial_productivity) + dist_Berlin, data= gdr_data)
car::vif(model_reduced_1)

# Descriptive Table
descriptive_vars <- dplyr::select(gdr_data, pris_per_ht, total_ims_per_ht, sum_im_impute_per_ht, imsopks, imsopks_impute, zersetz_incidence, zersetz_incidence_impute,
                                  city_county, percentage_construction_workers, flats_per_ht, hospital_beds_per_ht, theater_guests_per_ht, percentage_female,
                                  percentage_employed_persons, industrial_productivity, districtcapital, cities, percentage_farmers, protests53, 
                                  dist_Berlin, WGTV, WGTV_continuous)
descriptive_vars$county <- NULL
descriptive_vars <- as.data.frame(descriptive_vars)
#### THE FOLLOWING COMMAND REPRODUCES TABLE 1 IN THE ONLINE APPENDIX ####
stargazer(descriptive_vars, iqr = TRUE)  

# Drop data-objects that are not used anymore
rm(opks_full, opks_time, opks_time_small, opksyear, ovs_full, ovs_time, ovs_time_small, ovsyear, pimpyear, see_miss,
   sho, res, i)

# Create year-dummies
gdr_data$year_83 <- 0
gdr_data$year_83[gdr_data$year == 1983] <- 1
gdr_data$year_84 <- 0
gdr_data$year_84[gdr_data$year == 1984] <- 1
gdr_data$year_85 <- 0
gdr_data$year_85[gdr_data$year == 1985] <- 1
gdr_data$year_86 <- 0
gdr_data$year_86[gdr_data$year == 1986] <- 1
gdr_data$year_87 <- 0
gdr_data$year_87[gdr_data$year == 1987] <- 1
gdr_data$year_88 <- 0
gdr_data$year_88[gdr_data$year == 1988] <- 1


### Multivariate analyses 

#### 1.) Information logic ####

#### Spying per monitored individual ####

# imsopks - parrests not standardized
m1 <- lm(parrests ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m1) 

# imsopks_imputed - parrests not standardized
m2 <- lm(parrests ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m2) 

# imsopks with year fixed effects - parrests standardized
m3 <- lm(pris_per_ht ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m3)
included_vars <- m3$model

# imsopks_imputed with year fixed effects - parrests standardized
m4 <- lm(pris_per_ht ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m4) 

# imsopks with random effects for counties - parrests standardized
m5 <- lmer(pris_per_ht ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
              lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + 
              lag(industrial_productivity) + protests53 + lag(ovs_impute_per_ht) + factor(year) + 
              (1| county), data = gdr_data, REML = TRUE)
screenreg(m5)

# imsopks_imputed with random effects for counties - parrests standardized
m6 <- lmer(pris_per_ht ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
              lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + 
              lag(industrial_productivity) + protests53 + lag(ovs_impute_per_ht) + factor(year) + 
              (1| county), data = gdr_data, REML = TRUE)
screenreg(m6)

# imsopks_imputed with test for electronic surveillance (telephone-tapping) - parrests standardized
m7 <- lm(pris_per_ht ~ lag(imsopks_impute) + realised_telephone + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m7)

# imsopks_imputed with test for electronic surveillance (telephone-tapping_imputed) - parrests standardized
m8 <- lm(pris_per_ht ~ lag(imsopks_impute) + realised_telephone_imputed + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m8)

# imsopks_imputed with test for electronic surveillance (realised_acustic_imputed) - parrests standardized
m9 <- lm(pris_per_ht ~ lag(imsopks_impute) + realised_acustic_imputed + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m9)

# imsopks_imputed with test for electronic surveillance (abt26_imputed) - parrests standardized
m10 <- lm(pris_per_ht ~ lag(imsopks_impute) + abt26_imputed + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m10)

# imsopks_imputed with test for electronic surveillance (room_control_imputed) - parrests standardized
m11 <- lm(pris_per_ht ~ lag(imsopks_impute) + realised_roomcontrol_imputed + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(ovs_impute_per_ht), data = gdr_data)
screenreg(m11)

# imsopks test for Objektdienststellen - parrests standardized
m12 <- lm(pris_per_ht ~ lag(imsopks) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m12)

# imsopks_imputed test for Objektdienststellen - parrests standardized
m13 <- lm(pris_per_ht ~ lag(imsopks_impute) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m13)

# imsopks with alternative IM-measure (IMS only)
m14 <- lm(pris_per_ht ~ lag(imsopks_only) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m14)

# imsopks with alternative IM-measure (IMS only imputed)
m15 <- lm(pris_per_ht ~ lag(imsopks_only_impute) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m15)

# imsopks with control for yearly pop_change to capture out-migration
m16 <- lm(pris_per_ht ~ lag(imsopks) + lag(pop_change_yearly) + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m16)

# imsopks instrumented with WGTV signal strength
summary(first1 <- lm(imsopks ~ WGTV_continuous + city_county + lag(percentage_construction_workers) + 
                       lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
                       protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data))
linearHypothesis(first1, "WGTV_continuous = 0", type = c("F") )
iv1 <- ivreg(pris_per_ht ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year) | WGTV_continuous + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
coeftest(iv1, vcov = vcovHC, type = "HC1") 
summary(iv1, vcov = sandwich, diagnostics = TRUE)

# imsopks_imputed instrumented with WGTV signal strength
first1 <- lm(imsopks_impute ~ WGTV_continuous + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
first_pred <- first1$fitted.values
reduced = lm(pris_per_ht ~ WGTV_continuous, data = gdr_data)
iv2 = ivreg(pris_per_ht ~ lag(imsopks_impute) | WGTV_continuous, data = gdr_data)
iv2 <- ivreg(pris_per_ht ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year) | WGTV_continuous + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
coeftest(iv2, vcov = vcovHC, type = "HC1") 
summary(iv2, vcov = sandwich, diagnostics = TRUE)

# imsopks instrumented with distance to Berlin 
first1 <- lm(imsopks ~ dist_Berlin + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
first_pred <- first1$fitted.values
reduced = lm(pris_per_ht ~ dist_Berlin, data = gdr_data)
iv1_Berlin <- ivreg(pris_per_ht ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
                      lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
                      protests53 + lag(ovs_impute_per_ht) + factor(year) | dist_Berlin + city_county + lag(percentage_construction_workers) + 
                      lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) +  lag(industrial_productivity) + 
                      protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
coeftest(iv1_Berlin, vcov = vcovHC, type = "HC1") 
summary(iv1_Berlin, vcov = sandwich, diagnostics = TRUE)

# imsopks_imputed instrumented with distance to Berlin 
first1 <- lm(imsopks_impute ~ dist_Berlin + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
first_pred <- first1$fitted.values
reduced = lm(pris_per_ht ~ dist_Berlin, data = gdr_data)
iv2_Berlin <- ivreg(pris_per_ht ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
                      lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(industrial_productivity) + 
                      protests53 + lag(ovs_impute_per_ht) + factor(year) | dist_Berlin + city_county + lag(percentage_construction_workers) + 
                      lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
                      protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
coeftest(iv2_Berlin, vcov = vcovHC, type = "HC1") 
summary(iv2_Berlin, vcov = sandwich, diagnostics = TRUE)

# Create delta-variable for pris_per_ht
gdr_data$delta_pris_per_ht <- c(0, diff(gdr_data$pris_per_ht))
gdr_data$delta_pris_per_ht[gdr_data$year == 1984] <- 0
m_delta1 <- lm(delta_pris_per_ht ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m_delta1)
# Create delta-variable for parrests
gdr_data$delta_parrests <- c(0, diff(gdr_data$parrests))
gdr_data$delta_parrests[gdr_data$year == 1984] <- 0
m_delta2 <- lm(delta_parrests ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
                 lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
                 protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m_delta2)


#### County level of spy infiltration ####

# IMs per capita - parrests not standardized
m_1a <- lm(parrests ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m_1a) 

# IMs per capita imputed - parrests not standardized
m_2a <- lm(parrests ~ lag(sum_im_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m_2a) 

# IMs per capita with year fixed effects - parrests standardized
m_3a <- lm(pris_per_ht ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m_3a)
included_vars_a <- m_3a$model

# IMs per capita imputed with year fixed effects - parrests standardized
m_4a <- lm(pris_per_ht ~ lag(sum_im_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m_4a) 

# IMs per capita with random effects for counties - parrests standardized
m_5a <- lmer(pris_per_ht ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + 
             lag(industrial_productivity) + protests53 + lag(ovs_impute_per_ht) + factor(year) + 
             (1| county), data = gdr_data, REML = TRUE)
screenreg(m_5a)

# IMs per capita imputed with random effects for counties - parrests standardized
m_6a <- lmer(pris_per_ht ~ lag(sum_im_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + 
             lag(industrial_productivity) + protests53 + lag(ovs_impute_per_ht) + factor(year) + 
             (1| county), data = gdr_data, REML = TRUE)
screenreg(m_6a)

# IMs per capita imputed with test for electronic surveillance (telephone-tapping) - parrests standardized
m_7a <- lm(pris_per_ht ~ lag(sum_im_impute_per_ht) + realised_telephone + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m_7a)

# IMs per capita imputed with test for electronic surveillance (telephone-tapping_imputed) - parrests standardized
m_8a <- lm(pris_per_ht ~ lag(sum_im_impute_per_ht) + realised_telephone_imputed + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m_8a)

# IMs per capita imputed with test for electronic surveillance (realised_acustic_imputed) - parrests standardized
m_9a <- lm(pris_per_ht ~ lag(sum_im_impute_per_ht) + realised_acustic_imputed + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m_9a)

# IMs per capita imputed with test for electronic surveillance (abt26_imputed) - parrests standardized
m_10a <- lm(pris_per_ht ~ lag(sum_im_impute_per_ht) + abt26_imputed + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m_10a)

# IMs per capita imputed with test for electronic surveillance (room_control_imputed) - parrests standardized
m_11a <- lm(pris_per_ht ~ lag(sum_im_impute_per_ht) + realised_roomcontrol_imputed + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(ovs_impute_per_ht), data = gdr_data)
screenreg(m_11a)

# IMs per capita test for Objektdienststellen - parrests standardized
m_12a <- lm(pris_per_ht ~ lag(total_ims_per_ht) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m_12a)

# IMs per capita imputed test for Objektdienststellen - parrests standardized
m_13a <- lm(pris_per_ht ~ lag(sum_im_impute_per_ht) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m_13a)

# IMs per capita with alternative IM-measure (IMS only)
m_14a <- lm(pris_per_ht ~ lag(ims_per_ht) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m_14a)

# IMs per capita with alternative IM-measure (IMS only imputed)
m_15a <- lm(pris_per_ht ~ lag(ims_only_impute_per_ht) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m_15a)

# IMs per capita with control for yearly pop_change to capture out-migration
m_16a <- lm(pris_per_ht ~ lag(total_ims_per_ht) + lag(pop_change_yearly) + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m_16a)

# IMs per capita instrumented with WGTV signal strength
summary(first1 <- lm(total_ims_per_ht ~ WGTV_continuous + lag(opks_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
                       lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
                       protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data))
linearHypothesis(first1, "WGTV_continuous = 0", type = c("F") )
iv2 <- ivreg(pris_per_ht ~ lag(total_ims_per_ht) + lag(opks_impute_per_ht) +  city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year) | WGTV_continuous + lag(opks_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
coeftest(iv2, vcov = vcovHC, type = "HC1") 
summary(iv2, vcov = sandwich, diagnostics = TRUE)

# IMs per capita imputed instrumented with WGTV signal strength
first1 <- lm(sum_im_impute_per_ht ~ WGTV_continuous + lag(opks_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
first_pred <- first1$fitted.values
reduced = lm(pris_per_ht ~ WGTV_continuous, data = gdr_data)
iv2 = ivreg(pris_per_ht ~ lag(sum_im_impute_per_ht) | WGTV_continuous, data = gdr_data)
iv2 <- ivreg(pris_per_ht ~ lag(sum_im_impute_per_ht) + lag(opks_impute_per_ht) + city_county  + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year) | WGTV_continuous + lag(opks_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
coeftest(iv2, vcov = vcovHC, type = "HC1") 
summary(iv2, vcov = sandwich, diagnostics = TRUE)

# IMs per capita instrumented with distance to Berlin 
first1 <- lm(total_ims_per_ht ~ dist_Berlin + lag(opks_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
first_pred <- first1$fitted.values
reduced = lm(pris_per_ht ~ dist_Berlin, data = gdr_data)
iv1_Berlin <- ivreg(pris_per_ht ~ lag(total_ims_per_ht) + lag(opks_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
                      lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
                      protests53 + lag(ovs_impute_per_ht) + factor(year) | dist_Berlin + lag(opks_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
                      lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) +  lag(industrial_productivity) + 
                      protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
coeftest(iv1_Berlin, vcov = vcovHC, type = "HC1") 
summary(iv1_Berlin, vcov = sandwich, diagnostics = TRUE)

# IMs per capita imputed instrumented with distance to Berlin 
first1 <- lm(sum_im_impute_per_ht ~ dist_Berlin + lag(opks_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
first_pred <- first1$fitted.values
reduced = lm(pris_per_ht ~ dist_Berlin, data = gdr_data)
iv2_Berlin <- ivreg(pris_per_ht ~ lag(sum_im_impute_per_ht) + lag(opks_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
                      lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
                      protests53 + lag(ovs_impute_per_ht) + factor(year) | dist_Berlin + lag(opks_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
                      lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
                      protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
coeftest(iv2_Berlin, vcov = vcovHC, type = "HC1") 
summary(iv2_Berlin, vcov = sandwich, diagnostics = TRUE)

# Use delta-variable for pris_per_ht
m_delta3 <- lm(delta_pris_per_ht ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
                 lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
                 protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m_delta3)
# Use delta-variable for parrests
m_delta4 <- lm(delta_parrests ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
                 lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
                 protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(m_delta4)


#### 2.) Strategy substitution logic ####

#### Spying per monitored individual ####

# imsopks - decomposition not standardized
s1 <- lm(zersetz_and_prevent ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + factor(year), data = gdr_data)
screenreg(s1) 

# imsopks_imputed - decomposition not standardized
s2 <- lm(zersetz_and_prevent ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + factor(year), data = gdr_data)
screenreg(s2) 

# imsopks with year fixed effects - decomposition standardized
s3 <- lm(zersetz_incidence ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht)  + factor(year), data = gdr_data)
screenreg(s3)

# imsopks_imputed with year fixed effects - decomposition standardized
s4 <- lm(zersetz_incidence ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s4) 

# imsopks with random effects for counties - decomposition standardized
s5 <- lmer(zersetz_incidence ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + 
             lag(industrial_productivity) + protests53 + lag(ovs_impute_per_ht) + factor(year) + 
             (1| county), data = gdr_data, REML = TRUE)
screenreg(s5)

# imsopks_imputed with random effects for counties - decomposition standardized
s6 <- lmer(zersetz_incidence ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) +  
             lag(industrial_productivity) + protests53 + lag(ovs_impute_per_ht) + factor(year) + 
             (1| county), data = gdr_data, REML = TRUE)
screenreg(s6)

# imsopks_imputed with test for electronic surveillance (telephone-tapping) - decomposition standardized
s7 <- lm(zersetz_incidence ~ lag(imsopks_impute) + realised_telephone + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s7)

# imsopks_imputed with test for electronic surveillance (telephone-tapping_imputed) - decomposition standardized
s8 <- lm(zersetz_incidence ~ lag(imsopks_impute) + realised_telephone_imputed + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s8)

# imsopks_imputed with test for electronic surveillance (realised_acustic_imputed) - decomposition standardized
s9 <- lm(zersetz_incidence ~ lag(imsopks_impute) + realised_acustic_imputed + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s9)

# imsopks_imputed with test for electronic surveillance (abt26_imputed) - decomposition standardized
s10 <- lm(zersetz_incidence ~ lag(imsopks_impute) + abt26_imputed + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s10)

# imsopks_imputed with test for electronic surveillance (room_control_imputed) - decomposition standardized
s11 <- lm(zersetz_incidence ~ lag(imsopks_impute) + realised_roomcontrol_imputed + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(ovs_impute_per_ht), data = gdr_data)
screenreg(s11)

# imsopks test for Objektdienststellen - decomposition standardized
s12 <- lm(zersetz_incidence ~ lag(imsopks) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s12)

# imsopks_imputed test for Objektdienststellen - decomposition standardized
s13 <- lm(zersetz_incidence ~ lag(imsopks_impute) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s13)

# imsopks with alternative IM-measure (IMS only) - decomposition standardized
s14 <- lm(zersetz_incidence ~ lag(imsopks_only) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s14)

# imsopks with alternative IM-measure (IMS only imputed) - decomposition standardized
s15 <- lm(zersetz_incidence ~ lag(imsopks_only_impute) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s15)

# imsopks with control for yearly pop_change to capture out-migration - decomposition standardized
s16 <- lm(zersetz_incidence ~ lag(imsopks) + lag(pop_change_yearly) + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s16)


#### County level of spy infiltration ####

# IMs per capita - decomposition not standardized
s_1a <- lm(zersetz_and_prevent ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + factor(year), data = gdr_data)
screenreg(s_1a) 

# IMs per capita imputed - decomposition not standardized
s_2a <- lm(zersetz_and_prevent ~ lag(sum_im_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + factor(year), data = gdr_data)
screenreg(s_2a) 

# IMs per capita with year fixed effects - decomposition standardized
s_3a <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s_3a)
included_vars_a <- s_3a$model

# IMs per capita imputed with year fixed effects - decomposition standardized
s_4a <- lm(zersetz_incidence ~ lag(sum_im_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s_4a) 

# IMs per capita with random effects for counties - decomposition standardized
s_5a <- lmer(zersetz_incidence ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) +  
               lag(industrial_productivity) + protests53 + lag(ovs_impute_per_ht) + factor(year) + 
               (1| county), data = gdr_data, REML = TRUE)
screenreg(s_5a)

# IMs per capita imputed with random effects for counties - decomposition standardized
s_6a <- lmer(zersetz_incidence ~ lag(sum_im_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) +  
               lag(industrial_productivity) + protests53 + lag(ovs_impute_per_ht) + factor(year) + 
               (1| county), data = gdr_data, REML = TRUE)
screenreg(s_6a)

# IMs per capita imputed with test for electronic surveillance (telephone-tapping_imputed) - decomposition standardized
s_7a <- lm(zersetz_incidence ~ lag(sum_im_impute_per_ht) + realised_telephone_imputed + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s_7a)

# IMs per capita imputed with test for electronic surveillance (realised_acustic_imputed) - decomposition standardized
s_8a <- lm(zersetz_incidence ~ lag(sum_im_impute_per_ht) + realised_acustic_imputed + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s_8a)

# IMs per capita imputed with test for electronic surveillance (abt26_imputed) - decomposition standardized
s_9a <- lm(zersetz_incidence ~ lag(sum_im_impute_per_ht) + abt26_imputed + city_county + lag(percentage_construction_workers) + 
              lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
              protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s_9a)

# IMs per capita imputed with test for electronic surveillance (room_control_imputed) - decomposition standardized
s_10a <- lm(zersetz_incidence ~ lag(sum_im_impute_per_ht) + realised_roomcontrol_imputed + city_county + lag(percentage_construction_workers) + 
              lag(hospital_beds_per_ht) + lag(theater_guests_per_ht), data = gdr_data)
screenreg(s_10a)

# IMs per capita test for Objektdienststellen - decomposition standardized
s_11a <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
              lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
              protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s_11a)

# IMs per capita imputed test for Objektdienststellen - decomposition standardized
s_12a <- lm(zersetz_incidence ~ lag(sum_im_impute_per_ht) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
              lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
              protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s_12a)

# IMs per capita with alternative IM-measure (IMS only) - decomposition standardized
s_13a <- lm(zersetz_incidence ~ lag(ims_per_ht) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
              lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
              protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s_13a)

# IMs per capita with alternative IM-measure (IMS only imputed) - decomposition standardized
s_14a <- lm(zersetz_incidence ~ lag(ims_only_impute_per_ht) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
              lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
              protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s_14a)

# IMs per capita with control for yearly pop_change to capture out-migration - decomposition standardized
s_15a <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + lag(pop_change_yearly) + city_county + lag(percentage_construction_workers) + 
              lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
              protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
screenreg(s_15a)


### Intra-class correlation coefficient (= proportion of variance explained by grouping structure)
m_small_1 <- lmer(parrests ~ lag(imsopks) + (1|county), data = gdr_data, REML = T)
icc(m_small_1)
m_small_2 <- lmer(parrests ~ (1|county), data = gdr_data, REML = T)
icc(m_small_2)

#### How much variation is explained by county fixed effects ####
test1 <- lm(parrests ~ factor(county), data = gdr_data)
screenreg(test1)
test2 <- lm(opks_clean ~ factor(county), data = gdr_data)
screenreg(test2)
test3 <- lm(ovs_clean ~ factor(county), data = gdr_data)
screenreg(test3)
test4 <- lm(sum_im_clean ~ factor(county), data = gdr_data)
screenreg(test4)
test5 <- lm(imsopks ~ factor(county), data = gdr_data)
screenreg(test5)

#### Test regression assumptions of main models ####

# Linearity of the data
m_3a <- lm(pris_per_ht ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
plot(m_3a, 1)
m3 <- lm(pris_per_ht ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
plot(m3, 1)
s3 <- lm(zersetz_incidence ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
plot(s3, 1)
s_3a <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
plot(s_3a, 1)

# QQ plot of residuals
m_3a <- lm(pris_per_ht ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
plot(m_3a, 2)
m3 <- lm(pris_per_ht ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
plot(m3, 2)
s3 <- lm(zersetz_incidence ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
plot(s3, 2)
s_3a <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
plot(s_3a, 2)

# Cook's distance
plot(m_3a, 4)
### Re-run model without leverage point
m_3a <- lm(pris_per_ht ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data[c(-480), ])
screenreg(m_3a)
# Cook's distance
plot(m3, 4)
### Re-run model without leverage point
m3 <- lm(pris_per_ht ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data[c(-733, -1182), ])
screenreg(m3)
# Cook's distance
plot(s3, 4)
### Re-run model without leverage point
s3 <- lm(zersetz_incidence ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data[c(-421), ])
screenreg(s3)
# Cook's distance
plot(s_3a, 4)
### Re-run model without leverage point
s_3a <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data[c(-263), ])
screenreg(s_3a)

# Testing the Homoscedasticity Assumption
plot(m_3a, 3)
plot(m3, 3)
plot(s3, 3)
plot(s_3a, 3)
# Breusch-Pagan test
bptest(m3)
bptest(m_3a)
bptest(s3)
bptest(s_3a)

# Re-run main models with heteroskedasticity robust standard errors
m_3a <- lm(pris_per_ht ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
coeftest(m_3a, vcov = vcovHC(m_3a, type = "HC0"))
m3 <- lm(pris_per_ht ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
coeftest(m3, vcov = vcovHC(m3, type = "HC0"))
s3 <- lm(zersetz_incidence ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
coeftest(s3, vcov = vcovHC(s3, type = "HC0"))
s_3a <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
coeftest(s_3a, vcov = vcovHC(s_3a, type = "HC0"))

# Re-run models with clustered standard errors
m_3a <- lm(pris_per_ht ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
coeftest(m_3a, vcov = vcovCL, cluster = ~county)
m3 <- lm(pris_per_ht ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
coeftest(m_3a, vcov = vcovCL, cluster = ~county)
s3 <- lm(zersetz_incidence ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
coeftest(s3, vcov = vcovCL, cluster = ~county)
s_3a <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
coeftest(s_3a, vcov = vcovCL, cluster = ~county)


#### Presentation of results ####

# Main OLS regression models information logic
#### THE FOLLOWING COMMANDS REPRODUCE TABLE 2 IN THE ONLINE APPENDIX ####
m_3a <- lm(pris_per_ht ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m_4a <- lm(pris_per_ht ~ lag(sum_im_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m3 <- lm(pris_per_ht ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m4 <- lm(pris_per_ht ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
stargazer(list(m_3a, m_4a, m3, m4), title = "Regression results", 
          omit="factor\\(year", add.lines=list(c("Year fixed effects", rep("YES", 4))))


# Main OLS regression models strategy substitution logic
#### THE FOLLOWING COMMANDS REPRODUCES TABLE 11 IN THE ONLINE APPENDIX ####
s3 <- lm(zersetz_incidence ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
s4 <- lm(zersetz_incidence ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
s_3a <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
s_4a <- lm(zersetz_incidence ~ lag(sum_im_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
stargazer(list(s_3a, s_4a, s3, s4), title = "Regression results", 
          omit="factor\\(year", add.lines=list(c("Year fixed effects", rep("YES", 4))))


# Alternative model specifications with different controls (information logic):
#### THE FOLLOWING COMMANDS REPRODUCE TABLE 8 IN THE ONLINE APPENDIX ####
m1 <- lm(pris_per_ht ~ lag(total_ims_per_ht) + cities +
           lag(percentage_female) + lag(percentage_employed_persons) + 
           lag(percentage_farmers) + 
           lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m2 <- lm(pris_per_ht ~ lag(sum_im_impute_per_ht) + cities +
           lag(percentage_female) + lag(percentage_employed_persons) + 
           lag(percentage_farmers) + 
           lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m3 <- lm(pris_per_ht ~ lag(imsopks) + cities +
           lag(percentage_female) + lag(percentage_employed_persons) + 
           lag(percentage_farmers) + 
           lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m4 <- lm(pris_per_ht ~ lag(imsopks_impute) + cities +
           lag(percentage_female) + lag(percentage_employed_persons) + 
           lag(percentage_farmers) + 
           lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
stargazer(list(m1, m2, m3, m4), title = "Regression results", 
          omit="factor\\(year", add.lines=list(c("Year fixed effects", rep("YES", 4))))


# Alternative model specifications with different controls (strategy substitution logic):
#### THE FOLLOWING COMMANDS REPRODUCE TABLE 17 IN THE ONLINE APPENDIX ####
m1 <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + cities +
           lag(percentage_female) + lag(percentage_employed_persons) + 
           lag(percentage_farmers) + 
           lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m2 <- lm(zersetz_incidence ~ lag(sum_im_impute_per_ht) + cities +
           lag(percentage_female) + lag(percentage_employed_persons) + 
           lag(percentage_farmers) + 
           lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m3 <- lm(zersetz_incidence ~ lag(imsopks) + cities +
           lag(percentage_female) + lag(percentage_employed_persons) + 
           lag(percentage_farmers) + 
           lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m4 <- lm(zersetz_incidence ~ lag(imsopks_impute) + cities +
           lag(percentage_female) + lag(percentage_employed_persons) + 
           lag(percentage_farmers) + 
           lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
stargazer(list(m1, m2, m3, m4), title = "Regression results", 
          omit="factor\\(year", add.lines=list(c("Year fixed effects", rep("YES", 4))))


# Random effects models (information logic)
#### THE FOLLOWING COMMANDS REPRODUCE TABLE 7 IN THE ONLINE APPENDIX ####
m_5a <- lmer(pris_per_ht ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + 
               lag(industrial_productivity) + protests53 + lag(ovs_impute_per_ht) + factor(year) + 
               (1| county), data = gdr_data, REML = TRUE)
m_6a <- lmer(pris_per_ht ~ lag(sum_im_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + 
               lag(industrial_productivity) + protests53 + lag(ovs_impute_per_ht) + factor(year) + 
               (1| county), data = gdr_data, REML = TRUE)
m5 <- lmer(pris_per_ht ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + 
             lag(industrial_productivity) + protests53 + lag(ovs_impute_per_ht) + factor(year) + 
             (1| county), data = gdr_data, REML = TRUE)
m6 <- lmer(pris_per_ht ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + 
             lag(industrial_productivity) + protests53 + lag(ovs_impute_per_ht) + factor(year) + 
             (1| county), data = gdr_data, REML = TRUE)
class(m_5a) <- "lmerMod"
class(m_6a) <- "lmerMod"
class(m5) <- "lmerMod"
class(m6) <- "lmerMod"
stargazer(list(m_5a, m_6a, m5, m6), title = "Random effects models", omit="factor\\(year", add.lines=list(c("Year fixed effects", rep("YES", 4))))

# Random effects models (strategy substitution logic)
#### THE FOLLOWING COMMANDS REPRODUCE TABLE 16 IN THE ONLINE APPENDIX ####
m_5a <- lmer(zersetz_incidence ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + 
               lag(industrial_productivity) + protests53 + lag(ovs_impute_per_ht) + factor(year) + 
               (1| county), data = gdr_data, REML = TRUE)
m_6a <- lmer(zersetz_incidence ~ lag(sum_im_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + 
               lag(industrial_productivity) + protests53 + lag(ovs_impute_per_ht) + factor(year) + 
               (1| county), data = gdr_data, REML = TRUE)
m5 <- lmer(zersetz_incidence ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + 
             lag(industrial_productivity) + protests53 + lag(ovs_impute_per_ht) + factor(year) + 
             (1| county), data = gdr_data, REML = TRUE)
m6 <- lmer(zersetz_incidence ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + 
             lag(industrial_productivity) + protests53 + lag(ovs_impute_per_ht) + factor(year) + 
             (1| county), data = gdr_data, REML = TRUE)
class(m_5a) <- "lmerMod"
class(m_6a) <- "lmerMod"
class(m5) <- "lmerMod"
class(m6) <- "lmerMod"
stargazer(list(m_5a, m_6a, m5, m6), title = "Random effects models", omit="factor\\(year", add.lines=list(c("Year fixed effects", rep("YES", 4))))


# Electronic surveillance robustness test in OLS regression models (information logic)
#### THE FOLLOWING COMMANDS REPRODUCE TABLE 3 IN THE ONLINE APPENDIX ####
m14 <- lm(pris_per_ht ~ lag(total_ims_per_ht) + realised_telephone_imputed + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m15 <- lm(pris_per_ht ~ lag(total_ims_per_ht) + realised_acustic_imputed + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m16 <- lm(pris_per_ht ~ lag(total_ims_per_ht) + abt26_imputed + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
stargazer(list(m14, m15, m16), title = "OLS regression with controls for electronic surveillance tools",
          omit="factor\\(year", add.lines=list(c("Year fixed effects", rep("YES", 3))))


# Electronic surveillance robustness test in OLS regression models (strategy sub. logic)
#### THE FOLLOWING COMMANDS REPRODUCE TABLE 12 IN THE ONLINE APPENDIX ####
m14 <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + realised_telephone_imputed + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m15 <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + realised_acustic_imputed + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m16 <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + abt26_imputed + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
stargazer(list(m14, m15, m16), title = "OLS regression with controls for electronic surveillance tools",
          omit="factor\\(year", add.lines=list(c("Year fixed effects", rep("YES", 3))))


# Exclude outlier Leipzig 1989 (information logic)
#### THE FOLLOWING COMMANDS REPRODUCE TABLE 6 IN THE ONLINE APPENDIX ####
m_3a <- lm(pris_per_ht ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = without_leipzig_88)
m_4a <- lm(pris_per_ht ~ lag(sum_im_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = without_leipzig_88)
m3 <- lm(pris_per_ht ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = without_leipzig_88)
m4 <- lm(pris_per_ht ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = without_leipzig_88)
stargazer(list(m_3a, m_4a, m3, m4), title = "Regression results", 
          omit="factor\\(year", add.lines=list(c("Year fixed effects", rep("YES", 4))))


# Exclude outlier Leipzig 1989 (strategy sub. logic)
#### THE FOLLOWING COMMANDS REPRODUCE TABLE 15 IN THE ONLINE APPENDIX ####
m_3a <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = without_leipzig_88)
m_4a <- lm(zersetz_incidence ~ lag(sum_im_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = without_leipzig_88)
m3 <- lm(zersetz_incidence ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = without_leipzig_88)
m4 <- lm(zersetz_incidence ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = without_leipzig_88)
stargazer(list(m_3a, m_4a, m3, m4), title = "Regression results", 
          omit="factor\\(year", add.lines=list(c("Year fixed effects", rep("YES", 4))))


# Control for out-migration (information logic)
#### THE FOLLOWING COMMANDS REPRODUCE TABLE 4 IN THE ONLINE APPENDIX ####
m_16a <- lm(pris_per_ht ~ lag(total_ims_per_ht) + lag(pop_change_yearly) + city_county + lag(percentage_construction_workers) + 
              lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
              protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m_16a. <- lm(pris_per_ht ~ lag(sum_im_impute_per_ht) + lag(pop_change_yearly) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m16 <- lm(pris_per_ht ~ lag(imsopks) + lag(pop_change_yearly) + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m16. <- lm(pris_per_ht ~ lag(imsopks_impute) + lag(pop_change_yearly) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
stargazer(list(m_16a, m_16a., m16, m16.), title = "Regression results", 
          omit="factor\\(year", add.lines=list(c("Year fixed effects", rep("YES", 4))))


# Control for out-migration (strategy sub. logic)
#### THE FOLLOWING COMMANDS REPRODUCE TABLE 13 IN THE ONLINE APPENDIX ####
m_16a <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + lag(pop_change_yearly) + city_county + lag(percentage_construction_workers) + 
              lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
              protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m_16a. <- lm(zersetz_incidence ~ lag(sum_im_impute_per_ht) + lag(pop_change_yearly) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m16 <- lm(zersetz_incidence ~ lag(imsopks) + lag(pop_change_yearly) + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m16. <- lm(zersetz_incidence ~ lag(imsopks_impute) + lag(pop_change_yearly) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
stargazer(list(m_16a, m_16a., m16, m16.), title = "Regression results", 
          omit="factor\\(year", add.lines=list(c("Year fixed effects", rep("YES", 4))))


# Alternative measure of IMs (IMS only) (information logic)
#### THE FOLLOWING COMMANDS REPRODUCE TABLE 5 IN THE ONLINE APPENDIX ####
m_14a <- lm(pris_per_ht ~ lag(ims_per_ht) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
              lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
              protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m_15a <- lm(pris_per_ht ~ lag(ims_only_impute_per_ht) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
              lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
              protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m14 <- lm(pris_per_ht ~ lag(imsopks_only) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m15 <- lm(pris_per_ht ~ lag(imsopks_only_impute) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
stargazer(list(m_14a, m_15a, m14, m15), title = "Regression results", 
          omit="factor\\(year", add.lines=list(c("Year fixed effects", rep("YES", 4))))


# Alternative measure of IMs (IMS only) (strategy sub. logic)
#### THE FOLLOWING COMMANDS REPRODUCE TABLE 14 IN THE ONLINE APPENDIX ####
m_14a <- lm(zersetz_incidence ~ lag(ims_per_ht) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
              lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
              protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m_15a <- lm(zersetz_incidence ~ lag(ims_only_impute_per_ht) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
              lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
              protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m14 <- lm(zersetz_incidence ~ lag(imsopks_only) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m15 <- lm(zersetz_incidence ~ lag(imsopks_only_impute) + objektdienststellen + city_county + lag(percentage_construction_workers) + 
            lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
            protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
stargazer(list(m_14a, m_15a, m14, m15), title = "Regression results", 
          omit="factor\\(year", add.lines=list(c("Year fixed effects", rep("YES", 4))))


# Information logic with clustered standard errors
#### THE FOLLOWING COMMANDS REPRODUCE TABLE 10 IN THE ONLINE APPENDIX ####
m_3a <- lm(pris_per_ht ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m3acoeffs_cl <- coeftest(m_3a, vcov = vcovCL, cluster = ~county)
m_4a <- lm(pris_per_ht ~ lag(sum_im_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m4acoeffs_cl <- coeftest(m_4a, vcov = vcovCL, cluster = ~county)
m3 <- lm(pris_per_ht ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m3coeffs_cl <- coeftest(m3, vcov = vcovCL, cluster = ~county)
m4 <- lm(pris_per_ht ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m4coeffs_cl <- coeftest(m4, vcov = vcovCL, cluster = ~county)
stargazer(list(m_3a, m_4a, m3, m4), title = "Regression results", se = list(m3acoeffs_cl[,2], m4acoeffs_cl[,2], m3coeffs_cl[,2], m4coeffs_cl[,2]),
          omit="factor\\(year", add.lines=list(c("Year fixed effects", rep("YES", 4))))


# Strategy substitution logic with clustered standard errors
#### THE FOLLOWING COMMANDS REPRODUCE TABLE 19 IN THE ONLINE APPENDIX ####
s3 <- lm(zersetz_incidence ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
s3coeffs_cl <- coeftest(s3, vcov = vcovCL, cluster = ~county)
s4 <- lm(zersetz_incidence ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
s4coeffs_cl <- coeftest(s4, vcov = vcovCL, cluster = ~county)
s_3a <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
s3acoeffs_cl <- coeftest(s_3a, vcov = vcovCL, cluster = ~county)
s_4a <- lm(zersetz_incidence ~ lag(sum_im_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
s4acoeffs_cl <- coeftest(s_4a, vcov = vcovCL, cluster = ~county)
stargazer(list(s_3a, s_4a, s3, s4), title = "Regression results", se = list(s3acoeffs_cl[,2], s4acoeffs_cl[,2], s3coeffs_cl[,2], s4coeffs_cl[,2]),
          omit="factor\\(year", add.lines=list(c("Year fixed effects", rep("YES", 4))))


# Information logic with heteroskedasiticy robust standard errors
#### THE FOLLOWING COMMANDS REPRODUCE TABLE 9 IN THE ONLINE APPENDIX ####
m_3a <- lm(pris_per_ht ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m3acoeffs_cl <- coeftest(m_3a, vcov = vcovHC(m_3a, type = "HC0"))
m_4a <- lm(pris_per_ht ~ lag(sum_im_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m4acoeffs_cl <- coeftest(m_4a, vcov = vcovHC(m_4a, type = "HC0"))
m3 <- lm(pris_per_ht ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m3coeffs_cl <- coeftest(m3, vcov = vcovHC(m3, type = "HC0"))
m4 <- lm(pris_per_ht ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
m4coeffs_cl <- coeftest(m4, vcov = vcovHC(m4, type = "HC0"))
stargazer(list(m_3a, m_4a, m3, m4), title = "Regression results", se = list(m3acoeffs_cl[,2], m4acoeffs_cl[,2], m3coeffs_cl[,2], m4coeffs_cl[,2]),
          omit="factor\\(year", add.lines=list(c("Year fixed effects", rep("YES", 4))))

# Strategy substitution logic with heteroskedasiticy robust standard errors
#### THE FOLLOWING COMMANDS REPRODUCE TABLE 18 IN THE ONLINE APPENDIX ####
s3 <- lm(zersetz_incidence ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
s3coeffs_cl <- coeftest(s3, vcov = vcovHC(s3, type = "HC0"))
s4 <- lm(zersetz_incidence ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
s4coeffs_cl <- coeftest(s4, vcov = vcovHC(s4, type = "HC0"))
s_3a <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
s3acoeffs_cl <- coeftest(s_3a, vcov = vcovHC(s_3a, type = "HC0"))
s_4a <- lm(zersetz_incidence ~ lag(sum_im_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
s4acoeffs_cl <- coeftest(s_4a, vcov = vcovHC(s_4a, type = "HC0"))
stargazer(list(s_3a, s_4a, s3, s4), title = "Regression results", se = list(s3acoeffs_cl[,2], s4acoeffs_cl[,2], s3coeffs_cl[,2], s4coeffs_cl[,2]),
          omit="factor\\(year", add.lines=list(c("Year fixed effects", rep("YES", 4))))



#### Presentation of IVs ####

# IMs per capita instrumented with WGTV signal strength
summary(first1 <- lm(total_ims_per_ht ~ WGTV_continuous + lag(opks_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
                       lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
                       protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data))
linearHypothesis(first1, "WGTV_continuous = 0", type = c("F") )
iv1 <- ivreg(pris_per_ht ~ lag(total_ims_per_ht) + lag(opks_impute_per_ht) +  city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year) | WGTV_continuous + lag(opks_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
coeftest(iv1, vcov = vcovHC, type = "HC1") 
summary(iv1, vcov = sandwich, diagnostics = TRUE)

# IMs per capita imputed instrumented with WGTV signal strength
first2 <- lm(sum_im_impute_per_ht ~ WGTV_continuous + lag(opks_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
first_pred <- first2$fitted.values
reduced = lm(pris_per_ht ~ WGTV_continuous, data = gdr_data)
iv2 = ivreg(pris_per_ht ~ lag(sum_im_impute_per_ht) | WGTV_continuous, data = gdr_data)
iv2 <- ivreg(pris_per_ht ~ lag(sum_im_impute_per_ht) + lag(opks_impute_per_ht) + city_county  + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)   + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year) | WGTV_continuous + lag(opks_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
               protests53 + lag(ovs_impute_per_ht) + factor(year), data = gdr_data)
coeftest(iv2, vcov = vcovHC, type = "HC1") 
summary(iv2, vcov = sandwich, diagnostics = TRUE)

iv.fit <- mget(paste0("iv", 1:2))
gaze.lines.ivreg.diagn <- function(x, col="p-value", row=1:3, digits=2){
  stopifnot(is.list(x))
  out <- lapply(x, function(y){
    stopifnot(class(y)=="summary.ivreg")
    y$diagnostics[row, col, drop=FALSE]
  })
  out <- as.list(data.frame(t(as.data.frame(out)), check.names = FALSE))
  for(i in 1:length(out)){
    out[[i]] <- c(names(out)[i], round(out[[i]], digits=digits))
  }
  return(out)
}
stargazer::stargazer(iv.fit[1:2], add.lines=gaze.lines.ivreg.diagn(lapply(iv.fit[1:2], summary, diagnostics=TRUE), row=1:2))
#### THE FOLLOWING COMMAND REPRODUCE TABLE 20 & 21 IN THE ONLINE APPENDIX ####
stargazer(list(first1, first2))


#### Simulation section: Information logic ####

### Main OLS model for information logic (imsopks)
m3 <- lm(pris_per_ht ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + year_85 + year_86 + year_87 + year_88, data = gdr_data)
beta_hat <- coef(m3)
V_hat <- vcov(m3) 
library(MASS)
nsim <- 1000 
S <- mvrnorm(nsim, beta_hat, V_hat) 
dim(S)

# Set up scenario
sequence <- seq(min(gdr_data$imsopks, na.rm = T), max(gdr_data$imsopks, na.rm = T), length.out = 100)
new <- cbind(1, sequence, mean(gdr_data$city_county, na.rm = T), mean(gdr_data$percentage_construction_workers, na.rm = T),
             mean(gdr_data$hospital_beds_per_ht, na.rm = T), mean(gdr_data$theater_guests_per_ht, na.rm = T),
             mean(gdr_data$industrial_productivity, na.rm = T),
             mean(gdr_data$protests53, na.rm = T), mean(gdr_data$ovs_impute_per_ht, na.rm = T), 
             mean(gdr_data$year_85, na.rm = T), mean(gdr_data$year_86, na.rm = T), mean(gdr_data$year_87, na.rm = T),
             mean(gdr_data$year_88, na.rm = T))
dim(new)

# Calculate expected values
eval <- S %*% t(new)  

# Summarize the results
median <- apply(eval, 2, median)
quants <- t(apply(eval, 2, quantile, c(0.025, 0.975)))

# Plot results
#### THE FOLLOWING COMMANDS REPRODUCE FIGURE 4b IN THE ARTICLE ####
## pdf(file = "imsopks_nonimputed.pdf")
plot(sequence,
     median,
     type = "n",
     ylab = "Expected # of Pol. Imprisonments per 100.000 citizens",
     xlab = "# IMs to # OPKs (ratio of spies to monitored individuals)",
     bty = "n",
     ylim = c(0, 12),
     las = 1
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

lines(sequence, median, lwd = 2, col = "blue")
lines(sequence, quants[, 1], lwd = 0.5, lty = "dashed", col = "grey")
lines(sequence, quants[, 2], lwd = 0.5, lty = "dashed", col = "grey")

axis(1, at = gdr_data$imsopks,
     col.ticks = "gray30",
     labels = FALSE, tck = 0.02)
## dev.off()


### Main OLS model for information logic (IMs per capita)

m_3a <- lm(pris_per_ht ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + year_85 + year_86 + year_87 + year_88, data = gdr_data)
beta_hat <- coef(m_3a)
V_hat <- vcov(m_3a) 
library(MASS)
nsim <- 1000 
S <- mvrnorm(nsim, beta_hat, V_hat) 
dim(S)

# Set up scenario
sequence <- seq(min(gdr_data$total_ims_per_ht, na.rm = T), max(gdr_data$total_ims_per_ht, na.rm = T), length.out = 100)
new <- cbind(1, sequence, mean(gdr_data$city_county, na.rm = T), mean(gdr_data$percentage_construction_workers, na.rm = T),
             mean(gdr_data$hospital_beds_per_ht, na.rm = T), mean(gdr_data$theater_guests_per_ht, na.rm = T),
             mean(gdr_data$industrial_productivity, na.rm = T),
             mean(gdr_data$protests53, na.rm = T), mean(gdr_data$ovs_impute_per_ht, na.rm = T), 
             mean(gdr_data$year_85, na.rm = T), mean(gdr_data$year_86, na.rm = T), mean(gdr_data$year_87, na.rm = T),
             mean(gdr_data$year_88, na.rm = T))
dim(new)

# Calculate expected values
eval <- S %*% t(new)  

# Summarize the results
median <- apply(eval, 2, median)
quants <- t(apply(eval, 2, quantile, c(0.025, 0.975)))

# Plot results
#### THE FOLLOWING COMMANDS REPRODUCE FIGURE 4a IN THE ARTICLE ####
## pdf(file = "imsperht_nonimputed.pdf")
plot(sequence,
     median,
     type = "n",
     ylab = "Expected # of Pol. Imprisonments per 100.000 citizens",
     xlab = "# IMs per 100.000 citizens",
     bty = "n",
     ylim = c(5,12),
     las = 1
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

lines(sequence, median, lwd = 2, col = "blue")
lines(sequence, quants[, 1], lwd = 0.5, lty = "dashed", col = "grey")
lines(sequence, quants[, 2], lwd = 0.5, lty = "dashed", col = "grey")

axis(1, at = gdr_data$total_ims_per_ht,
     col.ticks = "gray30",
     labels = FALSE, tck = 0.02)
## dev.off()



### Main OLS model for information logic (imsopks_imputed as balanced panel)

m4 <- lm(pris_per_ht ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + year_85 + year_86 + year_87 + year_88, data = gdr_data)
beta_hat <- coef(m4)
V_hat <- vcov(m4) 
library(MASS)
nsim <- 1000 
S <- mvrnorm(nsim, beta_hat, V_hat) 
dim(S)

# Set up scenario
sequence <- seq(min(gdr_data$imsopks_impute, na.rm = T), max(gdr_data$imsopks_impute, na.rm = T), length.out = 100)
new <- cbind(1, sequence, mean(gdr_data$city_county, na.rm = T), mean(gdr_data$percentage_construction_workers, na.rm = T),
             mean(gdr_data$hospital_beds_per_ht, na.rm = T), mean(gdr_data$theater_guests_per_ht, na.rm = T),
             mean(gdr_data$industrial_productivity, na.rm = T),
             mean(gdr_data$protests53, na.rm = T), mean(gdr_data$ovs_impute_per_ht, na.rm = T), 
             mean(gdr_data$year_85, na.rm = T), mean(gdr_data$year_86, na.rm = T), mean(gdr_data$year_87, na.rm = T),
             mean(gdr_data$year_88, na.rm = T))
dim(new)

# Calculate expected values
eval <- S %*% t(new)  

# Summarize the results
median <- apply(eval, 2, median)
quants <- t(apply(eval, 2, quantile, c(0.025, 0.975)))

# Plot results
#### THE FOLLOWING COMMANDS REPRODUCE FIGURE 3b IN THE ONLINE APPENDIX ####
## pdf(file = "imsopks_imputed_balanced.pdf")
plot(sequence,
     median,
     type = "n",
     ylab = "Expected # of Political Imprisonments per 100.000 citizens",
     xlab = "# IMs to # OPKs (ratio of spies to monitored individuals)",
     bty = "n",
     ylim = c(0, 12),
     las = 1
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

lines(sequence, median, lwd = 2, col = "blue")
lines(sequence, quants[, 1], lwd = 0.5, lty = "dashed", col = "grey")
lines(sequence, quants[, 2], lwd = 0.5, lty = "dashed", col = "grey")

axis(1, at = gdr_data$imsopks_impute,
     col.ticks = "gray30",
     labels = FALSE, tck = 0.02)
## dev.off()


### Main OLS model for information logic (IMs per capita imputed as balanced panel)

m_4a <- lm(pris_per_ht ~ lag(sum_im_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
             protests53 + lag(ovs_impute_per_ht) + year_85 + year_86 + year_87 + year_88, data = gdr_data)
beta_hat <- coef(m_4a)
V_hat <- vcov(m_4a) 
library(MASS)
nsim <- 1000 
S <- mvrnorm(nsim, beta_hat, V_hat) 
dim(S)

# Set up scenario
sequence <- seq(min(gdr_data$sum_im_impute_per_ht, na.rm = T), max(gdr_data$sum_im_impute_per_ht, na.rm = T), length.out = 100)
new <- cbind(1, sequence, mean(gdr_data$city_county, na.rm = T), mean(gdr_data$percentage_construction_workers, na.rm = T),
             mean(gdr_data$hospital_beds_per_ht, na.rm = T), mean(gdr_data$theater_guests_per_ht, na.rm = T),
             mean(gdr_data$industrial_productivity, na.rm = T),
             mean(gdr_data$protests53, na.rm = T), mean(gdr_data$ovs_impute_per_ht, na.rm = T), 
             mean(gdr_data$year_85, na.rm = T), mean(gdr_data$year_86, na.rm = T), mean(gdr_data$year_87, na.rm = T),
             mean(gdr_data$year_88, na.rm = T))
dim(new)

# Calculate expected values
eval <- S %*% t(new)  

# Summarize the results
median <- apply(eval, 2, median)
quants <- t(apply(eval, 2, quantile, c(0.025, 0.975)))

# Plot results
#### THE FOLLOWING COMMANDS REPRODUCE FIGURE 3a IN THE ONLINE APPENDIX ####
##pdf(file = "imsperht_imputed_balancedpanel.pdf")
plot(sequence,
     median,
     type = "n",
     ylab = "Expected # of Pol. Imprisonments per 100.000 citizens",
     xlab = "# IMs per 100.000 citizens",
     bty = "n",
     ylim = c(0,12),
     las = 1
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

lines(sequence, median, lwd = 2, col = "blue")
lines(sequence, quants[, 1], lwd = 0.5, lty = "dashed", col = "grey")
lines(sequence, quants[, 2], lwd = 0.5, lty = "dashed", col = "grey")

axis(1, at = gdr_data$sum_im_impute_per_ht,
     col.ticks = "gray30",
     labels = FALSE, tck = 0.02)
##dev.off()




#### Simulation section: Strategy substitution logic ####

### Main OLS model for strategy substitution logic (IMs per capita)
s3 <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + year_85 + year_86 + year_87 + year_88, data = gdr_data)
beta_hat <- coef(s3)
V_hat <- vcov(s3) 
library(MASS)
nsim <- 1000 
S <- mvrnorm(nsim, beta_hat, V_hat) 
dim(S)

# Set up scenario
sequence <- seq(min(gdr_data$total_ims_per_ht, na.rm = T), max(gdr_data$total_ims_per_ht, na.rm = T), length.out = 300)
new <- cbind(1, sequence, mean(gdr_data$city_county, na.rm = T), mean(gdr_data$percentage_construction_workers, na.rm = T),
             mean(gdr_data$hospital_beds_per_ht, na.rm = T), mean(gdr_data$theater_guests_per_ht, na.rm = T),
             mean(gdr_data$industrial_productivity, na.rm = T),
             mean(gdr_data$protests53, na.rm = T), mean(gdr_data$ovs_impute_per_ht, na.rm = T),
             mean(gdr_data$year_85, na.rm = T), mean(gdr_data$year_86, na.rm = T), mean(gdr_data$year_87, na.rm = T),
             mean(gdr_data$year_88, na.rm = T))
dim(new)

# Calculate expected values
eval <- S %*% t(new)  

# Summarize the results
median <- apply(eval, 2, median)
quants <- t(apply(eval, 2, quantile, c(0.025, 0.975)))

# Plot results
#### THE FOLLOWING COMMANDS REPRODUCE FIGURE 5a IN THE ARTICLE ####
## pdf(file = "decomposition_nonimputed.pdf")
plot(sequence,
     median,
     ylab = "Exp. # of decomposition per 1 million citizens",
     xlab = "# IMs per 100.000 citizens",
     bty = "n",
     ylim = c(5, 40),
     las = 1
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

lines(sequence, median, lwd = 2, col = "blue")
lines(sequence, quants[, 1], lwd = 0.5, lty = "dashed", col = "grey")
lines(sequence, quants[, 2], lwd = 0.5, lty = "dashed", col = "grey")

axis(1, at = gdr_data$total_ims_per_ht,
     col.ticks = "gray30",
     labels = FALSE, tck = 0.02)
## dev.off()


### Main OLS model for strategy substitution logic (IMs per capita imputed)
s4 <- lm(zersetz_incidence ~ lag(sum_im_impute_per_ht) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht)  + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + year_85 + year_86 + year_87 + year_88, data = gdr_data)
beta_hat <- coef(s4)
V_hat <- vcov(s4) 
library(MASS)
nsim <- 1000 
S <- mvrnorm(nsim, beta_hat, V_hat) 
dim(S)

# Set up scenario
sequence <- seq(min(gdr_data$sum_im_impute_per_ht, na.rm = T), max(gdr_data$sum_im_impute_per_ht, na.rm = T), length.out = 300)
new <- cbind(1, sequence, mean(gdr_data$city_county, na.rm = T), mean(gdr_data$percentage_construction_workers, na.rm = T),
             mean(gdr_data$hospital_beds_per_ht, na.rm = T), mean(gdr_data$theater_guests_per_ht, na.rm = T),
             mean(gdr_data$industrial_productivity, na.rm = T),
             mean(gdr_data$protests53, na.rm = T), mean(gdr_data$ovs_per_ht, na.rm = T),
             mean(gdr_data$year_85, na.rm = T), mean(gdr_data$year_86, na.rm = T), mean(gdr_data$year_87, na.rm = T),
             mean(gdr_data$year_88, na.rm = T))
dim(new)

# Calculate expected values
eval <- S %*% t(new)  

# Summarize the results
median <- apply(eval, 2, median)
quants <- t(apply(eval, 2, quantile, c(0.025, 0.975)))

# Plot results
#### THE FOLLOWING COMMANDS REPRODUCE FIGURE 5a IN THE ONLINE APPENDIX ####
## pdf(file = "decomposition_meanimputed_imsperht.pdf")
plot(sequence,
     median,
     ylab = "Exp. # of decomposition per 1 million citizens",
     xlab = "# IMs per 100.000 citizens",
     ylim = c(10, 35),
     bty = "n",
     las = 1
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

lines(sequence, median, lwd = 2, col = "blue")
lines(sequence, quants[, 1], lwd = 0.5, lty = "dashed", col = "grey")
lines(sequence, quants[, 2], lwd = 0.5, lty = "dashed", col = "grey")

axis(1, at = gdr_data$sum_im_impute_per_ht,
     col.ticks = "gray30",
     labels = FALSE, tck = 0.02)
## dev.off()

### Main OLS model for strategy substitution logic (spies to monitored individuals)
s3 <- lm(zersetz_incidence ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + year_84 + year_85 + year_86 + year_87 + year_88, data = gdr_data)
beta_hat <- coef(s3)
V_hat <- vcov(s3) 
library(MASS)
nsim <- 1000 
S <- mvrnorm(nsim, beta_hat, V_hat) 
dim(S)

# Set up scenario
sequence <- seq(min(gdr_data$imsopks, na.rm = T), max(gdr_data$imsopks, na.rm = T), length.out = 300)
new <- cbind(1, sequence, mean(gdr_data$city_county, na.rm = T), mean(gdr_data$percentage_construction_workers, na.rm = T),
             mean(gdr_data$hospital_beds_per_ht, na.rm = T), mean(gdr_data$theater_guests_per_ht, na.rm = T),
             mean(gdr_data$industrial_productivity, na.rm = T),
             mean(gdr_data$protests53, na.rm = T), mean(gdr_data$ovs_impute_per_ht, na.rm = T), mean(gdr_data$year_84, na.rm = T),
             mean(gdr_data$year_85, na.rm = T), mean(gdr_data$year_86, na.rm = T), mean(gdr_data$year_87, na.rm = T),
             mean(gdr_data$year_88, na.rm = T))
dim(new)

# Calculate expected values
eval <- S %*% t(new)  

# Summarize the results
median <- apply(eval, 2, median)
quants <- t(apply(eval, 2, quantile, c(0.025, 0.975)))

# Plot results
#### THE FOLLOWING COMMANDS REPRODUCE FIGURE 5b IN THE ARTICLE ####
## pdf(file = "decomposition_nonimputed_imsopks.pdf")
plot(sequence,
     median,
     ylab = "Exp. # of decomposition per 1 million citizens",
     xlab = "# IMs to # OPKs (ratio of spies to monitored individuals)",
     bty = "n",
     xlim = c(3, 50),
     las = 1
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

lines(sequence, median, lwd = 2, col = "blue")
lines(sequence, quants[, 1], lwd = 0.5, lty = "dashed", col = "grey")
lines(sequence, quants[, 2], lwd = 0.5, lty = "dashed", col = "grey")

axis(1, at = gdr_data$imsopks,
     col.ticks = "gray30",
     labels = FALSE, tck = 0.02)
## dev.off()


### Main OLS model for strategy substitution logic (spies to monitored individuals imputed)
s4 <- lm(zersetz_incidence ~ lag(imsopks_impute) + city_county + lag(percentage_construction_workers) + 
           lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
           protests53 + lag(ovs_impute_per_ht) + year_84 + year_85 + year_86 + year_87 + year_88, data = gdr_data)
beta_hat <- coef(s4)
V_hat <- vcov(s4) 
library(MASS)
nsim <- 1000 
S <- mvrnorm(nsim, beta_hat, V_hat) 
dim(S)

# Set up scenario
sequence <- seq(min(gdr_data$imsopks_impute, na.rm = T), max(gdr_data$imsopks_impute, na.rm = T), length.out = 300)
new <- cbind(1, sequence, mean(gdr_data$city_county, na.rm = T), mean(gdr_data$percentage_construction_workers, na.rm = T),
             mean(gdr_data$hospital_beds_per_ht, na.rm = T), mean(gdr_data$theater_guests_per_ht, na.rm = T),
             mean(gdr_data$industrial_productivity, na.rm = T),
             mean(gdr_data$protests53, na.rm = T), mean(gdr_data$ovs_impute_per_ht, na.rm = T), mean(gdr_data$year_84, na.rm = T),
             mean(gdr_data$year_85, na.rm = T), mean(gdr_data$year_86, na.rm = T), mean(gdr_data$year_87, na.rm = T),
             mean(gdr_data$year_88, na.rm = T))
dim(new)

# Calculate expected values
eval <- S %*% t(new)  

# Summarize the results
median <- apply(eval, 2, median)
quants <- t(apply(eval, 2, quantile, c(0.025, 0.975)))

# Plot results
#### THE FOLLOWING COMMANDS REPRODUCE FIGURE 5b IN THE ONLINE APPENDIX ####
## pdf(file = "decomposition_meanimputed_imsopks.pdf")
plot(sequence,
     median,
     ylab = "Exp. # of decomposition per 1 million citizens",
     xlab = "# IMs to # OPKs (ratio of spies to monitored individuals)",
     bty = "n",
     xlim = c(3, 50),
     ylim = c(10, 30),
     las = 1
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

lines(sequence, median, lwd = 2, col = "blue")
lines(sequence, quants[, 1], lwd = 0.5, lty = "dashed", col = "grey")
lines(sequence, quants[, 2], lwd = 0.5, lty = "dashed", col = "grey")

axis(1, at = gdr_data$imsopks_impute,
     col.ticks = "gray30",
     labels = FALSE, tck = 0.02)
## dev.off()


#### Share of silent repression to total repression ####
gdr_data$sharesilent <- (gdr_data$zersetz_and_prevent) / (gdr_data$zersetz_and_prevent + gdr_data$parrests) * 100

### Share of silent repression to total repression (ims_per_ht)
share1 <- lm(sharesilent ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
               protests53 + factor(year), data = gdr_data)
beta_hat <- coef(share1)
V_hat <- vcov(share1) 
library(MASS)
nsim <- 1000 
S <- mvrnorm(nsim, beta_hat, V_hat) 
dim(S)

# Set up scenario
sequence <- seq(min(gdr_data$total_ims_per_ht, na.rm = T), max(gdr_data$total_ims_per_ht, na.rm = T), length.out = 300)
new <- cbind(1, sequence, mean(gdr_data$city_county, na.rm = T), mean(gdr_data$percentage_construction_workers, na.rm = T),
             mean(gdr_data$hospital_beds_per_ht, na.rm = T), mean(gdr_data$theater_guests_per_ht, na.rm = T),
             mean(gdr_data$industrial_productivity, na.rm = T),
             mean(gdr_data$protests53, na.rm = T),
             mean(gdr_data$year_85, na.rm = T), mean(gdr_data$year_86, na.rm = T), mean(gdr_data$year_87, na.rm = T),
             mean(gdr_data$year_88, na.rm = T))
dim(new)

# Calculate expected values
eval <- S %*% t(new)  

# Summarize the results
median <- apply(eval, 2, median)
quants <- t(apply(eval, 2, quantile, c(0.025, 0.975)))

# Plot results
#### THE FOLLOWING COMMANDS REPRODUCE FIGURE 8a IN THE ONLINE APPENDIX ####
## pdf(file = "percentagesilent_nonimputed_imsperht.pdf")
plot(sequence,
     median,
     ylab = "Percentage of silent repression to total repression",
     xlab = "# IMs per 100.000 citizens",
     ylim = c(0, 50),
     bty = "n",
     las = 1
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

lines(sequence, median, lwd = 2, col = "blue")
lines(sequence, quants[, 1], lwd = 0.5, lty = "dashed", col = "grey")
lines(sequence, quants[, 2], lwd = 0.5, lty = "dashed", col = "grey")

axis(1, at = gdr_data$total_ims_per_ht,
     col.ticks = "gray30",
     labels = FALSE, tck = 0.02)
## dev.off()

### Share of silent repression to total repression (ims_per_ht)
share1 <- lm(sharesilent ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
               lag(hospital_beds_per_ht) + lag(theater_guests_per_ht) + lag(industrial_productivity) + 
               protests53 + factor(year), data = gdr_data)
beta_hat <- coef(share1)
V_hat <- vcov(share1) 
library(MASS)
nsim <- 1000 
S <- mvrnorm(nsim, beta_hat, V_hat) 
dim(S)

# Set up scenario
sequence <- seq(min(gdr_data$imsopks, na.rm = T), max(gdr_data$imsopks, na.rm = T), length.out = 300)
new <- cbind(1, sequence, mean(gdr_data$city_county, na.rm = T), mean(gdr_data$percentage_construction_workers, na.rm = T),
             mean(gdr_data$hospital_beds_per_ht, na.rm = T), mean(gdr_data$theater_guests_per_ht, na.rm = T),
             mean(gdr_data$industrial_productivity, na.rm = T),
             mean(gdr_data$protests53, na.rm = T),
             mean(gdr_data$year_85, na.rm = T), mean(gdr_data$year_86, na.rm = T), mean(gdr_data$year_87, na.rm = T),
             mean(gdr_data$year_88, na.rm = T))
dim(new)

# Calculate expected values
eval <- S %*% t(new)  

# Summarize the results
median <- apply(eval, 2, median)
quants <- t(apply(eval, 2, quantile, c(0.025, 0.975)))

# Plot results
#### THE FOLLOWING COMMANDS REPRODUCE FIGURE 8b IN THE ONLINE APPENDIX ####
## pdf(file = "percentagesilent_nonimputed_imsopks.pdf")
plot(sequence,
     median,
     ylab = "Percentage of silent repression to total repression",
     xlab = "# IMs to # OPKs (ratio of spies to monitored individuals)",
     bty = "n",
     ylim = c(10, 60),
     las = 1
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

lines(sequence, median, lwd = 2, col = "blue")
lines(sequence, quants[, 1], lwd = 0.5, lty = "dashed", col = "grey")
lines(sequence, quants[, 2], lwd = 0.5, lty = "dashed", col = "grey")

axis(1, at = gdr_data$imsopks,
     col.ticks = "gray30",
     labels = FALSE, tck = 0.02)
## dev.off()



#### Missing imputation with Amelia (information logic) ####

model_subdata <- dplyr::select(gdr_data, pris_per_ht, pop_county, county, year, imsopks, total_ims_per_ht, WGTV_continuous, percentage_construction_workers, protests53, districtcapital,
                               dist_border, pop_change_80to88, percentage_farmers, available_flats,
                               hospital_beds_per_ht, theater_guests_per_ht, industrial_productivity, dist_Berlin, city_county, cities, opks_per_ht, ovs_per_ht, percentage_employed_persons)
set.seed(140192)
a.out <- Amelia::amelia(model_subdata, m = 5, idvars = c("pris_per_ht"), ts = "year", cs = "county", splinetime = 2, intercs = TRUE, empri = .1*nrow(model_subdata))

# Check imputations
#### THE FOLLOWING COMMANDS REPRODUCE THE FIGURES PRESENTED ON PAGE 26 IN THE ONLINE APPENDIX ####
## pdf(file = "amelia_hidburghhausen_imsopks.pdf")
tscsPlot(a.out, cs = "Hildburghausen", var = "imsopks")
## dev.off()
## pdf(file = "amelia_hidburghhausen_imsperht.pdf")
tscsPlot(a.out, cs = "Hildburghausen", var = "total_ims_per_ht")
## dev.off()
## pdf(file = "amelia_greifswald_imsopks.pdf")
tscsPlot(a.out, cs = "Greifswald", var = "imsopks")
## dev.off()
## pdf(file = "amelia_greifswald_imsperht.pdf")
tscsPlot(a.out, cs = "Greifswald", var = "total_ims_per_ht")
## dev.off()

# Create map of missing observations
keyvariables <- dplyr::select(gdr_data, ovs_clean, opks_clean, sum_im_clean, parrests, county, year)
keyvariables <- dplyr::rename(keyvariables, OVs = ovs_clean)
keyvariables <- dplyr::rename(keyvariables, OPKs = opks_clean)
keyvariables <- dplyr::rename(keyvariables, IMs_total = sum_im_clean)
keyvariables <- dplyr::rename(keyvariables, Parrests = parrests)
keyvariables <- dplyr::rename(keyvariables, County = county)
keyvariables <- dplyr::rename(keyvariables, Year = year)
#### THE FOLLOWING COMMAND REPRODUCES FIGURE 7 IN THE ONLINE APPENDIX ####
## pdf(file = "missmap.pdf")
missmap(keyvariables, csvar = "County", idvars = c("Year", "County"),
        main = "Missingness map of key variables", margins = c(5, 8))
## dev.off()

# Test information logic 
devtools::install_github('IQSS/Zelig', force = TRUE)
library(Zelig)
amelia1 <- zelig(pris_per_ht ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
                lag(hospital_beds_per_ht) + lag(industrial_productivity) + lag(percentage_employed_persons) +
                protests53 + lag(ovs_per_ht) + factor(year), data = a.out, model = "ls")
screenreg(amelia1)
amelia2 <- zelig(pris_per_ht ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
                lag(hospital_beds_per_ht)  + lag(industrial_productivity) + lag(percentage_employed_persons) +
                protests53 + lag(ovs_per_ht) + factor(year), data = a.out, model = "ls")
screenreg(amelia2)



### Simulate Amelia-imputed key models

# Information logic: spies per capita
a.out$imputations$imp1$year_83 <- 0
a.out$imputations$imp1$year_83[a.out$imputations$imp1$year == 1983] <- 1
a.out$imputations$imp1$year_84 <- 0
a.out$imputations$imp1$year_84[a.out$imputations$imp1$year == 1984] <- 1
a.out$imputations$imp1$year_85 <- 0
a.out$imputations$imp1$year_85[a.out$imputations$imp1$year == 1985] <- 1
a.out$imputations$imp1$year_86 <- 0
a.out$imputations$imp1$year_86[a.out$imputations$imp1$year == 1986] <- 1
a.out$imputations$imp1$year_87 <- 0
a.out$imputations$imp1$year_87[a.out$imputations$imp1$year == 1987] <- 1
a.out$imputations$imp1$year_88 <- 0
a.out$imputations$imp1$year_88[a.out$imputations$imp1$year == 1988] <- 1
m_a1 <- lm(pris_per_ht ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(industrial_productivity) + lag(percentage_employed_persons) +
             protests53 + lag(ovs_per_ht)  + year_84 + year_85 + year_86 + year_87, data = a.out$imputations$imp1)
beta_hat <- coef(m_a1)
V_hat <- vcov(m_a1) 
library(MASS)
nsim <- 1000 
S <- mvrnorm(nsim, beta_hat, V_hat) 
dim(S)

# Set up Scenario
sequence <- seq(min(a.out$imputations$imp1$total_ims_per_ht, na.rm = T), max(a.out$imputations$imp1$total_ims_per_ht, na.rm = T), length.out = 100)
new <- cbind(1, sequence, mean(a.out$imputations$imp1$city_county, na.rm = T), mean(a.out$imputations$imp1$percentage_construction_workers, na.rm = T),
             mean(a.out$imputations$imp1$hospital_beds_per_ht, na.rm = T), mean(a.out$imputations$imp1$industrial_productivity, na.rm = T),
             mean(a.out$imputations$imp1$percentage_employed_persons, na.rm = T),
             mean(a.out$imputations$imp1$protests53, na.rm = T), mean(a.out$imputations$imp1$ovs_per_ht, na.rm = T),
             mean(a.out$imputations$imp1$year_84, na.rm = T), mean(a.out$imputations$imp1$year_85, na.rm = T), 
             mean(a.out$imputations$imp1$year_86, na.rm = T), mean(a.out$imputations$imp1$year_87, na.rm = T))
dim(new)

# Calculate expected values
eval <- S %*% t(new)  

# Summarize the results
median <- apply(eval, 2, median)
quants <- t(apply(eval, 2, quantile, c(0.025, 0.975)))

# Plot results
#### THE FOLLOWING COMMANDS REPRODUCE FIGURE 4a IN THE ONLINE APPENDIX ####
## pdf(file = "ims_perht_amelia_ols.pdf")
plot(sequence,
     median,
     type = "n",
     ylab = "Expected # of Pol. Imprisonments per 100.000 citizens",
     xlab = "# IMs per 100.000 citizens",
     bty = "n",
     ylim = c(6, 15),
     las = 1
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

lines(sequence, median, lwd = 2, col = "blue")
lines(sequence, quants[, 1], lwd = 0.5, lty = "dashed", col = "grey")
lines(sequence, quants[, 2], lwd = 0.5, lty = "dashed", col = "grey")

axis(1, at = a.out$imputations$imp1$total_ims_per_ht,
     col.ticks = "gray30",
     labels = FALSE, tck = 0.02)
## dev.off()


# Information logic: spies per monitored individual
m_a2 <- lm(pris_per_ht ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(industrial_productivity) + lag(percentage_employed_persons) +
             protests53 + lag(ovs_per_ht)  + year_84 + year_85 + year_86 + year_87, data = a.out$imputations$imp1)
beta_hat <- coef(m_a2)
V_hat <- vcov(m_a2) 
library(MASS)
nsim <- 1000 
S <- mvrnorm(nsim, beta_hat, V_hat) 
dim(S)

# Set up Scenario
sequence <- seq(min(a.out$imputations$imp1$imsopks, na.rm = T), max(a.out$imputations$imp1$imsopks, na.rm = T), length.out = 100)
new <- cbind(1, sequence, mean(a.out$imputations$imp1$city_county, na.rm = T), mean(a.out$imputations$imp1$percentage_construction_workers, na.rm = T),
             mean(a.out$imputations$imp1$hospital_beds_per_ht, na.rm = T), mean(a.out$imputations$imp1$industrial_productivity, na.rm = T),
             mean(a.out$imputations$imp1$percentage_employed_persons, na.rm = T),
             mean(a.out$imputations$imp1$protests53, na.rm = T), mean(a.out$imputations$imp1$ovs_per_ht, na.rm = T),
             mean(a.out$imputations$imp1$year_84, na.rm = T), mean(a.out$imputations$imp1$year_85, na.rm = T), 
             mean(a.out$imputations$imp1$year_86, na.rm = T), mean(a.out$imputations$imp1$year_87, na.rm = T))
dim(new)

# Calculate expected values
eval <- S %*% t(new)  

# Summarize the results
median <- apply(eval, 2, median)
quants <- t(apply(eval, 2, quantile, c(0.025, 0.975)))

# Plot results
#### THE FOLLOWING COMMANDS REPRODUCE FIGURE 4b IN THE ONLINE APPENDIX ####
## pdf(file = "imsopks_amelia_ols.pdf")
plot(sequence,
     median,
     type = "n",
     ylab = "Expected # of Pol. Imprisonments per 100.000 citizens",
     xlab = "# IMs to # OPKs (ratios of spies to monitored individuals)",
     bty = "n",
     ylim = c(6, 15),
     las = 1
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

lines(sequence, median, lwd = 2, col = "blue")
lines(sequence, quants[, 1], lwd = 0.5, lty = "dashed", col = "grey")
lines(sequence, quants[, 2], lwd = 0.5, lty = "dashed", col = "grey")

axis(1, at = a.out$imputations$imp1$imsopks,
     col.ticks = "gray30",
     labels = FALSE, tck = 0.02)
## dev.off()


#### Missing imputation with Amelia (strategy substitution logic) ####

model_subdata <- dplyr::select(gdr_data, pris_per_ht, pop_county, county, year, imsopks, total_ims_per_ht, WGTV_continuous, percentage_construction_workers, protests53, districtcapital,
                               dist_border, pop_change_80to88, percentage_farmers, available_flats, zersetz_incidence,
                               hospital_beds_per_ht, theater_guests_per_ht, industrial_productivity, dist_Berlin, city_county, cities, opks_per_ht, ovs_per_ht, percentage_employed_persons)
set.seed(140192)
a.out <- Amelia::amelia(model_subdata, m = 5, idvars = c("pris_per_ht"), ts = "year", cs = "county", splinetime = 2, intercs = TRUE, empri = .1*nrow(model_subdata))

# Test strategy substitution logic
amelia1 <- zelig(zersetz_incidence ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
                   lag(hospital_beds_per_ht) + lag(industrial_productivity) + lag(percentage_employed_persons) +
                   protests53 + lag(ovs_per_ht) + factor(year), data = a.out, model = "ls")
screenreg(amelia1)
amelia2 <- zelig(zersetz_incidence ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
                   lag(hospital_beds_per_ht)  + lag(industrial_productivity) + lag(percentage_employed_persons) +
                   protests53 + lag(ovs_per_ht) + factor(year), data = a.out, model = "ls")
screenreg(amelia2)

### Simulate Amelia-imputed key models

# Strategy substitution logic: spies per capita
a.out$imputations$imp1$year_83 <- 0
a.out$imputations$imp1$year_83[a.out$imputations$imp1$year == 1983] <- 1
a.out$imputations$imp1$year_84 <- 0
a.out$imputations$imp1$year_84[a.out$imputations$imp1$year == 1984] <- 1
a.out$imputations$imp1$year_85 <- 0
a.out$imputations$imp1$year_85[a.out$imputations$imp1$year == 1985] <- 1
a.out$imputations$imp1$year_86 <- 0
a.out$imputations$imp1$year_86[a.out$imputations$imp1$year == 1986] <- 1
a.out$imputations$imp1$year_87 <- 0
a.out$imputations$imp1$year_87[a.out$imputations$imp1$year == 1987] <- 1
a.out$imputations$imp1$year_88 <- 0
a.out$imputations$imp1$year_88[a.out$imputations$imp1$year == 1988] <- 1
m_a1 <- lm(zersetz_incidence ~ lag(total_ims_per_ht) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(industrial_productivity) + lag(percentage_employed_persons) +
             protests53 + lag(ovs_per_ht) + year_84 + year_85 + year_86 + year_87, data = a.out$imputations$imp1)
beta_hat <- coef(m_a1)
V_hat <- vcov(m_a1) 
library(MASS)
nsim <- 1000 
S <- mvrnorm(nsim, beta_hat, V_hat) 
dim(S)

# Set up Scenario
sequence <- seq(min(a.out$imputations$imp1$total_ims_per_ht, na.rm = T), max(a.out$imputations$imp1$total_ims_per_ht, na.rm = T), length.out = 100)
new <- cbind(1, sequence, mean(a.out$imputations$imp1$city_county, na.rm = T), mean(a.out$imputations$imp1$percentage_construction_workers, na.rm = T),
             mean(a.out$imputations$imp1$hospital_beds_per_ht, na.rm = T), mean(a.out$imputations$imp1$industrial_productivity, na.rm = T),
             mean(a.out$imputations$imp1$percentage_employed_persons, na.rm = T),
             mean(a.out$imputations$imp1$protests53, na.rm = T), mean(a.out$imputations$imp1$ovs_per_ht, na.rm = T),
             mean(a.out$imputations$imp1$year_84, na.rm = T), mean(a.out$imputations$imp1$year_85, na.rm = T), 
             mean(a.out$imputations$imp1$year_86, na.rm = T), mean(a.out$imputations$imp1$year_87, na.rm = T))
dim(new)

# Calculate expected values
eval <- S %*% t(new)  

# Summarize the results
median <- apply(eval, 2, median)
quants <- t(apply(eval, 2, quantile, c(0.025, 0.975)))

# Plot results
#### THE FOLLOWING COMMANDS REPRODUCE FIGURE 6a IN THE ONLINE APPENDIX ####
## pdf(file = "ims_perht_amelia_strategysub.pdf")
plot(sequence,
     median,
     type = "n",
     ylab = "Exp. # of decomposition per 1 million citizens",
     xlab = "# IMs per 100.000 citizens",
     bty = "n",
     ylim = c(5, 25),
     las = 1
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

lines(sequence, median, lwd = 2, col = "blue")
lines(sequence, quants[, 1], lwd = 0.5, lty = "dashed", col = "grey")
lines(sequence, quants[, 2], lwd = 0.5, lty = "dashed", col = "grey")

axis(1, at = a.out$imputations$imp1$total_ims_per_ht,
     col.ticks = "gray30",
     labels = FALSE, tck = 0.02)
## dev.off()


# Strategy substitution logic: spies per monitored individual
m_a2 <- lm(zersetz_incidence ~ lag(imsopks) + city_county + lag(percentage_construction_workers) + 
             lag(hospital_beds_per_ht) + lag(industrial_productivity) + lag(percentage_employed_persons) +
             protests53 + lag(ovs_per_ht) + year_84 + year_85 + year_86 + year_87, data = a.out$imputations$imp1)
beta_hat <- coef(m_a2)
V_hat <- vcov(m_a2) 
library(MASS)
nsim <- 1000 
S <- mvrnorm(nsim, beta_hat, V_hat) 
dim(S)

# Set up Scenario
sequence <- seq(min(a.out$imputations$imp1$imsopks, na.rm = T), max(a.out$imputations$imp1$imsopks, na.rm = T), length.out = 100)
new <- cbind(1, sequence, mean(a.out$imputations$imp1$city_county, na.rm = T), mean(a.out$imputations$imp1$percentage_construction_workers, na.rm = T),
             mean(a.out$imputations$imp1$hospital_beds_per_ht, na.rm = T), mean(a.out$imputations$imp1$industrial_productivity, na.rm = T),
             mean(a.out$imputations$imp1$percentage_employed_persons, na.rm = T),
             mean(a.out$imputations$imp1$protests53, na.rm = T), mean(a.out$imputations$imp1$ovs_per_ht, na.rm = T),
             mean(a.out$imputations$imp1$year_84, na.rm = T), mean(a.out$imputations$imp1$year_85, na.rm = T), 
             mean(a.out$imputations$imp1$year_86, na.rm = T), mean(a.out$imputations$imp1$year_87, na.rm = T))
dim(new)

# Calculate expected values
eval <- S %*% t(new)  

# Summarize the results
median <- apply(eval, 2, median)
quants <- t(apply(eval, 2, quantile, c(0.025, 0.975)))

# Plot results
#### THE FOLLOWING COMMANDS REPRODUCE FIGURE 6b IN THE ONLINE APPENDIX ####
## pdf(file = "imsopks_amelia_strategysub.pdf")
plot(sequence,
     median,
     type = "n",
     ylim = c(5, 25),
     xlim = c(0, 50),
     ylab = "Exp. # of decomposition per 1 million citizens",
     xlab = "# IMs to # OPKs (ratio of spies to monitored individuals)",
     bty = "n",
     las = 1
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

polygon(
  c(rev(sequence), sequence),
  c(rev(quants[, 2]), quants[, 1]),
  col = adjustcolor("grey", alpha = 0.2),
  border = NA
)

lines(sequence, median, lwd = 2, col = "blue")
lines(sequence, quants[, 1], lwd = 0.5, lty = "dashed", col = "grey")
lines(sequence, quants[, 2], lwd = 0.5, lty = "dashed", col = "grey")

axis(1, at = a.out$imputations$imp1$imsopks,
     col.ticks = "gray30",
     labels = FALSE, tck = 0.02)
## dev.off()








