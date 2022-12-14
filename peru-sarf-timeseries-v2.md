---
title: "peru-time-series-v2"
author: "Ayse D Lokmanoglu"
date: "2022-10-13"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---

```{r setup, }
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
```

## Supplemental Code for: Social Media Amplification of Risk about COVID-19 Vaccination and Vaccine Acceptance Among Peruvian Social Media Users: A Longitudinal Time Series Analysis                             
### Authors: 
#### Ayse D. Lokmanoglu, Erik C. Nisbet, Matthew T. Osborne, Joseph Tien, Sam Malloy, Lourdes Cueva Chacón, Esteban Villa Turek, and Rod Abhari            

The code includes all the steps of the text, and regression analysis, and the visualizations. 

The data set accompanying the code: <>

For questions, or more information on the code please contact: 
Ayse D. Lokmanoglu\
ayse [dot] lokmanoglu [at] northwestern [dot] edu

### Time Series Analysis
Load packages
```{r}
library(forecast)
library(tseries)
library(dynlm)
library(vars)
library(jtools)
library(stargazer)
library(sjPlot)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(modelsummary)
library(ggthemes)
library(scales)
library(wesanderson)
library(lubridate)
library(readr)
```
Load dataset, remove index column and change date to date variable
```{r}
url<-c("https://raw.githubusercontent.com/nwccpp/peru-sarf/main/data/DataSet_Peru_DV_IV_S3.csv?token=GHSAT0AAAAAABZ6XSD57BZFVM2ZCCP3OM5WY2J5RFQ")
temp <- read.csv(url)
temp <- temp %>%
  dplyr::select(-X) %>%
  mutate(date = ymd(date))
glimpse(temp)
```
- Descriptive statistics
```{r}
temp %>% dplyr::select(sentiment_covid_vac, trust_covid_vac, pct_vu) %>%
  vtable::sumtable()
```
- Select vaccine acceptance and trust Covid vaccination cluster. Change data to time series
```{r, echo=FALSE}
dat_comm6<-temp %>% 
  dplyr::select(trust_covid_vac, pct_vu) %>%
  drop_na()
dat_comm6 <- ts(dat_comm6)
plot(dat_comm6)
```
- Test for stationary
```{r, eval=FALSE}
Acf(dat_comm6[,"trust_covid_vac"])
Acf(dat_comm6[,"pct_vu"])
adf.test(dat_comm6[,"trust_covid_vac"], k=4)
adf.test(dat_comm6[,"pct_vu"], k=4)
adf.test(diff(dat_comm6[,"trust_covid_vac"], k=4))
adf.test(diff(dat_comm6[,"pct_vu"], k=4))
kpss.test(dat_comm6[,"trust_covid_vac"])
kpss.test(dat_comm6[,"pct_vu"])
coint1 <- dynlm(trust_covid_vac~pct_vu, data=dat_comm6)
ehat1 <- resid(coint1)
adf.test(ehat1, k=4)
```
- Run the time-series analysis 
```{r}
fitvarcomm6 <- VAR(dat_comm6, p=4, type="both")
summary(fitvarcomm6) 
causality(fitvarcomm6, cause = "trust_covid_vac")
irf_comm6_pct_vu <- irf(fitvarcomm6, impulse = "trust_covid_vac", response = "pct_vu", boot = TRUE)
grangertest(pct_vu ~ trust_covid_vac, order = 4, data = dat_comm6) #test for Granger causality
```
- Select vaccine acceptance and  net-sentiment covid vaccination cluster. Change data to time series
```{r}
dat_comm8<-temp %>% 
  dplyr::select(sentiment_covid_vac, pct_vu) %>%
  drop_na()
dat_comm8 <- ts(dat_comm8)
plot(dat_comm8)
```
- Test for stationary
```{r, eval=FALSE}
Acf(dat_comm8[,"sentiment_covid_vac"])
Acf(dat_comm8[,"pct_vu"])
adf.test(dat_comm8[,"sentiment_covid_vac"], k=4)
adf.test(dat_comm8[,"pct_vu"], k=4)
adf.test(diff(dat_comm8[,"sentiment_covid_vac"], k=4))
adf.test(diff(dat_comm8[,"pct_vu"], k=4))
kpss.test(dat_comm8[,"sentiment_covid_vac"])
kpss.test(dat_comm8[,"pct_vu"])
coint1 <- dynlm(sentiment_covid_vac~pct_vu, data=dat_comm8)
ehat1 <- resid(coint1)
adf.test(ehat1, k=4)
```
- Run the time-series analysis 
```{r}
fitvarcomm8 <- VAR(dat_comm8, p=4, type="both")
summary(fitvarcomm8) 
causality(fitvarcomm8, cause = "sentiment_covid_vac")
irf_comm8_pct_vu <- irf(fitvarcomm8, impulse = "sentiment_covid_vac", response = "pct_vu", boot = TRUE)
grangertest(pct_vu ~ sentiment_covid_vac, order = 4, data = dat_comm8) # run Granger Causality test
```
- Retrieve info from main models for tables
```{r} 
varcomm8G <- fitvarcomm8[["varresult"]]$pct_vu # net sentiment model
varcomm6G <- fitvarcomm6[["varresult"]]$pct_vu # trust model
```
- Print tables
```{r}
library(sjPlot)

coef_remove_pctvu<-c("pct_vu.l1", 
               "pct_vu.l2",
               "pct_vu.l3",
               "pct_vu.l4",
               "const")

tab_model(varcomm8G, varcomm6G,
                  show.se = TRUE,
                  show.p = FALSE,
                  digits = 3,
                  wrap.labels = 25,
                  collapse.se = FALSE,
          rm.terms = coef_remove_pctvu,
          pred.labels = c("Net Sentiment (lag 1)", "Net Sentiment (lag 2)", "Net Sentiment (lag 3)", "Net Sentiment (lag 4)", "trend", "Trust (lag 1)", "Trust (lag 2)", "Trust (lag 3)", "Trust (lag 4)"),
          dv.labels = c("Vaccination Acceptance Model 1", "Vaccine Acceptance Model 2"),
                  p.style = c("scientific_stars"))
```
Robustness Checks
1. Run reverse time series
- Retrieve information for reverse causality from main models
```{r}
reversecomm8G <- fitvarcomm8[["varresult"]]$sentiment_covid_vac
reversecomm6G <- fitvarcomm6[["varresult"]]$trust_covid_vac
```
- Print tables
```{r}
coef_remove_pctvu<-c("sentiment_covid_vac.l1", 
               "sentiment_covid_vac.l2",
               "sentiment_covid_vac.l3",
               "sentiment_covid_vac.l4",
               "trust_covid_vac.l1",
               "trust_covid_vac.l2",
               "trust_covid_vac.l3",
               "trust_covid_vac.l4",
               "const")

tab_model(reversecomm8G, reversecomm6G,
                  show.se = TRUE,
                  show.p = FALSE,
                  digits = 3,
                  wrap.labels = 25,
                  collapse.se = TRUE,
          rm.terms = coef_remove_pctvu,
          pred.labels = c("Vaccine Acceptance % (lag 1)", "Vaccine Acceptance % (lag 2)", "Vaccine Acceptance % (lag 3)", "Vaccine Acceptance % (lag 4)", "trend"),
          dv.labels = c("Net Sentiment", "Trust"),
                  p.style = c("scientific_stars"))
```
2. Run trust vaccine with accept_covid_vaccine_no_appointment and appointment_not_vaccinated indicators 
- Descriptive statistics
```{r}
temp %>% dplyr::select(trust_covid_vac, v3a, v15aa) %>%
  vtable::sumtable()
```
*There are only 82 observations for ccept_covid_vaccine_no_appointment indicator 
- Select v3A and cluster trust Covid vaccination. Change data to time series
```{r, echo=FALSE}
dat_comm1s<-temp %>% 
  dplyr::select(trust_covid_vac, v3a) %>%
  drop_na()
dat_comm1s <- ts(dat_comm1s)
plot(dat_comm1s)
```
- Test for stationary
```{r, eval=FALSE}
Acf(dat_comm1s[,"trust_covid_vac"])
Acf(dat_comm1s[,"v3a"])
adf.test(dat_comm1s[,"trust_covid_vac"], k=4)
adf.test(dat_comm1s[,"v3a"], k=4)
adf.test(diff(dat_comm1s[,"trust_covid_vac"], k=4))
adf.test(diff(dat_comm1s[,"v3a"], k=4))
kpss.test(dat_comm1s[,"trust_covid_vac"])
kpss.test(dat_comm1s[,"v3a"])
coint1 <- dynlm(trust_covid_vac~v3a, data=dat_comm1s)
ehat1 <- resid(coint1)
adf.test(ehat1, k=4)
```
- Run the time-series analysis (k=4)
```{r}
fitvarcomm1s <- VAR(dat_comm1s, p=4, type="both")
summary(fitvarcomm1s) 
causality(fitvarcomm1s, cause = "trust_covid_vac")
irf_comm1s_v3a <- irf(fitvarcomm1s, impulse = "trust_covid_vac", response = "v3a", boot = TRUE)
grangertest(v3a ~ trust_covid_vac, order = 4, data = dat_comm1s) #test for Granger causality
```
- Select appointment_not_vaccinated v15a and cluster trust covid vaccination. Change data to time series
```{r}
dat_comm2s<-temp %>% 
  dplyr::select(trust_covid_vac, v15aa) %>%
  drop_na()
dat_comm2s <- ts(dat_comm2s)
plot(dat_comm2s)
```
- Test for stationary
```{r, eval=FALSE}
Acf(dat_comm2s[,"trust_covid_vac"])
Acf(dat_comm2s[,"v15aa"])
adf.test(dat_comm2s[,"trust_covid_vac"], k=4)
adf.test(dat_comm2s[,"v15aa"], k=4)
adf.test(diff(dat_comm2s[,"trust_covid_vac"], k=4))
adf.test(diff(dat_comm2s[,"v15aa"], k=4))
kpss.test(dat_comm2s[,"trust_covid_vac"])
kpss.test(dat_comm2s[,"v15aa"])
coint1 <- dynlm(trust_covid_vac~v15aa, data=dat_comm2s)
ehat1 <- resid(coint1)
adf.test(ehat1, k=4)
```
- Run the time-series analysis 
```{r}
fitvarcomm2s <- VAR(dat_comm2s, p=4, type="both")
summary(fitvarcomm2s) 
causality(fitvarcomm2s, cause = "trust_covid_vac")
irf_comm2s_v15aa <- irf(fitvarcomm2s, impulse = "trust_covid_vac", response = "v15aa", boot = TRUE)
grangertest(v15aa ~ trust_covid_vac, order = 4, data = dat_comm2s) # run Granger Causality test
```
- Select covid_vaccine v1 and cluster trust covid vaccination. Change data to time series
```{r}
dat_comm3s<-temp %>% 
  dplyr::select(trust_covid_vac, v1) %>%
  drop_na()
dat_comm3s <- ts(dat_comm3s)
plot(dat_comm3s)
```

- Test for stationary
```{r, eval=FALSE}
Acf(dat_comm3s[,"trust_covid_vac"])
Acf(dat_comm3s[,"v1"])
adf.test(dat_comm3s[,"trust_covid_vac"], k=4)
adf.test(dat_comm3s[,"v1"], k=4)
adf.test(diff(dat_comm3s[,"trust_covid_vac"], k=4))
adf.test(diff(dat_comm3s[,"v1"], k=4))
kpss.test(dat_comm3s[,"trust_covid_vac"])
kpss.test(dat_comm3s[,"v1"])
coint3s <- dynlm(trust_covid_vac~v1, data=dat_comm3s)
ehat3s <- resid(coint3s)
adf.test(ehat3s, k=4)
```
- Run the time-series analysis 
```{r}
fitvarcomm3s <- VAR(dat_comm3s, p=4, type="both")
summary(fitvarcomm3s) 
causality(fitvarcomm3s, cause = "trust_covid_vac")
irf_comm3s_v1 <- irf(fitvarcomm3s, impulse = "trust_covid_vac", response = "v1", boot = TRUE)
grangertest(v1 ~ trust_covid_vac, order = 4, data = dat_comm3s) # run Granger Causality test
```

- Retrieve info from main models for tables
```{r} 
varcomm1sG <- fitvarcomm1s[["varresult"]]$v3a 
varcomm2sG <- fitvarcomm2s[["varresult"]]$v15aa 
varcomm3sG <- fitvarcomm3s[["varresult"]]$v1
```
- Print tables
```{r}
library(sjPlot)

coef_remove_trust_s<-c("v3a.l1", 
               "v3a.l2",
               "v3a.l3",
               "v3a.l4",
               "v15aa.l1",
               "v15aa.l2",
               "v15aa.l3",
               "v15aa.l4",
               "v1.l1",
               "v1.l2",
               "v1.l3",
               "v1.l4",
               "const")

tab_model(varcomm1sG, varcomm2sG, varcomm3sG,
                  show.se = TRUE,
                  show.p = FALSE,
                  digits = 3,
                  wrap.labels = 25,
                  collapse.se = TRUE,
          rm.terms = coef_remove_trust_s,
          pred.labels = c("Trust (lag 1)", "Trust (lag 2)", "Trust (lag 3)", "Trust (lag 4)", "trend"),
          dv.labels = c("accept_covid_vaccine_no_appointment (v3a)", "appointment_not_vaccinated (v15a)", "covid_vaccine (v1)"), 
                  p.style = c("scientific_stars"))

```
3. Run net sentiment vaccine cluster with accept_covid_vaccine_no_appointment and appointment_not_vaccinated indicators 
- Descriptive statistics
```{r}
temp %>% dplyr::select(sentiment_covid_vac, v3a, v15aa, v1) %>%
  vtable::sumtable()
```
*There are only 82 observations for ccept_covid_vaccine_no_appointment indicator* 
- Select v3A and cluster trust Covid vaccination. Change data to time series
```{r, echo=FALSE}
dat_comm4s<-temp %>% 
  dplyr::select(sentiment_covid_vac, v3a) %>%
  drop_na()
dat_comm4s <- ts(dat_comm4s)
plot(dat_comm4s)
```
- Test for stationary
```{r, eval=FALSE}
Acf(dat_comm4s[,"sentiment_covid_vac"])
Acf(dat_comm4s[,"v3a"])
adf.test(dat_comm4s[,"sentiment_covid_vac"], k=4)
adf.test(dat_comm4s[,"v3a"], k=4)
adf.test(diff(dat_comm4s[,"sentiment_covid_vac"], k=4))
adf.test(diff(dat_comm4s[,"v3a"], k=4))
kpss.test(dat_comm4s[,"sentiment_covid_vac"])
kpss.test(dat_comm4s[,"v3a"])
coint4 <- dynlm(sentiment_covid_vac~v3a, data=dat_comm4s)
ehat4 <- resid(coint4)
adf.test(ehat4, k=4)
```
- Run the time-series analysis (k=4)
```{r}
fitvarcomm4s <- VAR(dat_comm4s, p=4, type="both")
summary(fitvarcomm4s) 
causality(fitvarcomm4s, cause = "sentiment_covid_vac")
irf_comm4s_v3a <- irf(fitvarcomm4s, impulse = "sentiment_covid_vac", response = "v3a", boot = TRUE)
grangertest(v3a ~ sentiment_covid_vac, order = 4, data = dat_comm4s) #test for Granger causality
```
- Select appointment_not_vaccinated v15a and net sentiment covid vaccine cluster. Change data to time series
```{r}
dat_comm5s<-temp %>% 
  dplyr::select(sentiment_covid_vac, v15aa) %>%
  drop_na()
dat_comm5s <- ts(dat_comm5s)
plot(dat_comm5s)
```
- Test for stationary
```{r, eval=FALSE}
Acf(dat_comm5s[,"sentiment_covid_vac"])
Acf(dat_comm5s[,"v15aa"])
adf.test(dat_comm5s[,"sentiment_covid_vac"], k=4)
adf.test(dat_comm5s[,"v15aa"], k=4)
adf.test(diff(dat_comm5s[,"sentiment_covid_vac"], k=4))
adf.test(diff(dat_comm5s[,"v15aa"], k=4))
kpss.test(dat_comm5s[,"sentiment_covid_vac"])
kpss.test(dat_comm5s[,"v15aa"])
coint5 <- dynlm(sentiment_covid_vac~v15aa, data=dat_comm5s)
ehat5 <- resid(coint5)
adf.test(ehat5, k=4)
```
- Run the time-series analysis 
```{r}
fitvarcomm5s <- VAR(dat_comm5s, p=4, type="both")
summary(fitvarcomm5s) 
causality(fitvarcomm5s, cause = "sentiment_covid_vac")
irf_comm5s_v15aa <- irf(fitvarcomm5s, impulse = "sentiment_covid_vac", response = "v15aa", boot = TRUE)
grangertest(v15aa ~ sentiment_covid_vac, order = 4, data = dat_comm5s) # run Granger Causality test
```
- Select covid_vaccine v1 and cluster net sentiment covid vaccination. Change data to time series
```{r}
dat_comm6s<-temp %>% 
  dplyr::select(sentiment_covid_vac, v1) %>%
  drop_na()
dat_comm6s <- ts(dat_comm6s)
plot(dat_comm6s)
```

- Test for stationary
```{r, eval=FALSE}
Acf(dat_comm6s[,"sentiment_covid_vac"])
Acf(dat_comm6s[,"v1"])
adf.test(dat_comm6s[,"sentiment_covid_vac"], k=4)
adf.test(dat_comm6s[,"v1"], k=4)
adf.test(diff(dat_comm6s[,"sentiment_covid_vac"], k=4))
adf.test(diff(dat_comm6s[,"v1"], k=4))
kpss.test(dat_comm6s[,"sentiment_covid_vac"])
kpss.test(dat_comm6s[,"v1"])
coint6s <- dynlm(sentiment_covid_vac~v1, data=dat_comm6s)
ehat6s <- resid(coint6s)
adf.test(ehat6s, k=4)
```
- Run the time-series analysis 
```{r}
fitvarcomm6s <- VAR(dat_comm6s, p=4, type="both")
summary(fitvarcomm6s) 
causality(fitvarcomm6s, cause = "sentiment_covid_vac")
irf_comm6s_v1 <- irf(fitvarcomm6s, impulse = "sentiment_covid_vac", response = "v1", boot = TRUE)
grangertest(v1 ~ sentiment_covid_vac, order = 4, data = dat_comm6s) # run Granger Causality test
```

- Retrieve info from main models for tables
```{r} 
varcomm4sG <- fitvarcomm4s[["varresult"]]$v3a 
varcomm5sG <- fitvarcomm5s[["varresult"]]$v15aa 
varcomm6sG <- fitvarcomm6s[["varresult"]]$v1 
```
- Print tables
```{r}
library(sjPlot)

coef_remove_trust_s<-c("v3a.l1", 
               "v3a.l2",
               "v3a.l3",
               "v3a.l4",
               "v15aa.l1",
               "v15aa.l2",
               "v15aa.l3",
               "v15aa.l4",
                "v1.l1",
               "v1.l2",
               "v1.l3",
               "v1.l4",
               "const")

tab_model(varcomm3sG, varcomm4sG,varcomm6sG,
                  show.se = TRUE,
                  show.p = FALSE,
                  digits = 3,
                  wrap.labels = 25,
                  collapse.se = TRUE,
          rm.terms = coef_remove_trust_s,
          pred.labels = c("Net Sentiment (lag 1)", "Net Sentiment (lag 2)", "Net Sentiment (lag 3)", "Net Sentiment (lag 4)", "trend"),
          dv.labels = c("accept_covid_vaccine_no_appointment (v3a)", "appointment_not_vaccinated (v15a)", "covid_vaccine (v1)"),
                  p.style = c("scientific_stars"))
```
