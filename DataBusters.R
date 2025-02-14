rm(list = ls())

# Libraries
library(readr) #dataset
library(pls) #PCR
library(fpp2)
library(dynlm)
library(lmtest)
library(sandwich)
library(quantmod)
library(readxl)
library(forecast)
library(ggplot2)
library(corrplot)

# Set Seed
set.seed(1234567)

# Read Data 
df = read_csv(("FRED-QD.csv"), col_types = cols(sasdate = col_date(format = "%m/%d/%Y")))
df = df[-c(1:2),] # remove first 2 rows 
df = na.omit(df) # remove rows with NAs
# 2006Q1 to 2024Q3
# Rows: 75 Columns: 246

################################
###Descriptive Statistics on GDP
################################

# GDP series as xts object
GDPC1 = xts(df$GDPC1, df$sasdate)

# GDP growth (at an annual rate) series as xts object
GDPC1_Growth <- xts(400 * log(GDPC1/lag(GDPC1)))

# plot Date against Quarterly GDP
plot(log(as.zoo(GDPC1)),
     col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date",
     main = "Quarterly GDP")

# plot Date against GDP Growth Rates
plot(as.zoo(GDPC1_Growth),
     col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date",
     main = "GDP Growth Rates")
abline(h=0, lty=2) # add horizontal line as reference 


###First we do PCA to reduce dimension 

###############################
###Principal Component Analysis # threshold: 90% of variance explained 
###############################

df = df[2:246] # removed date for easy indexing

# Group 1 
group1 = cbind(df[2:21], df[188:189])
prall = prcomp(group1, scale = TRUE)
prall.s = summary(prall)
prall.s$importance

# PC1      PC2      PC3              
# Standard deviation     3.882332 1.860054 1.219056 
# Proportion of Variance 0.685110 0.157260 0.067550 
# Cumulative Proportion  0.685110 0.842380 0.909930

# Extract the principal components
pcs1 = prall$x[, 1:3]

# Group 2 
group2 = cbind(df[22:34], df[192:194])
prall = prcomp(group2, scale = TRUE)
prall.s = summary(prall)
prall.s$importance

# PC1      PC2      PC3      PC4       
# Standard deviation     2.909752 2.056826 1.208774 1.004954 
# Proportion of Variance 0.529170 0.264410 0.091320 0.063120 
# Cumulative Proportion  0.529170 0.793570 0.884900 0.948020

# Extract the principal components
pcs2 = prall$x[, 1:4]

# Group 3
group3 = cbind(df[35:72])
prall = prcomp(group3, scale = TRUE)
prall.s = summary(prall)
prall.s$importance

# PC1      PC2      PC3      PC4     
# Standard deviation     4.764478 2.315237 1.998488 1.637562 
# Proportion of Variance 0.597380 0.141060 0.105100 0.070570 
# Cumulative Proportion  0.597380 0.738440 0.843540 0.914110 

# Extract the principal components
pcs3 = prall$x[, 1:4]

# Group 4
group4 = cbind(df[81:87],df[177:179],df[225:228])
prall = prcomp(group4, scale = TRUE)
prall.s = summary(prall)
prall.s$importance

# PC1       PC2    
# Standard deviation     3.43937 0.9879213 
# Proportion of Variance 0.84495 0.0697100 
# Cumulative Proportion  0.84495 0.9146600 

# Extract the principal components
pcs4 = prall$x[, 1:2]

# Group 5
group5 = cbind(df[88:94],df[220:221])
prall = prcomp(group5, scale = TRUE)
prall.s = summary(prall)
prall.s$importance

# PC1      PC2      PC3       
# Standard deviation     2.089703 1.700537 1.002857 
# Proportion of Variance 0.485210 0.321310 0.111750 
# Cumulative Proportion  0.485210 0.806520 0.918270

# Extract the principal components
pcs5 = prall$x[, 1:3]

# Group 6
group6 = cbind(df[95:130],df[203:213], df[231])
prall = prcomp(group6, scale = TRUE)
prall.s = summary(prall)
prall.s$importance

# PC1      PC2      
# Standard deviation     6.063281 2.645263 
# Proportion of Variance 0.765900 0.145780 
# Cumulative Proportion  0.765900 0.911680

# Extract the principal components
pcs6 = prall$x[, 1:2]

# Group 7
group7 = cbind(df[131:143],df[214])
prall = prcomp(group7, scale = TRUE)
prall.s = summary(prall)
prall.s$importance

# PC1      PC2      
# Standard deviation     3.417894 1.084047 
# Proportion of Variance 0.834430 0.083940 
# Cumulative Proportion  0.834430 0.918370

# Extract the principal components
pcs7 = prall$x[, 1:2]

# Group 8 
group8 = cbind(df[144:157], df[201:204], df[225:226])
prall = prcomp(group8, scale = TRUE)
prall.s = summary(prall)
prall.s$importance

# PC1      PC2      PC3      PC4      PC5       
# Standard deviation     2.783426 2.246371 1.476283 1.325952 1.140114 
# Proportion of Variance 0.387370 0.252310 0.108970 0.087910 0.064990 
# Cumulative Proportion  0.387370 0.639680 0.748650 0.836560 0.901550

# Extract the principal components
pcs8 = prall$x[, 1:5]

# Group 9 
group9 = cbind(df[157:166], df[196:197], df[214:216])
prall = prcomp(group9, scale = TRUE)
prall.s = summary(prall)
prall.s$importance

# PC1      PC2      PC3     
# Standard deviation     3.166226 1.582619 1.149883 
# Proportion of Variance 0.668330 0.166980 0.088150 
# Cumulative Proportion  0.668330 0.835310 0.923460 

# Extract the principal components
pcs9 = prall$x[, 1:3]

# Group 10 
group10 = cbind(df[167:174], df[221])
prall = prcomp(group10, scale = TRUE)
prall.s = summary(prall)
prall.s$importance

# PC1      PC2       
# Standard deviation     2.643408 1.272648 
# Proportion of Variance 0.776400 0.179960 
# Cumulative Proportion  0.776400 0.956360

# Extract the principal components
pcs10 = prall$x[, 1:2]

# Group 11
group11 = cbind(df[179:184])
prall = prcomp(group10, scale = TRUE)
prall.s = summary(prall)
prall.s$importance

# PC1      PC2      
# Standard deviation     2.643408 1.272648 
# Proportion of Variance 0.776400 0.179960 
# Cumulative Proportion  0.776400 0.956360 

# Extract the principal components
pcs11 = prall$x[, 1:2]

# Group 12 (PCA not used here)
group12 = cbind(df[186:187])

# Group 13
group13 = cbind(df[176],df[229:230],df[243],df[244:245])
prall = prcomp(group13, scale = TRUE)
prall.s = summary(prall)
prall.s$importance

# PC1      PC2      PC3       
# Standard deviation     1.906353 1.074367 0.963749
# Proportion of Variance 0.605700 0.192380 0.154800 
# Cumulative Proportion  0.605700 0.798070 0.952880

# Extract the principal components
pcs13 = prall$x[, 1:3]

# Group 14
group14 = cbind(df[190:191],df[232:242])
prall = prcomp(group14, scale = TRUE)
prall.s = summary(prall)
prall.s$importance

# PC1     PC2       
# Standard deviation     2.873722 1.98155 
# Proportion of Variance 0.635250 0.30204 
# Cumulative Proportion  0.635250 0.93729

# Extract the principal components
pcs14 = prall$x[, 1:2]

# Put all the PCs + Group 12 + GDPf into a single df 
# bind PCs
df_pc = cbind( pcs1, pcs2, pcs3, pcs4, pcs5, pcs6, pcs7, pcs8, pcs9, pcs10, pcs11, 
               df[186:187], pcs13, pcs14)


###Second PCA of df_pc 

prall = prcomp(df_pc, scale = TRUE)
prall.s = summary(prall)
prall.s$importance

# PC1      PC2      PC3      PC4      PC5      PC6      PC7     
# Standard deviation     3.467323 2.713374 2.538325 2.076205 1.490846 1.401805 1.137011
# Proportion of Variance 0.308260 0.188780 0.165210 0.110530 0.056990 0.050390 0.033150
# Cumulative Proportion  0.308260 0.497040 0.662250 0.772780 0.829770 0.880160 0.913310

# Extract the principal components
secondpc = prall$x[, 1:7]

# Bind PC and GDP 
df_pc = cbind(df[1], secondpc)

################################################################################

###Correlation tables

y <- cbind(df[1])
# Had to call function directly because of error
# Overall:
correlation_matrix <- cor(df, use = "complete.obs")
corrplot::corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 100) #Takes VERY long to load 

# Grp1:
combi1 <- cbind(group1, y)
cor1 <- cor(combi1, use = "complete.obs")
corrplot::corrplot(cor1, method = "color", type = "upper", tl.col = "black", tl.srt = 100) 

# Grp2:
combi2 <- cbind(group2, y)
cor2 <- cor(combi2, use = "complete.obs")
corrplot::corrplot(cor2, method = "color", type = "upper", tl.col = "black", tl.srt = 100) 

# Grp3:
combi3 <- cbind(group3, y)
cor3 <- cor(combi3, use = "complete.obs")
corrplot::corrplot(cor3, method = "color", type = "upper", tl.col = "black", tl.srt = 90) 

# Grp4:
combi4 <- cbind(group4, y)
cor4 <- cor(combi4, use = "complete.obs")
corrplot::corrplot(cor4, method = "color", type = "upper", tl.col = "black", tl.srt = 100) 

# Grp5:
combi5 <- cbind(group5, y)
cor5 <- cor(combi5, use = "complete.obs")
corrplot::corrplot(cor5, method = "color", type = "upper", tl.col = "black", tl.srt = 100) 

# Grp6:
combi6 <- cbind(group6, y)
cor6 <- cor(combi6, use = "complete.obs")
corrplot::corrplot(cor6, method = "color", type = "upper", tl.col = "black", tl.srt = 90) 

# Grp7:
combi7 <- cbind(group7, y)
cor7 <- cor(combi7, use = "complete.obs")
corrplot::corrplot(cor7, method = "color", type = "upper", tl.col = "black", tl.srt = 90) 

# Grp8:
combi8 <- cbind(group8, y)
cor8 <- cor(combi8, use = "complete.obs")
corrplot::corrplot(cor8, method = "color", type = "upper", tl.col = "black", tl.srt = 90) 

# Grp9:
combi9 <- cbind(group9, y)
cor9 <- cor(combi9, use = "complete.obs")
corrplot::corrplot(cor9, method = "color", type = "upper", tl.col = "black", tl.srt = 90) 

# Grp10:
combi10 <- cbind(group10, y)
cor10 <- cor(combi10, use = "complete.obs")
corrplot::corrplot(cor10, method = "color", type = "upper", tl.col = "black", tl.srt = 90) 

# Grp11:
combi11 <- cbind(group11, y)
cor11 <- cor(combi11, use = "complete.obs")
corrplot::corrplot(cor11, method = "color", type = "upper", tl.col = "black", tl.srt = 90) 

# Grp12:
combi12 <- cbind(group12, y)
cor12 <- cor(combi12, use = "complete.obs")
corrplot::corrplot(cor12, method = "color", type = "upper", tl.col = "black", tl.srt = 90) 

#Grp13:
combi13 <- cbind(group13, y)
cor13 <- cor(combi13, use = "complete.obs")
corrplot::corrplot(cor13, method = "color", type = "upper", tl.col = "black", tl.srt = 90) 

#Grp14:
combi14 <- cbind(group14, y)
cor14 <- cor(combi14, use = "complete.obs")
corrplot::corrplot(cor14, method = "color", type = "upper", tl.col = "black", tl.srt = 90) 

#############################################
###Autoregressive Distributed Lag (ADL) Model 
#############################################

# convert GDP and PCs series to ts objects
GDPC1_ts <- ts(df_pc[1], 
               start = c(2006, 1), 
               end = c(2024, 3), 
               frequency = 4)

PC1_ts <- ts(df_pc[2], 
             start = c(2006, 1), 
             end = c(2024, 3), 
             frequency = 4)

PC2_ts <- ts(df_pc[3], 
             start = c(2006, 1), 
             end = c(2024, 3), 
             frequency = 4)

PC3_ts <- ts(df_pc[4], 
             start = c(2006, 1), 
             end = c(2024, 3), 
             frequency = 4)

PC4_ts <- ts(df_pc[5], 
             start = c(2006, 1), 
             end = c(2024, 3), 
             frequency = 4)

PC5_ts <- ts(df_pc[6], 
             start = c(2006, 1), 
             end = c(2024, 3), 
             frequency = 4)

PC6_ts <- ts(df_pc[7], 
             start = c(2006, 1), 
             end = c(2024, 3), 
             frequency = 4)

PC7_ts <- ts(df_pc[8], 
             start = c(2006, 1), 
             end = c(2024, 3), 
             frequency = 4)

# compute AIC for AR model objects of class 'dynlm'
AIC <- function(model) {
  
  ssr <- sum(model$residuals^2)
  t <- length(model$residuals)
  npar <- length(model$coef)
  
  return(
    round(c("p" = npar - 1,
            "AIC" = log(ssr/t) + 2 * npar / t,
            "Adj.R2" = summary(model)$adj.r.squared), 4)
  )
}


###AR(1)
# apply the AIC() to an intercept-only model of GDP 
AIC(dynlm(ts(GDPC1_ts) ~ 1))
# p     AIC    Adj.R2 
# 0.0000 15.3684  0.0000 

# loop AIC over models of different orders
order <- 1:6

AICs <- sapply(order, function(x) 
  "AR" = AIC(dynlm(ts(GDPC1_ts) ~ L(ts(GDPC1_ts), 1:x))))

# select the AR model with the smallest AIC
AICs[, which.min(AICs[2, ])]
# p     AIC  Adj.R2 
# 2.0000 11.3390  0.9824 

# 11.3<15.4, AR(2) is a better model than AR(1)
# lag order of Y = 2


###AR(2)
# estimate the AR(2) model
GDPC1_AR2 <- dynlm(ts(GDPC1_ts) ~ L(ts(GDPC1_ts)) + L(ts(GDPC1_ts), 2))

coeftest(GDPC1_AR2, vcov. = sandwich)
coeffs_AR2 <- coef(GDPC1_AR2)
# (Intercept)    L(ts(GDPC1_ts)) L(ts(GDPC1_ts), 2) 
# -195.9793754          0.7880723          0.2284550 

# AR(2) forecast of GDP in 2024:Q3
forecast_AR2 = coeffs_AR2[1] + coeffs_AR2[2]*tail(GDPC1_ts, 3)[2] + coeffs_AR2[3]*tail(GDPC1_ts, 3)[1] 

# compute AR(2) RMFSE 
rmfse_AR2 = sqrt(mean((tail(GDPC1_ts, 3)[3]-forecast_AR2)^2))
rmfse_AR2
# 27.45986


###Fan Chart
# data has seasonality, fit seasonal ARIMA model (SARIMA)
model <- arima(GDPC1_ts, order = c(2, 1, 0), seasonal = list(order = c(1, 1, 0), period = 4))

# Forecasting
fc <- forecast(model, h = 5, level = seq(5, 95, 10))

# Plot the fan chart
plot(fc, 
     main = "GDP Forecast (SARIMA model)", 
     showgap = FALSE, 
     fcol = "red", 
     flty = 2)

################################################################################

###Finding lag order of PC1 to 7 

# loop 'AIC()' over multiple ADL models 
order <- 1:12

# PC1
AICs <- sapply(order, function(x) 
  AIC(dynlm(GDPC1_ts ~ L(GDPC1_ts, 1:2) + 
              L(PC1_ts, 1:x),
            start = c(2006, 1), end = c(2024, 3))))

AICs[, which.min(AICs[2, ])]
# p     AIC  Adj.R2 
# 4.0000 11.2042  0.9850 

# lag order of PC1 = 2

# PC2
AICs <- sapply(order, function(x) 
  AIC(dynlm(GDPC1_ts ~ L(GDPC1_ts, 1:2) + 
              L(PC1_ts, 1:2) + 
              L(PC2_ts, 1:x),
            start = c(2006, 1), end = c(2024, 3))))

AICs[, which.min(AICs[2, ])]
# p     AIC  Adj.R2 
# 5.0000 11.1873  0.9855 

# lag order of PC2 = 1

# PC3
AICs <- sapply(order, function(x) 
  AIC(dynlm(GDPC1_ts ~ L(GDPC1_ts, 1:2) + 
              L(PC1_ts, 1:2) + 
              L(PC2_ts, 1:1) + 
              L(PC3_ts, 1:x),
            start = c(2006, 1), end = c(2024, 3))))

AICs[, which.min(AICs[2, ])]
# p     AIC  Adj.R2 
# 6.0000 11.2114  0.9853 

# lag order of PC3 = 1

# PC4
AICs <- sapply(order, function(x) 
  AIC(dynlm(GDPC1_ts ~ L(GDPC1_ts, 1:2) + 
              L(PC1_ts, 1:2) + 
              L(PC2_ts, 1:1) + 
              L(PC3_ts, 1:1) + 
              L(PC4_ts, 1:x),
            start = c(2006, 1), end = c(2024, 3))))

AICs[, which.min(AICs[2, ])]
# p     AIC  Adj.R2 
# 7.0000 11.2070  0.9856 

# lag order of PC4 = 1

# PC5
AICs <- sapply(order, function(x) 
  AIC(dynlm(GDPC1_ts ~ L(GDPC1_ts, 1:2) + 
              L(PC1_ts, 1:2) + 
              L(PC2_ts, 1:1) + 
              L(PC3_ts, 1:1) + 
              L(PC4_ts, 1:1) + 
              L(PC5_ts, 1:x) ,
            start = c(2006, 1), end = c(2024, 3))))

AICs[, which.min(AICs[2, ])]
# p     AIC  Adj.R2 
# 9.0000 11.1286  0.9870 

#lag order of PC5 = 2

# PC6
AICs <- sapply(order, function(x) 
  AIC(dynlm(GDPC1_ts ~ L(GDPC1_ts, 1:2) + 
              L(PC1_ts, 1:2) + 
              L(PC2_ts, 1:1) + 
              L(PC3_ts, 1:1) + 
              L(PC4_ts, 1:1) + 
              L(PC5_ts, 1:2) + 
              L(PC6_ts, 1:x) ,
            start = c(2006, 1), end = c(2024, 3))))

AICs[, which.min(AICs[2, ])]
# p     AIC  Adj.R2 
# 10.0000 11.0784  0.9877 

# lag order of PC6 = 1

# PC7
AICs <- sapply(order, function(x) 
  AIC(dynlm(GDPC1_ts ~ L(GDPC1_ts, 1:2) + 
              L(PC1_ts, 1:2) + 
              L(PC2_ts, 1:1) + 
              L(PC3_ts, 1:1) + 
              L(PC4_ts, 1:1) + 
              L(PC5_ts, 1:2) + 
              L(PC6_ts, 1:1) + 
              L(PC7_ts, 1:x) ,
            start = c(2006, 1), end = c(2024, 3))))

AICs[, which.min(AICs[2, ])]
# p     AIC  Adj.R2 
# 13.0000 10.9917  0.9891 

# lag order of PC7 = 3

###lowest AIC (best model) is ADL(2, (2, 1, 1, 1, 2, 1, 3))


# estimate the ADL(2, (2, 1, 1, 1, 2, 1, 3)) model of GDP
GDPC1_ADL = dynlm(GDPC1_ts ~ L(GDPC1_ts, 1:2) + 
                    L(PC1_ts, 1:2) + 
                    L(PC2_ts, 1:1) + 
                    L(PC3_ts, 1:1) + 
                    L(PC4_ts, 1:1) + 
                    L(PC5_ts, 1:2) + 
                    L(PC6_ts, 1:1) + 
                    L(PC7_ts, 1:3) ,
                  start = c(2006, 1), end = c(2024, 3))

coeftest(GDPC1_ADL, vcov. = sandwich)
# t test of coefficients:
#   
#   Estimate  Std. Error t value  Pr(>|t|)    
# (Intercept)        2.2250e+04  5.9360e+03  3.7484 0.0004123 ***
#   L(GDPC1_ts, 1:2)1 -9.1407e-02  2.0082e-01 -0.4552 0.6506905    
# L(GDPC1_ts, 1:2)2 -7.6257e-02  2.0355e-01 -0.3746 0.7092932    
# L(PC1_ts, 1:2)1    7.5087e+02  2.2437e+02  3.3465 0.0014406 ** 
#   L(PC1_ts, 1:2)2   -2.9009e+01  1.3427e+02 -0.2160 0.8297133    
# L(PC2_ts, 1:1)     1.1612e+02  2.8702e+01  4.0456 0.0001562 ***
#   L(PC3_ts, 1:1)    -1.6881e+01  1.2928e+01 -1.3058 0.1967886    
# L(PC4_ts, 1:1)    -1.0610e+02  4.4838e+01 -2.3662 0.0213328 *  
#   L(PC5_ts, 1:2)1    2.5600e+01  3.6760e+01  0.6964 0.4889570    
# L(PC5_ts, 1:2)2   -1.0077e+02  5.7859e+01 -1.7416 0.0868779 .  
# L(PC6_ts, 1:1)     1.5909e+02  4.5900e+01  3.4660 0.0010011 ** 
#   L(PC7_ts, 1:3)1   -7.3703e+01  6.9026e+01 -1.0678 0.2900524    
# L(PC7_ts, 1:3)2    7.2094e+01  6.4953e+01  1.1099 0.2716049    
# L(PC7_ts, 1:3)3   -8.6325e+01  4.9261e+01 -1.7524 0.0849864 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

################################################################################

###Prediction 
# Since dataframe is small (75), we use rolling estimation window 

# Fit ARIMA models to PCs
pc1_model <- auto.arima(PC1_ts)
pc2_model <- auto.arima(PC2_ts)
pc3_model <- auto.arima(PC3_ts)
pc4_model <- auto.arima(PC4_ts)
pc5_model <- auto.arima(PC5_ts)
pc6_model <- auto.arima(PC6_ts)
pc7_model <- auto.arima(PC7_ts)

# Forecast PC1-PC7 for 2025 Q1, Q2, Q4
h <- 5  # Forecast horizon (2024 Q4- 2025 Q4)
pc1_forecast <- forecast(pc1_model, h=h)$mean
pc2_forecast <- forecast(pc2_model, h=h)$mean
pc3_forecast <- forecast(pc3_model, h=h)$mean
pc4_forecast <- forecast(pc4_model, h=h)$mean
pc5_forecast <- forecast(pc5_model, h=h)$mean
pc6_forecast <- forecast(pc6_model, h=h)$mean
pc7_forecast <- forecast(pc7_model, h=h)$mean

# Extract coefficients from the estimated ADL model
coeffs <- coef(GDPC1_ADL)

# Initialize a vector to store GDP forecasts
gdpc1_forecast <- numeric(h)
gdpc1_forecast[1] <- last(GDPC1_ts)  # Last observed GDP

# Forecast GDP for 2024 Q4 to 2025 Q4
for (t in 2:h) {
  gdpc1_forecast[t] <- coeffs[1] +  # Intercept
    coeffs[2] * gdpc1_forecast[t-1] +  # L(GDPC1,1)
    coeffs[3] * (ifelse(t > 2, gdpc1_forecast[t-2], tail(GDPC1_ts, 2)[1])) +  # L(GDPC1,2)
    coeffs[4] * pc1_forecast[t-1] +  # L(PC1,1)
    coeffs[5] * (ifelse(t > 2, pc1_forecast[t-2], tail(PC1_ts, 2)[1])) +  # L(PC1,2)
    coeffs[6] * pc2_forecast[t-1] +  # L(PC2,1)
    coeffs[7] * pc3_forecast[t-1] +  # L(PC3,1)
    coeffs[8] * pc4_forecast[t-1] +  # L(PC4,1)
    coeffs[9] * pc5_forecast[t-1] +  # L(PC5,1)
    coeffs[10] * (ifelse(t > 2, pc5_forecast[t-2], tail(PC5_ts, 2)[1])) +  # L(PC5,2)
    coeffs[11] * pc6_forecast[t-1] +  # L(PC6,1)
    coeffs[12] * pc7_forecast[t-1] +  # L(PC7,1)
    coeffs[13] * (ifelse(t > 2, pc7_forecast[t-2], tail(PC7_ts, 2)[1])) +  # L(PC7,2)
    coeffs[14] * (ifelse(t > 3, pc7_forecast[t-3], tail(PC7_ts, 3)[1]))  # L(PC7,3)
}

# GDP forecasts 2024Q4 to 2025Q4
gdpc1_forecast
#23400.29 23453.77 23470.59 23504.38 23548.41


###Visualise the forecast
# Create a time series object for forecast
gdpc1_forecast_ts <- ts(gdpc1_forecast, start = c(2024, 4), frequency = 4)

# Combine actual and forecasted values
gdp_combined <- ts(c(GDPC1_ts, gdpc1_forecast), start = start(GDPC1_ts), frequency = 4)

# Plot GDP forecast 
autoplot(gdp_combined) +
  geom_line(color = "blue") +
  geom_point(data = data.frame(time = time(gdpc1_forecast_ts), value = gdpc1_forecast), aes(x = time, y = value), color = "red") +
  ggtitle("GDP Forecast (ADL Model)") +
  xlab("Year") + ylab("GDP Level")


###Determine whether an economic contraction will occur during 

# 1. 2025Q1
# Take average GDP within a year, 2024Q1 to 2024Q4
avg_GDP = mean(c(tail(df$GDPC1, 3), gdpc1_forecast[1]))
avg_GDP > gdpc1_forecast[2]
# FALSE 

# 2. 2025Q2
# Take average GDP within a year, 2024Q2 to 2025Q1
avg_GDP = mean(c(tail(df$GDPC1, 2), gdpc1_forecast[1:2]))
avg_GDP > gdpc1_forecast[3]
# FALSE 

# 3. 2025Q4
# Take average GDP within a year, 2024Q4 to 2025Q3
avg_GDP = mean(gdpc1_forecast[1:4])
avg_GDP > gdpc1_forecast[5]
# FALSE 

###No Contractions 