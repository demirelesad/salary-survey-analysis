library(readxl)
library(dplyr)
library(plyr)
library(tidyverse)


tt <- read.csv("taleptahmin.csv")
View(tt)
nrow(tt)
str(tt)
head(tt)
class(tt)
names(tt) 
class(names(tt))
summary(tt)

names(tt) <- c("CustomerAccount", "SalesDate", "ProductNumber", "SalesQuantity", "OrderNumber", "Period")
names(tt)

tt$SalesDate <- as.Date(tt$SalesDate)
str(tt)

hist(tt$SalesQuantity,
     breaks = 200,
     xlim = c(-1000,5000),
     col = c("#d35400" , "#2c3e50" , "#16a085"), #flatuicolors.com dan renk aldik 
     prob = T) 
lines(density(tt$SalesQuantity))

############################# urun satirlarini ve satislari kontrol etme

toplam <- tt %>%
  group_by(ProductNumber)%>%
  summarize(Toplam_Satis = sum(SalesQuantity))

table(tt$ProductNumber)




############################# negatif verileri cikartma

tt2 <- tt %>%
  filter(SalesQuantity >= 0 , na.rm = TRUE)
View(tt2)
nrow(tt2)  #153688 satir veri

tt_negatif <- tt %>%
  filter(SalesQuantity < 0 , na.rm = TRUE)
View(tt_negatif)
nrow(tt_negatif)  #2943 satir veri


############################# aykiri deger kontrolu

##### "z" değeri ile aykırı değer tespiti

install.packages("outliers")
library(outliers)


tt2_outliers <- scores(na.omit(tt2$SalesQuantity), type = "z" , prob = 0.95)
tt2_outliers
idst1 <- which(tt2_outliers == TRUE)
na.omit(tt2$SalesQuantity)[idst1] #1178 satir veri


min(na.omit(tt2$SalesQuantity)[idst1]) #5500
max(na.omit(tt2$SalesQuantity)[idst1]) #99000


par(mfrow = c(2,1)) # plot ekranını bölme
hist(tt2$SalesQuantity, breaks = 50, xlim = c(0,30000), ylim = c(0,1500))
hist(na.omit(tt2$SalesQuantity)[-idst1], breaks = 50, xlim = c(0,30000), ylim = c(0,1500))# aykırı değerler çıkartılınca



##### boxplot yöntemi

library(rstatix)

tt2_outliers2 <- identify_outliers(tt2[ "SalesQuantity"]) 
View(tt2_outliers2)
nrow(tt2_outliers2) #20938 satir veri 
names(tt2_outliers2)

min(tt2_outliers2[, "SalesQuantity"]) #539
max(tt2_outliers2[, "SalesQuantity"]) #99000


idst2 <- which(tt2_outliers2$is.extreme == TRUE)
extreme <- tt2_outliers2[idst2 , "SalesQuantity"]

min(extreme) #820
max(extreme) #99000
length(extreme)#13005 satir veri



################################ kayip deger kontrolu ve z'ye gore aykiri degerleri cikartma

is.na(tt2)
idst3 <- which(is.na(tt2)) 
idst3 # na değeri yok

nrow(tt2)#153688

tt3 <- tt2[-idst1 , ] #aykiri deger bulunan satirlari cikartma
nrow(tt3)#151510

################################ satis adetlerinde tamsayi kontrolu

tamsayi <- which(tt3$SalesQuantity %% 1 > 0)
tt3[tamsayi, ] 
nrow(tt3[tamsayi, ])#587 ondalikli satis adedi

tt4 <- tt3[-tamsayi, ] #ondalikli degerleri cikartma
nrow(tt4)#150923 satir veri

################################ urun secimi

PRD02 <- tt4 %>%
  filter(ProductNumber == "PRD0002" , na.rm = TRUE)
nrow(PRD02) #6224 satirlik veri

################################ PRD02 verileri duzeltme 

par(mfrow = c(1,1))
plot(PRD02$SalesQuantity ~ PRD02$SalesDate )

#yillara göre filtreleme

PRD2014 <- PRD02[PRD02$SalesDate >= "2014-01-01" & PRD02$SalesDate <= "2014-12-31" , c("CustomerAccount" , "SalesDate" , "ProductNumber" , "SalesQuantity" , "OrderNumber" , "Period" )]
PRD2015 <- PRD02[PRD02$SalesDate >= "2015-01-01" & PRD02$SalesDate <= "2015-12-31" , c("CustomerAccount" , "SalesDate" , "ProductNumber" , "SalesQuantity" , "OrderNumber" , "Period" )]
PRD2016 <- PRD02[PRD02$SalesDate >= "2016-01-01" & PRD02$SalesDate <= "2016-12-31" , c("CustomerAccount" , "SalesDate" , "ProductNumber" , "SalesQuantity" , "OrderNumber" , "Period" )]
PRD2017 <- PRD02[PRD02$SalesDate >= "2017-01-01" & PRD02$SalesDate <= "2017-12-31" , c("CustomerAccount" , "SalesDate" , "ProductNumber" , "SalesQuantity" , "OrderNumber" , "Period" )]
PRD2018 <- PRD02[PRD02$SalesDate >= "2018-01-01" & PRD02$SalesDate <= "2018-12-31" , c("CustomerAccount" , "SalesDate" , "ProductNumber" , "SalesQuantity" , "OrderNumber" , "Period" )]
PRD2019 <- PRD02[PRD02$SalesDate >= "2019-01-01" & PRD02$SalesDate <= "2019-12-31" , c("CustomerAccount" , "SalesDate" , "ProductNumber" , "SalesQuantity" , "OrderNumber" , "Period" )]


sum14 <- PRD2014 %>%
  group_by(Period)%>%
  summarize(Toplam_Satis = sum(SalesQuantity))
sum15 <- PRD2015 %>%
  group_by(Period)%>%
  summarize(Toplam_Satis = sum(SalesQuantity))
sum16 <- PRD2016 %>%
  group_by(Period)%>%
  summarize(Toplam_Satis = sum(SalesQuantity))
sum17 <- PRD2017 %>%
  group_by(Period)%>%
  summarize(Toplam_Satis = sum(SalesQuantity))
sum18 <- PRD2018 %>%
  group_by(Period)%>%
  summarize(Toplam_Satis = sum(SalesQuantity))
sum19 <- PRD2019 %>%
  group_by(Period)%>%
  summarize(Toplam_Satis = sum(SalesQuantity))

sum72 <- PRD02 %>%
  group_by(Period)%>%
  summarize(Toplam_Satis = sum(SalesQuantity))
sum72$Period <- c(1:72)


library(lubridate)

year_sum <- PRD02 %>%
  group_by(year(SalesDate))%>%
  summarize(Toplam_Satis = sum(SalesQuantity))

############################# yillara gore sacilim grafigi
str(toplam72)

plot( 1:72 , sum72$Toplam_Satis ,
      pch = 20 , bty = "L",
      xlim = c(1 , 72),
      ylim = c(0 , 160000),
      main = "Aylara Göre Satış Toplamı",
      xlab = "Aylar",
      ylab = "Satış Miktarları"
)
############################# boxplot 

boxplot(year_sum[ , c("Toplam_Satis")],
        main = "Yillara Göre Satış Adedinde Uç Yıllar",
        xlab = "2014-2019 Yıllari",
        ylab = "Satis Toplamlari",
        ylim = c(00000 , 600000),
        col = "orange",
        border = "black",
        pch = 19)

##############################333 isi haritasi 
monthyearsum <- data.frame(
  aylar = c(1:12),
  "2014" = sum14$Toplam_Satis,
  "2015" = sum15$Toplam_Satis,
  "2016" = sum16$Toplam_Satis,
  "2017" = sum17$Toplam_Satis,
  "2018" = sum18$Toplam_Satis,
  "2019" = sum19$Toplam_Satis
)

monthyearmatrix <- as.matrix(monthyearsum)#ısı haritası matrix olmalı

library(RColorBrewer)

heatmap(monthyearmatrix[ , -1 ] , scale = "column",
        Colv = NA , Rowv = NA, #aylar kolonunu ve dendrogramı çıkarttık
        cexRow = 1.3,
        cexCol = 1.3,
        col = colorRampPalette(brewer.pal(9 , "Blues"))(20))

legend("bottomright" , legend = c("Min" , "Ort" , "Max"),
       fill = colorRampPalette(brewer.pal(9 , "Blues"))(3))

##################regression time

plot(Toplam_Satis ~ Period , data = sum72)
PRDmodel <- lm(Toplam_Satis ~ Period , data = sum72)
summary(PRDmodel)

summary(sum72)
#movingaverage
library(fpp2)
library(TTR)
library(forecast) 
S <-ts(sum72[,2],start=c(1),frequency=1)
S
TS <- data.frame(S)    
TS
tttimeseries <- ts(TS)
tttimeseries

smatt <- SMA(tttimeseries, 4) # lag is 4    
smatt
smatt <- smatt[-c(1:3)]
smatt
forecasttt <- forecast(smatt, 3) # future 3 values
summary(forecasttt)


#exponential smoothing (A,N,N)

Z <-ts(sum72[ ,2], start=c(1),frequency=1)
Z
esmtt <- window(Z, end = 60)
esmtf <- window(Z, start = 61)

ses.esmt <- ses(esmtt, alpha = .2, h = 12)
autoplot(ses.esmt)

esmt.dif <- diff(esmtt)
autoplot(esmt.dif)

ses.esmt.dif <- ses(esmt.dif, alpha = .2, h = 12)
autoplot(ses.esmt.dif)

esmt.dif.test <- diff(esmtf)
accuracy(ses.esmt.dif, esmt.dif.test)


alpha <- seq(.01, .99, by = .01)
RMSE <- NA
for(i in seq_along(alpha)) {
  fit <- ses(esmt.dif, alpha = alpha[i], h = 12)
  RMSE[i] <- accuracy(fit, esmt.dif.test)[2,2]
}

# convert to a data frame and identify min alpha value
alpha.fit <- data.frame(alpha, RMSE)
alpha.min <- filter(alpha.fit, RMSE == min(RMSE))
is.na(alpha.min)

indexx <- data.frame(
  alpha = alpha.min[1 , 1],
  RMSE = alpha.min[1, 2])


# plot RMSE vs. alpha
library(ggplot2)

ggplot(alpha.fit, aes(alpha, RMSE)) +
  geom_line() +
  geom_point(data = indexx, aes(alpha, RMSE), size = 3, color = "blue") 


# uygun alpha degerine gore tahminleme
ses.esmt <- ses(esmtt, alpha = alpha.min[1, 1], h = 12)
autoplot(ses.esmt)

esmt.dif <- diff(esmtt)
autoplot(esmt.dif)

ses.esmt.dif <- ses(esmt.dif, alpha = alpha.min[1, 1], h = 12)
autoplot(ses.esmt.dif)

esmt.dif.test <- diff(esmtf)
accuracy(ses.esmt.dif, esmt.dif.test)

#Holt's exponential smoothing(ciftli ustel duzeltme)

holtesmt <- holt(esmtt, h = 12)
autoplot(holtesmt)

holtesmt$model

accuracy(holtesmt, esmtf)

######## seasonal 

head(tt3)

sum72_2 <- cbind(sum72, c(rep(1,3),rep(2,3), rep(3,3), rep(4,3)))
sum72_3 <- cbind(sum72_2, c(rep(2014,12),rep(2015,12),rep(2016,12),rep(2017,12),rep(2018,12),rep(2019,12)))
colnames(sum72_3) <- c("Period","Sales","Quarter","Year")
sum72_3 <- sum72_3[c("Year","Quarter","Period","Sales")]

R <- ts(sum72_3$Sales)
plot(R , xlim = c(0,84))

sum72_3$Quarter <- as.factor(sum72_3$Quarter)
output <- lm(Sales ~ Quarter, data = sum72_3)
summary(output)

output2 <- lm(Sales ~ factor(Quarter, exclude = "4"), data = sum72_3)
summary(output2)

png(file = "TimeSeriesGFG.png")


################ ARIMA 


# Plotting graph without forecasting
plot(Z , main = "Graph without forecasting",
     col.main = "darkgreen")


# Fitting model using arima model 
fit <- auto.arima(Z)

# Next 10 forecasted values 
forecastedValues <- forecast(fit, 12)
print(forecastedValues)

plot(forecastedValues, main = "Graph with forecasting",
     col.main = "darkgreen") 

# Fitting model using arima model without last year
fit <- auto.arima(Z)
fit2 <- auto.arima(head(Z, -12))
summary(fit2)

# Next 10 forecasted values 
forecastedValues2 <- forecast(fit2, 24)
print(forecastedValues2)

plot(forecastedValues2, main = "Graph with forecasting",
     col.main = "darkgreen") 



ddd <- data.frame(
  ID= c(101,101,101,102,102,103),
  Relationship= c("Spouse","Friend","Neighbour", "Friend", "Friend", "Spouse"),
  Weight = c(5,1,3,5,3,5)
)

desired <- data.frame(
  ID= c(101,102,103),                                                  
  Relationship= c("Spouse-Neighbour-Friend","Friend-Friend","Spouse")
)

ddd %>% 
  group_by(ID) %>% 
  summarise(Relationship = paste0(Relationship, collapse = "-"))
