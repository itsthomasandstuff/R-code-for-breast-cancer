#R code used in the 2024 paper "The Effect of Specific Ambient Air Pollutants on Breast Cancer Incidence Across US Counties" by Thomas Robinson, Spencer Hou, and Lea Bleibel.


#The following is all the code that was executed in R in order to wrangle the data.
#However, much preliminary work was done on the concentration data sets before importing anything into R.
#First the missing and incomplete data was removed manually using Excel and one pollutant standard was isolated so as not to have repeats in the data.
#Then the manually pruned data was aggregated using SPSS.
#It is these SPSS files that are being imported into R.
#The ID for the AQI and SES files also needed to added as an extra column before importing.
#Finally, some condensing was done on the SES files to make them more usable. This is not strictly necessary, however.
#With that done, we are ready to start using R.


##Load the necessary R packages

library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(readxl)

##Import the data and convert it to data frames

#Breast cancer data 2016-2020

bc <- as.data.frame(read_excel("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Breast Cancer Incidence Rates 2016-2020.xlsx", sheet = "Condensed"))

#Yearly concentration data from aggregated SPSS file

c11 <- as.data.frame(read_sav("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Obsolete/C11a.sav"))
c12 <- as.data.frame(read_sav("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Obsolete/C12a.sav"))
c13 <- as.data.frame(read_sav("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Obsolete/C13a.sav"))
c14 <- as.data.frame(read_sav("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Obsolete/C14a.sav"))
c15 <- as.data.frame(read_sav("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Obsolete/C15a.sav"))
c16 <- as.data.frame(read_sav("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Obsolete/C16a.sav"))
c17 <- as.data.frame(read_sav("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Obsolete/C17a.sav"))
c18 <- as.data.frame(read_sav("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Obsolete/C18a.sav"))
c19 <- as.data.frame(read_sav("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Obsolete/C19a.sav"))
c20 <- as.data.frame(read_sav("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Obsolete/C20a.sav"))

#Yearly AQI data from Excel file (make sure there is a column containing the FIPS code!)

aqi11 <- na.omit(as.data.frame(read_excel("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Annual concentrations and AQI by county 2011-2020 (definitive).xlsx", sheet = "AQI 11", range = "A1:T1500")))
aqi12 <- na.omit(as.data.frame(read_excel("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Annual concentrations and AQI by county 2011-2020 (definitive).xlsx", sheet = "AQI 12", range = "A1:T1500")))
aqi13 <- na.omit(as.data.frame(read_excel("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Annual concentrations and AQI by county 2011-2020 (definitive).xlsx", sheet = "AQI 13", range = "A1:T1500")))
aqi14 <- na.omit(as.data.frame(read_excel("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Annual concentrations and AQI by county 2011-2020 (definitive).xlsx", sheet = "AQI 14", range = "A1:T1500")))
aqi15 <- na.omit(as.data.frame(read_excel("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Annual concentrations and AQI by county 2011-2020 (definitive).xlsx", sheet = "AQI 15", range = "A1:T1500")))
aqi16 <- na.omit(as.data.frame(read_excel("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Annual concentrations and AQI by county 2011-2020 (definitive).xlsx", sheet = "AQI 16", range = "A1:T1500")))
aqi17 <- na.omit(as.data.frame(read_excel("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Annual concentrations and AQI by county 2011-2020 (definitive).xlsx", sheet = "AQI 17", range = "A1:T1500")))
aqi18 <- na.omit(as.data.frame(read_excel("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Annual concentrations and AQI by county 2011-2020 (definitive).xlsx", sheet = "AQI 18", range = "A1:T1500")))
aqi19 <- na.omit(as.data.frame(read_excel("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Annual concentrations and AQI by county 2011-2020 (definitive).xlsx", sheet = "AQI 19", range = "A1:T1500")))
aqi20 <- na.omit(as.data.frame(read_excel("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Annual concentrations and AQI by county 2011-2020 (definitive).xlsx", sheet = "AQI 20", range = "A1:T1500")))

#Income by period from Excel

i_1 <- as.data.frame(read_excel("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Median Income Data/Median Earnings 2012-2016.xlsx", sheet = "Condensed"))
i_2 <- as.data.frame(read_excel("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Median Income Data/Median Earnings 2016-2020.xlsx", sheet = "Condensed"))

#Education by period from Excel

e_1 <- as.data.frame(read_excel("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Education Data/Education Attainment 2012-2016.xlsx", sheet = "Condensed Data"))
e_2 <- as.data.frame(read_excel("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Education Data/Education Attainment 2016-2020.xlsx", sheet = "Condensed Data"))

#Poverty by period from Excel

p_1 <- as.data.frame(read_excel("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Poverty 2012-2016.xlsx"))
p_2 <- as.data.frame(read_excel("C:/Users/space/OneDrive - Dawson College/Winter 2024/Extracurriculars/Youreka/DATA!!/Poverty 2016-2020.xlsx", sheet = "Condensed Data"))


##Bring the data into a usable form

#Define a function for separating the concentration data into columns by pollution indicator

separation <- function(frame_name){
  names(frame_name)[names(frame_name) == '@50thPercentile_mean'] <- "Median_mean"
  frame_name_pr <- frame_name[, c(1, 5, 7, 8, 9)]
  frame_name_sep <- frame_name_pr %>%
    pivot_wider(names_from = c(ParameterName), values_from = c(ArithmeticMean_mean, ArithmeticStandardDev_mean, Median_mean))}

#Apply the function to each concentration data set

c11s <- separation(c11)
c12s <- separation(c12)
c13s <- separation(c13)
c14s <- separation(c14)
c15s <- separation(c15)
c16s <- separation(c16)
c17s <- separation(c17)
c18s <- separation(c18)
c19s <- separation(c19)
c20s <- separation(c20)

#Define functions for taking the average over five years of each indicator for each county with two or more years of data 
#Apply that function to form two concentration data sets, respectively for 2011-2015 and for 2016-2020
#Make sure that the parameters f, g, h, i, and j correspond to the number of the column you are averaging
#There are certainly simpler ways of doing this but this is what ended up working for us

avg.df.no2 <- function(a, b, c, d, e, f, g, h, i, j) {
  avgdf <- list(a[,c(1, f)], b[,c(1, g)], c[,c(1, h)], d[,c(1, i)], e[,c(1, j)]) %>% reduce(full_join, by="ID")
  avgdf$na_count <- apply(avgdf, 1, function(x) sum(is.na(x)))
  avgdf <- subset(avgdf, na_count < 4) 
  avgdfsub <- avgdf[-c(1, 7)]
  avgdfsub$NO2_mean <- rowMeans(avgdfsub, na.rm = TRUE)
  avgdf$NO2_mean <- avgdfsub$NO2_mean
  avgdf
}

no2means1 <- avg.df.no2(c11s, c12s, c13s, c14s, c15s, 3, 5, 5, 5, 5)

no2means2 <- avg.df.no2(c16s, c17s, c18s, c19s, c20s, 4, 4, 4, 5, 4)

avg.df.o3 <- function(a, b, c, d, e, f, g, h, i, j) {
  avgdf <- list(a[,c(1, f)], b[,c(1, g)], c[,c(1, h)], d[,c(1, i)], e[,c(1, j)]) %>% reduce(full_join, by="ID")
  avgdf$na_count <- apply(avgdf, 1, function(x) sum(is.na(x)))
  avgdf <- subset(avgdf, na_count < 4) 
  avgdfsub <- avgdf[-c(1, 7)]
  avgdfsub$O3_mean <- rowMeans(avgdfsub, na.rm = TRUE)
  avgdf$O3_mean <- avgdfsub$O3_mean
  avgdf
}

o3means1 <- avg.df.o3(c11s, c12s, c13s, c14s, c15s, 2, 2, 2, 2, 2)

o3means2 <- avg.df.o3(c16s, c17s, c18s, c19s, c20s, 3, 2, 2, 2, 2)

avg.df.pm25 <- function(a, b, c, d, e, f, g, h, i, j) {
  avgdf <- list(a[,c(1, f)], b[,c(1, g)], c[,c(1, h)], d[,c(1, i)], e[,c(1, j)]) %>% reduce(full_join, by="ID")
  avgdf$na_count <- apply(avgdf, 1, function(x) sum(is.na(x)))
  avgdf <- subset(avgdf, na_count < 4) 
  avgdfsub <- avgdf[-c(1, 7)]
  avgdfsub$pm25_mean <- rowMeans(avgdfsub, na.rm = TRUE)
  avgdf$pm25_mean <- avgdfsub$pm25_mean
  avgdf
}

pm25means1 <- avg.df.pm25(c11s, c12s, c13s, c14s, c15s, 5, 3, 3, 3, 3)

pm25means2 <- avg.df.pm25(c16s, c17s, c18s, c19s, c20s, 2, 3, 3, 3, 3)

avg.df.pm10 <- function(a, b, c, d, e, f, g, h, i, j) {
  avgdf <- list(a[,c(1, f)], b[,c(1, g)], c[,c(1, h)], d[,c(1, i)], e[,c(1, j)]) %>% reduce(full_join, by="ID")
  avgdf$na_count <- apply(avgdf, 1, function(x) sum(is.na(x)))
  avgdf <- subset(avgdf, na_count < 4) 
  avgdfsub <- avgdf[-c(1, 7)]
  avgdfsub$pm10_mean <- rowMeans(avgdfsub, na.rm = TRUE)
  avgdf$pm10_mean <- avgdfsub$pm10_mean
  avgdf
}

pm10means1 <- avg.df.pm10(c11s, c12s, c13s, c14s, c15s, 4, 4, 4, 4, 4)

pm10means2 <- avg.df.pm10(c16s, c17s, c18s, c19s, c20s, 5, 5, 5, 4, 5)

avg.df.aqi <- function(a, b, c, d, e, f, g, h, i, j) {
  avgdf <- list(a[,c(20, f)], b[,c(20, g)], c[,c(20, h)], d[,c(20, i)], e[,c(20, j)]) %>% reduce(full_join, by="ID")
  avgdf$na_count <- apply(avgdf, 1, function(x) sum(is.na(x)))
  avgdf <- subset(avgdf, na_count < 4) 
  avgdfsub <- avgdf[-c(1, 7)]
  avgdfsub$aqi_mean <- rowMeans(avgdfsub, na.rm = TRUE)
  avgdf$aqi_mean <- avgdfsub$aqi_mean
  avgdf
}

aqimeans1 <- avg.df.aqi(aqi11, aqi12, aqi13, aqi14, aqi15, 14, 14, 14, 14, 14)

aqimeans2 <- avg.df.aqi(aqi16, aqi17, aqi18, aqi19, aqi20, 14, 14, 14, 14, 14)

#Rename the FIPS column to "ID" where necessary

names(bc)[names(bc) == 'FIPS'] <- 'ID'
names(e_1)[names(e_1) == 'FIPS'] <- 'ID'
names(e_2)[names(e_2) == 'FIPS'] <- 'ID'
names(i_1)[names(i_1) == 'FIPS'] <- 'ID'
names(i_2)[names(i_2) == 'FIPS'] <- 'ID'
names(p_1)[names(p_1) == 'FIPS Code'] <- 'ID'
names(p_2)[names(p_2) == 'FIPS Code'] <- 'ID'

#Change the column type of certain ID columns to numeric (not sure why this is necessary)

e_1$ID <- as.numeric(e_1$ID)
e_2$ID <- as.numeric(e_2$ID)
i_1$ID <- as.numeric(i_1$ID)
i_2$ID <- as.numeric(i_2$ID)
p_1$ID <- as.numeric(p_1$ID)
p_2$ID <- as.numeric(p_2$ID)

#Combine everything into the two full data sets

full1 <- na.omit(list(bc[,c(2, 3)], no2means1[,c(1, 8)], o3means1[,c(1, 8)], pm25means1[,c(1, 8)], pm10means1[,c(1, 8)], aqimeans1[,c(1, 8)], e_1[,c(1, 15)], i_1[,c(1, 6)], p_1[,c(1, 8)]) %>% reduce(left_join, by="ID"))

full2 <- na.omit(list(bc[,c(2, 3)], no2means2[,c(1, 8)], o3means2[,c(1, 8)], pm25means2[,c(1, 8)], pm10means2[,c(1, 8)], aqimeans2[,c(1, 8)], e_2[,c(1, 15)], i_2[,c(1, 6)], p_2[,c(1, 8)]) %>% reduce(left_join, by="ID"))


##Run the statistical analysis

#Get the correlation table

View(cor(full1))
View(cor(full2))

#Check for normality
#You can mess with the binwidth parameter to see the data better

#Data set 1

ggplot(data = full1) + geom_histogram(mapping = aes(x = `Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`), binwidth = 3) + labs(title = "BC incidence", x = "BC incidence", y = "Count")
shapiro.test(full1$"Age-Adjusted Incidence Rate([rate note]) - cases per 100,000")

ggplot(data = full1) + geom_histogram(mapping = aes(x = `NO2_mean`), binwidth = 3) + labs(title = "NO2", x = "NO2", y = "Count")
shapiro.test(full1$"NO2_mean")

ggplot(data = full1) + geom_histogram(mapping = aes(x = `O3_mean`), binwidth = .001) + labs(title = "O3", x = "O3", y = "Count")
shapiro.test(full1$"O3_mean")

ggplot(data = full1) + geom_histogram(mapping = aes(x = `pm25_mean`), binwidth = .5) + labs(title = "pm25", x = "pm25", y = "Count")
shapiro.test(full1$"pm25_mean")

ggplot(data = full1) + geom_histogram(mapping = aes(x = `pm10_mean`), binwidth = 1) + labs(title = "pm10", x = "pm10", y = "Count")
shapiro.test(full1$pm10_mean)

ggplot(data = full1) + geom_histogram(mapping = aes(x = `aqi_mean`), binwidth = 2) + labs(title = "aqi", x = "aqi", y = "Count")
shapiro.test(full1$aqi_mean)

ggplot(data = full1) + geom_histogram(mapping = aes(x = `Total percentage of people without a high school diploma`), binwidth = 1) + labs(title = "education", x = "education", y = "Count")
shapiro.test(full1$`Total percentage of people without a high school diploma`)

ggplot(data = full1) + geom_histogram(mapping = aes(x = `Percent of population below poverty line`), binwidth = 2) + labs(title = "poverty", x = "poverty", y = "Count")
shapiro.test(full1$`Percent of population below poverty line`)

ggplot(data = full1) + geom_histogram(mapping = aes(x = `Median earnings over 16 with earnings (dollars)`), binwidth = 1000) + labs(title = "income", x = "income", y = "Count")
shapiro.test(full1$`Median earnings over 16 with earnings (dollars)`)

#Data set 2

ggplot(data = full2) + geom_histogram(mapping = aes(x = `Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`), binwidth = 1) + labs(title = "BC incidence", x = "BC incidence", y = "Count")
shapiro.test(full2$"Age-Adjusted Incidence Rate([rate note]) - cases per 100,000")

ggplot(data = full2) + geom_histogram(mapping = aes(x = `NO2_mean`), binwidth = 3) + labs(title = "NO2", x = "NO2", y = "Count")
shapiro.test(full2$"NO2_mean")

ggplot(data = full2) + geom_histogram(mapping = aes(x = `O3_mean`), binwidth = .001) + labs(title = "O3", x = "O3", y = "Count")
shapiro.test(full2$"O3_mean")

ggplot(data = full2) + geom_histogram(mapping = aes(x = `pm25_mean`), binwidth = .5) + labs(title = "pm25", x = "pm25", y = "Count")
shapiro.test(full2$"pm25_mean")

ggplot(data = full2) + geom_histogram(mapping = aes(x = `pm10_mean`), binwidth = 1) + labs(title = "pm10", x = "pm10", y = "Count")
shapiro.test(full2$pm10_mean)

ggplot(data = full2) + geom_histogram(mapping = aes(x = `aqi_mean`), binwidth = 1) + labs(title = "aqi", x = "aqi", y = "Count")
shapiro.test(full2$aqi_mean)

ggplot(data = full2) + geom_histogram(mapping = aes(x = `Total percentage of people without a high school diploma`), binwidth = 1) + labs(title = "education", x = "education", y = "Count")
shapiro.test(full2$`Total percentage of people without a high school diploma`)

ggplot(data = full2) + geom_histogram(mapping = aes(x = `Percent of population below poverty line`), binwidth = 2) + labs(title = "poverty", x = "poverty", y = "Count")
shapiro.test(full2$`Percent of population below poverty line`)

ggplot(data = full2) + geom_histogram(mapping = aes(x = `Median earnings over 16 with earnings (dollars)`), binwidth = 1000) + labs(title = "income", x = "income", y = "Count")
shapiro.test(full2$`Median earnings over 16 with earnings (dollars)`)

#Run the simple regression models

re.n.1 <- lm(scale(full1$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`) ~ scale(full1$NO2_mean))
summary(re.n.1)

re.o.1 <- lm(scale(full1$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`) ~ scale(full1$O3_mean))
summary(re.o.1)

re.10.1 <- lm(scale(full1$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`) ~ scale(full1$pm10_mean))
summary(re.10.1)

re.25.1 <- lm(scale(full1$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`) ~ scale(full1$pm25_mean))
summary(re.25.1)

re.aqi.1 <- lm(scale(full1$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`) ~ scale(full1$aqi_mean))
summary(re.aqi.1)

re.n.2 <- lm(scale(full2$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`) ~ scale(full2$NO2_mean))
summary(re.n.2)

re.o.2 <- lm(scale(full2$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`) ~ scale(full2$O3_mean))
summary(re.o.2)

re.10.2 <- lm(scale(full2$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`) ~ scale(full2$pm10_mean))
summary(re.10.2)

re.25.2 <- lm(scale(full2$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`) ~ scale(full2$pm25_mean))
summary(re.25.2)

re.aqi.2 <- lm(scale(full2$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`) ~ scale(full2$aqi_mean))
summary(re.aqi.2)

#Run the multiple regression models

mult1 <- lm(scale(full1$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`) ~ scale(full1$NO2_mean) + scale(full1$O3_mean) + scale(full1$pm25_mean) + scale(full1$pm10_mean) + scale(full1$`Total percentage of people without a high school diploma`) + scale(full1$`Percent of population below poverty line`))
summary(mult1)

mult2 <- lm(scale(full2$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`) ~ scale(full2$NO2_mean) + scale(full2$O3_mean) + scale(full2$pm25_mean) + scale(full2$pm10_mean) + scale(full2$`Total percentage of people without a high school diploma`) + scale(full2$`Percent of population below poverty line`))
summary(mult2)

#Obtain the plots

plot(full1$NO2_mean, full1$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`, xlab = "NO2 Concentration Before (Parts per billion)", ylab = "Cancer Incidence (Cases per 100 000)")
abline(lm(full1$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000` ~ full1$NO2_mean))

plot(full1$O3_mean, full1$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`, xlab = "Ozone Concentration Before (Parts per million)", ylab = "Cancer Incidence (Cases per 100 000)")
abline(lm(full1$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000` ~ full1$O3_mean))

plot(full1$`Total percentage of people without a high school diploma`, full1$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`, xlab = "% Without High School Diploma Before", ylab = "Cancer Incidence (Cases per 100 000")
abline(lm(full1$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000` ~ full1$`Total percentage of people without a high school diploma`))

plot(full1$pm25_mean, full1$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`, xlab = expression("PM2.5 Concentration Before" ~ (~ mu ~ g/m^3)), ylab = "Cancer Incidence (Cases per 100 000)")
abline(lm(full1$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000` ~ full1$pm25_mean))

plot(full1$pm10_mean, full1$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`, xlab = expression("PM10 Concentration Before" ~ (~ mu ~ g/m^3)), ylab = "Cancer Incidence (Cases per 100 000)")
abline(lm(full1$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000` ~ full1$pm10_mean))


plot(full2$NO2_mean, full2$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`, xlab = "NO2 Concentration During (Parts per billion)", ylab = "Cancer Incidence (Cases per 100 000")
abline(lm(full2$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000` ~ full2$NO2_mean))

plot(full2$O3_mean, full2$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`, xlab = "Ozone Concentration During (Parts per million)", ylab = "Cancer Incidence (Cases per 100 000)")
abline(lm(full2$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000` ~ full2$O3_mean))

plot(full2$`Total percentage of people without a high school diploma`, full2$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`, xlab = "% Without High School Diploma During", ylab = "Cancer Incidence (Cases per 100 000)")
abline(lm(full2$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000` ~ full2$`Total percentage of people without a high school diploma`))

plot(full2$pm25_mean, full2$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`, xlab = expression("PM2.5 Concentration During" ~ (~ mu ~ g/m^3)), ylab = "Cancer Incidence (Cases per 100 000)")
abline(lm(full2$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000` ~ full2$pm25_mean))

plot(full2$pm10_mean, full2$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000`, xlab = expression("PM10 Concentration During" ~ (~ mu ~ g/m^3)), ylab = "Cancer Incidence (Cases per 100 000)")
abline(lm(full2$`Age-Adjusted Incidence Rate([rate note]) - cases per 100,000` ~ full2$pm10_mean))

