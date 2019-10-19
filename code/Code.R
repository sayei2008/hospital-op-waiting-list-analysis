#Case Studies for Data Analytics Assignment 3
#Group Members:
#Surya Balakrishnan Ramakrishnan (18231072)
#Sai Krishna Lakshminarayanan (18230229)

#Including the required libraries
library(ckanr)
library(gtools)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(ggridges)


#TASK 1 Accessing the datasets available on Open Data Platform

#Fetching the dataset using ckanr
ckanr_setup(url = "https://data.gov.ie/")
records <- ckanr::package_show("op-waiting-list-by-group-hospital")

#Extracting datasets
yearly <- NA
for (i in 1:length(records$resources)) {
  yearly[i] <- as.vector(records$resources[[i]]$url) 
}

Dataf2014 <- as.data.frame(read.csv(yearly[6]))
Dataf2015 <- as.data.frame(read.csv(yearly[5]))
Dataf2016 <- as.data.frame(read.csv(yearly[4]))
Dataf2017 <- as.data.frame(read.csv(yearly[3]))
Dataf2018 <- as.data.frame(read.csv(yearly[2]))

#TASK 2 Integrating the datasets into one dataset 

#Checking if all dataframes have similar column names
colnames(Dataf2014)
colnames(Dataf2015)
colnames(Dataf2016)
colnames(Dataf2017)
colnames(Dataf2018)

#There is a colunm name mismatch with dataframe of all the years so reassigning column names

colnames(Dataf2014) <- c("Archive.Date","Group","Hospital.HIPE","Hospital","Specialty.HIPE","Specialty","Adult.Child","Age.Categorisation",
                         "Time.Bands","Count")

colnames(Dataf2015) <- c("Archive.Date","Group","Hospital.HIPE","Hospital","Specialty.HIPE","Specialty","Adult.Child","Age.Categorisation",
                         "Time.Bands","Count")


colnames(Dataf2016) <- c("Archive.Date","Group","Hospital.HIPE","Hospital","Specialty.HIPE","Specialty","Adult.Child","Age.Categorisation",
                         "Time.Bands","Count")


colnames(Dataf2017) <- c("Archive.Date","Group","Hospital.HIPE","Hospital","Specialty.HIPE","Specialty","Adult.Child","Age.Categorisation",
                         "Time.Bands","Count")

colnames(Dataf2018) <- c("Archive.Date","Group","Hospital.HIPE","Hospital","Specialty.HIPE","Specialty","Adult.Child","Age.Categorisation",
                         "Time.Bands","Count")

#Merging all the dataframes
DataFrame <- smartbind(Dataf2014,Dataf2015,Dataf2016,Dataf2017,Dataf2018)
DataFrame <- as.data.frame(DataFrame)

#Converting all columns to the appropriate datatypes such as date for date columns

#The current datatype is Factor converting it to date.
class(DataFrame$Archive.Date) 
DataFrame$Archive.Date <- as.Date(DataFrame$Archive.Date)

#The current datatype is Factor converting it to character.
class(DataFrame$Group)
DataFrame$Group <- as.character(DataFrame$Group)

#The currebt datatype is Factor converting it to character
class(DataFrame$Hospital) 
DataFrame$Hospital <- as.character(DataFrame$Hospital)

#The currebt datatype is Factor converting it to character
class(DataFrame$Specialty)
DataFrame$Specialty <- as.character(DataFrame$Specialty)

#The currebt datatype is Factor converting it to character
class(DataFrame$Adult.Child)
DataFrame$Adult.Child <- as.character(DataFrame$Adult.Child)

#The currebt datatype is Factor converting it to character
class(DataFrame$Age.Categorisation)
DataFrame$Age.Categorisation <- as.character(DataFrame$Age.Categorisation)

#The currebt datatype is Factor converting it to character
class(DataFrame$Time.Bands)
DataFrame$Time.Bands <- as.character(DataFrame$Time.Bands)

#Checking if some the column names have spelling mistake which may resulting them to be grouped as different entities for later part of the assignment.
print(unique(DataFrame$Group))
print(unique(DataFrame$Hospital))
print(unique(DataFrame$Specialty))

#Removing unnecessary columns
DataFrame$Hospital.HIPE <- NULL
DataFrame$Specialty.HIPE <- NULL

#Writing the data frame to CSV
write.csv(DataFrame,"Records.csv", row.names = FALSE)

#TASK 3 Aggregate counts

#Creating new columns for year and month separately to apply the aggregate function.
Aggregation <- DataFrame %>%
  mutate(Month = month(Archive.Date,label = T,abbr = T),
         Year = year(Archive.Date))
#Gist of the data
head(Aggregation)
#Writing the data frame as csv
write.csv(Aggregation,"Aggregation.csv", row.names = FALSE)

#Aggregation by year
ByYear <- Aggregation %>%
  group_by(Year) %>% 
  summarise(TotalPatients = sum(Count))
#Gist of the data
head(ByYear)
write.csv(ByYear,"ByYear.csv", row.names = FALSE)

#Aggregation by year and hospital name
ByYearHospital <- Aggregation %>%
  group_by(Year, Hospital) %>% 
  summarise(TotalPatients = sum(Count))
#Gist of the data
head(ByYearHospital)
write.csv(ByYearHospital,"ByYearHospital.csv", row.names = FALSE)

#Aggregation by year and by hospital group
ByYearGroup <- Aggregation %>%
  group_by(Year, Group) %>% 
  summarise(TotalPatients = sum(Count))
#Gist of the data
head(ByYearGroup)
write.csv(ByYearGroup,"ByYearGroup.csv", row.names = FALSE)

#Aggregation by year and speciality
ByYearSpecialty <- Aggregation %>%
  group_by(Year, Specialty) %>% 
  summarise(TotalPatients = sum(Count))
#Gist of the data
head(ByYearSpecialty)
write.csv(ByYearSpecialty,"ByYearSpecialty.csv", row.names = FALSE)

#Aggregation by year and age category
ByYearCategory <- Aggregation %>%
  group_by(Year, Adult.Child) %>% 
  summarise(TotalPatients = sum(Count))
#To remove columns for which category is not classified
Removed <- ByYearCategory[!grepl(" ", ByYearCategory$Adult.Child),]
ByYearCategory <- Removed
#Gist of the data
head(ByYearCategory)
write.csv(ByYearCategory,"ByYearCategory.csv", row.names = FALSE)

#Aggregation by year and age
ByYearAge <- Aggregation %>%
  group_by(Year, Age.Categorisation) %>% 
  summarise(TotalPatients = sum(Count))
#To remove columns for which category is not classified
ByYearAge$Age.Categorisation[ByYearAge$Age.Categorisation ==""] <- "none"
Removed1 <- ByYearAge[!grepl("none", ByYearAge$Age.Categorisation),]
ByYearAge <- Removed1
#Gist of the data
head(ByYearAge)
write.csv(ByYearAge,"ByYearAge.csv", row.names = FALSE)

#Aggregation by year and time band
ByYearBand <- Aggregation %>%
  group_by(Year, Time.Bands) %>% 
  summarise(TotalPatients = sum(Count))
#Gist of the data
head(ByYearBand)
write.csv(ByYearBand,"ByYearBand.csv", row.names = FALSE)

#Aggregation by year hospital and month
ByYearHospitalMonth <- Aggregation %>%
  group_by(Year,Hospital,Month) %>% 
  summarise(TotalPatients = sum(Count))
#Gist of the data
head(ByYearHospitalMonth)
write.csv(ByYearHospitalMonth,"ByYearHospitalMonth.csv", row.names = FALSE)

#Aggregation by year, Month, hospital and specialty
ByYearHospitalSpecialty <- Aggregation %>%
  group_by(Year,Month,Hospital,Specialty) %>% 
  summarise(TotalPatients = sum(Count))
#Gist of the data
head(ByYearHospitalSpecialty)
write.csv(ByYearHospitalSpecialty,"ByYearHospitalSpecialty.csv", row.names = FALSE)

#Aggregation by year, Month,  hospital and time band
ByYearHospitalBand <- Aggregation %>%
  group_by(Year,Month,Hospital,Time.Bands) %>% 
  summarise(TotalPatients = sum(Count))
#Gist of the data
head(ByYearHospitalBand)
write.csv(ByYearHospitalBand,"ByYearHospitalBand.csv", row.names = FALSE)

#Aggregation by year, Month,  hospital and age
ByYearHospitalAge <- Aggregation %>%
  group_by(Year,Month,Hospital,Age.Categorisation) %>% 
  summarise(TotalPatients = sum(Count))
#To remove columns for which category is not classified
ByYearHospitalAge$Age.Categorisation[ByYearHospitalAge$Age.Categorisation ==""] <- "NA"
Removed2 <- ByYearHospitalAge[!grepl("NA", ByYearHospitalAge$Age.Categorisation),]
ByYearAge <- Removed2
#Gist of the data
head(ByYearHospitalAge)
write.csv(ByYearHospitalAge,"ByYearHospitalAge.csv", row.names = FALSE)

#Aggregation by year, Month
ByYearMonth <- Aggregation %>%
  group_by(Year,Month) %>% 
  summarise(TotalPatients = sum(Count))
#Gist of the data
head(ByYearMonth)
write.csv(ByYearMonth,"ByYearMonth.csv", row.names = FALSE)

#TASK 4 Visualisation of the result

#Visualisatin of patient growth by year
ByYear$Year <- as.Date.character(ByYear$Year, "%Y")
ggplot(ByYear, aes(Year)) +
  ggtitle("Growth in patient every year")+
  geom_ridgeline(aes(height = TotalPatients, y=0), color = "#006400", fill = "#90EE90", size = .75) +
  scale_y_continuous(limits = c(0, 7000000),name = "Patient Count",labels = scales::comma) + 
  scale_x_date(name = "year", breaks = "1 year", minor_breaks = "1 month", labels=date_format("%Y") ) +
  theme_classic()+
  theme(plot.margin = margin(7, 7, 3, 1.5))

#Visualisation by year and hospital name
ggplot(ByYearHospital, aes(x = Year, y = TotalPatients, fill = Hospital))+
  ggtitle("Visualisation by year and hospital name")+
  geom_bar(stat = "identity", position="dodge", colour = "black")+
  ylab("Patient Count")+
  scale_y_continuous(labels = scales::comma)+
  facet_wrap(.~ Hospital)+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, vjust = .5))


#Visualisation by year and hospital name
ggplot(ByYearHospital, aes(x = Year, y = TotalPatients, fill = Hospital))+
  ggtitle("Visualisation by year and hospital name")+
  geom_bar(stat = "identity", position="dodge", colour = "black")+
  ylab("Patient Count")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, vjust = .5))

#Visualisation by year and by hospital group
ggplot(ByYearGroup, aes(x = Year, y = TotalPatients, fill = Group))+
  ggtitle("Visualisation by year and hospital group")+
  geom_bar(stat = "identity", position="dodge", colour = "black")+
  scale_fill_brewer(palette="Dark2")+
  ylab("Patient Count")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, vjust = .5))

#Visualisation by year and speciality
ggplot(ByYearSpecialty,aes(x = Year, y = TotalPatients, fill = Specialty))+
  ggtitle("Visualisation by year and specialty")+
  geom_bar(stat = "identity", position="dodge", colour = "black")+
  scale_y_continuous(labels = scales::comma)+
  ylab("Patient Count")+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, vjust = .5))        

#Visualisation by year and age category
ggplot(ByYearCategory,aes(x = Year, y = TotalPatients, fill = Adult.Child))+
  ggtitle("Visualisation by year and age category")+
  geom_bar(stat = "identity", position="dodge", colour = "black")+
  geom_text(aes(label = TotalPatients), vjust= -0.3)+
  scale_fill_brewer(palette="Pastel2")+
  ylab("Patient Count")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, vjust = .5))    


#Visualisation by year and age category
ggplot(ByYearAge,aes(x = Year, y = TotalPatients, fill = Age.Categorisation))+
  ggtitle("Visualisation by year and age category")+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette="Pastel2")+
  ylab("Patient Count")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, vjust = .5))  

#Visualisation by year and waiting time band
ggplot(ByYearBand,aes(x = Year, y = TotalPatients, fill = Time.Bands))+
  ggtitle("Visualisation by year and time band")+
  geom_bar(stat = "identity", position="dodge", colour = "black")+
  scale_fill_brewer(palette="Pastel1")+
  ylab("Patient Count")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, vjust = .5)) 

#Visualisation by year hospital and month
ggplot(ByYearHospitalMonth,aes(x = Month, y = TotalPatients, colour = Month))+
  ggtitle("Box plot by month of patient count")+
  geom_boxplot()+
  scale_fill_brewer(palette="Accent")+
  ylab("Patient Count")+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, vjust = .5)) 

#Visualisation by year, Month
ggplot(ByYearHospitalSpecialty, aes(x = Month, y = TotalPatients, colour = Year)) +
  geom_bar(stat="identity", width=0.8)  +
  ggtitle("Visualisation of patient count by month and year") +
  scale_y_continuous(labels = scales::comma,name = "Patient Count")+
  theme_classic() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, vjust = .5))

#Visualisation by year and waiting time band faceted by month
ggplot(ByYearHospitalBand,aes(x = Year, y = TotalPatients, fill = Time.Bands))+
  ggtitle("Visualisation by year and time band faceted by month")+
  geom_bar(stat = "identity", position="dodge", colour = "black")+
  scale_fill_brewer(palette="Pastel2")+
  ylab("Patient Count")+
  scale_y_continuous(labels = scales::comma)+
  facet_wrap(.~ Month)+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 60, hjust=1, vjust = .5)) 



#Visualisation by year and waiting time band faceted by month
ggplot(ByYearHospitalAge,aes(x = Year, y = TotalPatients, fill = Age.Categorisation))+
  ggtitle("Visualisation by year and time band faceted by month")+
  geom_bar(stat = "identity", position="dodge", colour = "black")+
  scale_fill_brewer(palette="Paired")+
  ylab("Patient Count")+
  scale_y_continuous(labels = scales::comma)+
  facet_wrap(.~ Month)+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 60, hjust=1, vjust = .5)) 

#Visualising a time series of patient count per year by hospital group
#Selecting and transforming the data as required
plot <- Aggregation %>%
  select(Archive.Date,Group, Count) %>%
  group_by(Archive.Date,Group) %>%
  summarise(TotalPatients = sum(Count))
#Getting the gist of data
head(plot)
#Generating the plot
ggplot(plot,aes(x = Archive.Date, y = TotalPatients, colour = Group, shape = Group))+
  ggtitle("Visualisation time series of patients by hospital group")+
  geom_point(stat = "identity")+
  geom_line()+
  scale_x_date(name = "Year", breaks = "1 year", minor_breaks = "1 month", labels = date_format("%Y"))+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_brewer(palette="Spectral")+
  ylab("Patient Count")+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, vjust = .5))

#Visualising a time series of patient count per year by waiting time band
#Selecting and transforming the data as required
plot1 <- Aggregation %>%
  select(Archive.Date, Time.Bands, Count) %>%
  group_by(Archive.Date,Time.Bands) %>%
  summarise(TotalPatients = sum(Count))
#Getting the gist of data
head(plot1)
#Generating the plot
ggplot(plot1,aes(x = Archive.Date, y = TotalPatients, colour = Time.Bands, shape = Time.Bands))+
  ggtitle("Visualisation time series of patients by waiting time bands")+
  geom_point(stat = "identity")+
  geom_line()+
  scale_x_date(name = "Year", breaks = "1 year", minor_breaks = "1 month", labels = date_format("%Y"))+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_brewer(palette="Paired")+
  ylab("Patient Count")+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, vjust = .5))

#Visualising a time series of patient count per year by specialty
#Selecting and transforming the data as required
plot2 <- Aggregation %>%
  select(Archive.Date, Specialty, Count) %>%
  group_by(Archive.Date,Specialty) %>%
  summarise(TotalPatients = sum(Count))
#Getting the gist of data
head(plot2)
#Generating the plot
ggplot(plot2,aes(x = Archive.Date, y = TotalPatients, colour = Specialty))+
  ggtitle("Visualisation time series of patients by specialty")+
  geom_point(stat = "identity")+
  geom_line()+
  scale_x_date(name = "Year", breaks = "1 year", minor_breaks = "1 month", labels = date_format("%Y"))+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_brewer(palette="Paired")+
  ylab("Patient Count")+
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size = 0.25),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, vjust = .5))


#References
# 1) https://cran.r-project.org/web/packages/ckanr/index.html
# 2) https://cran.r-project.org/web/packages/ckanr/README.html
# 3) https://stackoverflow.com/questions/3402371/combine-two-data-frames-by-rows-rbind-when-they-have-different-sets-of-columns
# 4) https://datatofish.com/export-dataframe-to-csv-in-r/
# 5) https://stackoverflow.com/questions/22249702/delete-rows-containing-specific-strings-in-r
# 6) Tutorial materials from the course Programming for Data Analytics and Data Visualisation in blackboard.
