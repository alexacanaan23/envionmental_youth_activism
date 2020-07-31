#load data
brenna.dat<-read.csv("~/Desktop/brenna/brenna_data.csv", header = FALSE) #main dataframe
theme.dat<-read.csv("~/Desktop/brenna/new.csv", header = FALSE) #theme dataframe

#change column header names for main dataframe
names(brenna.dat)<-c("User", "Date", "URL", "Tweet", "Theme")

#add theme columns by checking whether or not they fall into "theme"
#themes<-c("1A", "1B", "1C", "2A", "2B", "2C", "3D", "3A", "3B", "3C")
brenna.dat$oneA<-grepl("1A", brenna.dat$Theme, fixed=TRUE)
brenna.dat$oneB<-grepl("1B", brenna.dat$Theme, fixed=TRUE) 
brenna.dat$oneC<-grepl("1C", brenna.dat$Theme, fixed=TRUE)
brenna.dat$twoA<-grepl("2A", brenna.dat$Theme, fixed=TRUE)
brenna.dat$twoB<-grepl("2B", brenna.dat$Theme, fixed=TRUE)
brenna.dat$twoC<-grepl("2C", brenna.dat$Theme, fixed=TRUE)
brenna.dat$threeA<-grepl("3A", brenna.dat$Theme, fixed=TRUE) 
brenna.dat$threeB<-grepl("3B", brenna.dat$Theme, fixed=TRUE)
brenna.dat$threeC<-grepl("3C", brenna.dat$Theme, fixed=TRUE) 

#format the date
library(lubridate)
brenna.dat$Date<-mdy(brenna.dat$Date)

#begin plotting
library(ggplot2)

#user data frequency
ggdat.user<-data.frame(prop.table(table(brenna.dat$User)))
names(ggdat.user)<-c("User", "Frequency")
ggplot(data = ggdat.user, aes(x=User, y = Frequency, fill=User))+
  geom_bar(stat = "identity")+
  theme_bw()+
  xlab("User")+
  ggtitle("Frequency of Tweets by User")+
  geom_hline(yintercept = 0)

#date data using density
ggdate<-data.frame(brenna.dat$User, brenna.dat$Date)
names(ggdate)<-c("User", "Date")
ggplot(data = ggdate, aes(x=Date, color=User)) +
  geom_density(aes(y=..density..))+
  theme_bw()+
  xlab("Date")+
  ggtitle("Time Spread of Tweets")+
  geom_hline(yintercept = 0)

#select only 3b tweets
ggdate.3B<-subset(brenna.dat, brenna.dat$threeB==TRUE)
ggdate.3B1<-data.frame(ggdate.3B$User, ggdate.3B$Date)
names(ggdate.3B1)<- c("User", "Date")
ggplot(data = ggdate.3B1, aes(x=Date, color=User)) +
  geom_density(aes(y=..density..))+
  theme_bw()+
  xlab("Date")+
  ggtitle("Time Spread of Tweets Related to Subtheme 3B by User")+
  geom_hline(yintercept = 0)

#themes
library(dplyr)
themedat<-data.frame(brenna.dat$oneA, brenna.dat$oneB,brenna.dat$oneC,
                 brenna.dat$twoA, brenna.dat$twoB,brenna.dat$twoC,
                 brenna.dat$threeA, brenna.dat$threeB,brenna.dat$threeC)
names(themedat)<-c("oneA", "oneB", "oneC", "twoA", "twoB", "twoC", "threeA", "threeB", "threeC")
library(magrittr)
themedat %<>% mutate_if(is.logical,as.numeric)

#calculate frequency first calculate sums
oneAsum<-sum(themedat[, "oneA"])
oneBsum<-sum(themedat[, "oneB"])
oneCsum<-sum(themedat[, "oneC"])

twoAsum<-sum(themedat[, "twoA"])
twoBsum<-sum(themedat[, "twoB"])
twoCsum<-sum(themedat[, "twoC"])

threeAsum<-sum(themedat[, "threeA"])
threeBsum<-sum(themedat[, "threeB"])
threeCsum<-sum(themedat[, "threeC"])

#create a new data frame
sums<-c(146, 50, 283, 202, 48, 129, 62, 261, 242)
total<-c(925, 925, 925, 925, 925, 925, 925, 925, 925)
theme<-c("1A", "1B", "1C",
            "2A", "2B", "2C",
            "3A", "3B", "3C")
hi<-data.frame(theme,sums,total) 
hi$frequency<-hi$sums/hi$total

ggplot(data = hi, aes(x=theme, y=frequency, fill=theme))+
  geom_bar(stat = "identity")+
  theme_bw()+
  xlab("Theme")+
  ggtitle("Frequency of Tweets by Theme")+
  geom_hline(yintercept = 0)