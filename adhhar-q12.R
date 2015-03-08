#########################################################
##Relationship between Age and Sex 
## Use ggplot
## Group staes based on AGE and SEX
####
####
library(ggplot2)
require(ggplot2)
uad<-read.csv("UAD20150124.csv",header=TRUE,stringsAsFactors=FALSE)
## SUbset which contains only F and M (removing NA Character)
uad1NoNA<-subset(uad,uad$Gender=="M" | uad$Gender=="F")
## Make the frequencies numbers (rather than factors)
uad1NoNA$Gender<-as.factor(uad1NoNA$Gender)
uad1NoNA$State<-as.factor(uad1NoNA$State)

counts<-as.numeric(table(uad1NoNA$Gender))

st<-table(as.factor(uad1NoNA$State))
grp<-with(uad1NoNA,table(Gender,State))
print(grp)

ymx <- c(0,(max(counts)/2))

jpeg("state_gen.jpg",width=1600,height=800)
bp<-barplot(grp, beside=TRUE, ylim=ymx,col=c("blue","red"),legend=TRUE,las=2)
## Add text at the top of the each BAR
## Add x-axis labels 
title(main="AADHAR Enrollment Gender Ratio in Each State - December 2014")
dev.off()

#####
#####
###### Children <- 0-12
###### Teens <- 12-18
####   Youth <-  18-40
###    Retired<- 40-60
###    sr.citize<- >60



for (i in 1:nrow(uad1NoNA)) {
  if (uad1NoNA$Age[i] <= "18") {
    uad1NoNA$Gen_Group[i]<-"Grp-18"
  }
  if ( (uad1NoNA$Age[i] > "18") & (uad1NoNA$Age[i] <= "30") ) {
    uad1NoNA$Gen_Group[i]<-"Grp-18-30"
  }
  if ( (uad1NoNA$Age[i] > "30") &  (uad1NoNA$Age[i] <= "45") ) {
    uad1NoNA$Gen_Group[i]<-"Grp-30-45"
  }
  if ( (uad1NoNA$Age[i] > "45") &  (uad1NoNA$Age[i] <= "65") ) {
    uad1NoNA$Gen_Group[i]<-"Grp-45-65"
  }
  if (uad1NoNA$Age[i] > "65") {
    uad1NoNA$Gen_Group[i]<-"Grp->65"
  }
  
}

