##Read the file
###Total Records 484404
uad<-read.csv("UAD20150124.csv",header=TRUE,stringsAsFactors=FALSE)
uad1<-uad

for (i in 1:nrow(uad1)){
  if (uad1$Gender[i] == "T"){
    uad1$Gender[i]<- "NA"
  }
}
## SUbset which contains only F and M (removing NA Character)
uad1NoNA<-subset(uad1,uad1$Gender=="M" | uad1$Gender=="F")
## Make the frequencies numbers (rather than factors)
counts<-as.numeric(table(uad1NoNA$Gender))
countsper<-c(counts[1]/sum(counts) * 100, counts[2]/sum(counts)*100)
countsper<-round(countsper,0)

####Step 2 of Analysis
stcounts<-as.numeric(table(as.factor(uad1NoNA$State)))
st<-table(as.factor(uad1NoNA$State))
ymx1<-c(0,max(stcounts)+20000)
jpeg("state.jpg",width=1600,height=800)
bpst<-barplot(table(uad1NoNA$State),ylim=ymx1, xaxt='n', xlab='',width=1.1, xlim=c(0,60), space=0.6,col=rainbow(40))
#bpst<-barplot(table(uad1NoNA$State),ylim=ymx1, xaxt='n', xlab='',main = "AADHAR Registration Statewise Enrollment in December 2014",width=1.1, xlim=c(0,60), space=0.6,col=rainbow(40))
text(x = bpst, y = stcounts,label = stcounts,pos=3,cex=1.0,col="red")
axis(1, at=bpst, labels=rownames(st), tick=FALSE, srt=45,las=2, line=-0.5, cex.axis = 1.0)
mtext(cex=3,side=3,"AADHAR Registration Statewise Enrollment in December 2014")
dev.off()
subset(st,st==max(st))
subset(st,st==min(st))

#####
#####
###### Children <- 0-12
###### Teens <- 12-18
####   Youth <-  18-40
###    Retired<- 40-60
###    sr.citize<- >60

tapply(uad1NoNA,uad1NoNA$Age,FUNC=if(uad1NoNA$Age <= "18"){uad1NoNA$Gen_Group<-"0-18"})


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

