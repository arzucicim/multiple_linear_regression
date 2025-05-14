data1<-nohut
names(data1)
names(data1)<-c("Meyve","Tane","Bitki","Verim")
attach(data1)
#normallik testi
qqnorm(Verim)
qqline(Verim)
shapiro.test(Verim)
#p=0,032 alpha=0,05 den k??c??k h0 red
lnverim<-log(Verim) #LN d??n??????m??
qqnorm(lnverim)
qqline(lnverim)
shapiro.test(lnverim)
#p value > alpha H0 reddedilemez. veriler normalle??ti

datayeni <- cbind(Meyve, Tane, Bitki, lnverim) 
pairs(datayeni)
datayeni
sonuc <- lm(lnverim~Meyve+Tane+Bitki)
summary(sonuc)
confint(sonuc, level = .95)
cor(datayeni)
