# Chi square
obsfre <- matrix(c(212,673,202,123,118,167,178,528), nrow =2)
dimnames(obsfre)<-list(Survived= c("Yes", "No"),Passengers = c("Crew","1st","2nd","3rd"))
results<-chisq.test(obsfre)
results

obsfre <- matrix(c(128,88,199,33,186,66), nrow =2)
dimnames(obsfre)<-list(chosseagain= c("Yes", "No"),Hotel = c("Gpalm","PalmR","Palmprince"))
results<-chisq.test(obsfre)
results