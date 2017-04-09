load("simulation_result.RData")


print(paste0("Subsetting and wrtin xlsx START Time ",Sys.Date()," ",Sys.time()))

EANrp2014<- subset(f,(f$`Mortality.Table`=="RP2014" & f$`Cost.Method`=='EAN'));
EANrp2010<- subset(f,(f$`Mortality.Table`=="RP2010" & f$`Cost.Method`=='EAN'));
EANrp2000<- subset(f,(f$`Mortality.Table`=="RP2000" & f$`Cost.Method`=='EAN'));
PUCrp2014<- subset(f,(f$`Mortality.Table`=="RP2014" & f$`Cost.Method`=='PUC'));
PUCrp2010<- subset(f,(f$`Mortality.Table`=="RP2010" & f$`Cost.Method`=='PUC'));
PUCrp2000<- subset(f,(f$`Mortality.Table`=="RP2000" & f$`Cost.Method`=='PUC'));

write.csv(EANrp2014, "EANrp2014full.csv", row.names=FALSE)
write.csv(EANrp2010, "EANrp2010full.csv", row.names=FALSE)
write.csv(EANrp2000, "EANrp2000full.csv", row.names=FALSE)
write.csv(PUCrp2014, "PUCrp2014full.csv", row.names=FALSE)
write.csv(PUCrp2010, "PUCrp2010full.csv", row.names=FALSE)
write.csv(PUCrp2000, "PUCrp2000full.csv", row.names=FALSE)

write.csv(rbind(EANrp2014,EANrp2010,EANrp2000,PUCrp2014,PUCrp2010,PUCrp2000),"Full.csv", row.names=FALSE)

print(paste0("Subsetting and wrtin xlsx END Time ",Sys.Date()," ",Sys.time()))

