# 1: Packages Laden 
# (falls einige noch nicht installiert sind, jeweils install.packages("....."))

# Visualisierung:
library(ggplot2)
library(hrbrthemes) # install.packages("hrbrthemes")
library(extrafont)

# Laden des Datensatzes
library(readstata13)

# Datenrekodieren:
library(car)

a <- c(1,2,3)

# Daten als Objekt importieren
econ <- read.dta13(file="data/offline/econometrics.dta" , convert.factors=F)

econ <- econ [sample(c(1:nrow(econ)),size = 5000,replace = FALSE),]

# Variable Alter generieren
econ$alter = 2003 - econ$gebjahr
econ$alter [econ$alter == 2004] <- NA # entfernen der fehlerhaften

#Sex 

econ$sex <- factor(econ$sex,levels = c(1,2),labels = c("maennlich","weiblich"))

# Ueberstunden recodieren 0=nein, 1=ja
econ$over = recode(econ$tp72,"2=0;-2=NA;-1=NA; 3=NA")

# Vertragliche und tatsaechliche Wochenarbeitszeit
# Missings bereinigen
econ$contract = recode(econ$tp7001,"-3=NA;-2=NA;-1=NA") 
econ$actual = recode(econ$tp7003,"-3=NA;-2=NA;-1=NA")
econ$contract = econ$contract/10
econ$actual = econ$actual/10

# Vertrauen 
# "Trust in people" und "Can't rely on anybody" recodieren
econ$trust = recode(econ$tp0301,"-1=NA")
econ$rely = recode(econ$tp0302,"-1=NA")

econ$netinc = recode(econ$tp7602,"-3=NA;-2=NA;-1=NA")



# Nur relevante Variablen werden übernommen
econ_data <- econ[,c("netinc","alter","sex","contract","actual","trust","rely")]
#head(econ_data)

# Delete NAs
#econ_data <- na.omit(econ_data)

# Einkommen Kategorisiert in Quartile

#hist(econ$netinc)
#quantile(econ_data$netinc)

econ_data$inc_kat <- NA
econ_data$inc_kat [econ_data$netinc < quantile(econ_data$netinc,na.rm = T)[2]] <- "Q1"
econ_data$inc_kat [econ_data$netinc >= quantile(econ_data$netinc,na.rm = T)[2] & econ_data$netinc < quantile(econ_data$netinc,na.rm = T)[3]] <- "Q2"
econ_data$inc_kat [econ_data$netinc >= quantile(econ_data$netinc,na.rm = T)[3] & econ_data$netinc < quantile(econ_data$netinc,na.rm = T)[4]] <- "Q3"
econ_data$inc_kat [econ_data$netinc >= quantile(econ_data$netinc,na.rm = T)[4] ] <- "Q4"

#table(econ_data$inc_kat)
saveRDS(econ_data,"data/offline/econdata.rds")
save(list = c("econ_data"),file = "data/offline/econdata.rdata")
rm(econ)
rm(econ_data)

