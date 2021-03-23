


#===========================================================================
                     #R-Code zu den Tabellen und Grafiken
#===========================================================================






#================Die Grafik zur Investitionen der DKB 2016==================#

library(ggplot2)
#Investitionen im Jahre 2016
Investition<- c(19.6, 2.5, 9.0,12.0,8.1, 5.9, 3.5)
#Branchen
names(Investition) <- c("Wohnen", "Gesundheit und Pflege", "
Kommunen,Bildung und Forschung", "Private Haushalt",
                        "Umwelttechnik", "Energie und Versorgung",
                        "Landwirtschaft und Ernährung")

Branchen<-names(Investition)
#Data-frame aus den Investitionen und den zugehörigen Branchen
Data<-data.frame(Branchen=Branchen,Investition= as.numeric(Investition))

#Barplot zu den Investitionen
par(ps = 16)
ggplot(data=Data, aes(x=Branchen, y=Investition),col = "red") +
  geom_bar(stat="identity",fill="dodgerblue3")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text( hjust = 1, size = 12,face="bold"),
        axis.title.x = element_text( size=16, face="bold"),
        axis.title.y = element_text( size=16, face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=15))+ 
  ylab("Investition in Mrd.") + xlab("Branchen")



#=========Datenimport und Datenbereinigung==========#

# Der Input- und Zielordner festlegen
setwd("N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\BA_DATEI\\R_Doku_Access")

#Paket "RODBC" Laden
library(RODBC)

#Die Verbindung mit der ODBC-Datenquelle herstellen
con <- odbcConnect("FLGD")
tbls <- sqlTables(con, tableType = "TABLE")
tbls$TABLE_NAME

#Einzelne Tabelle der Access-Datenbank in R einlesen
FLGD.list<- sqlFetch(con, "FLGD_Liste_FLGD")
FLGD.Segm <- sqlFetch(con, "FLGD_Liste_Segm")
FLGD.Segm <- FLGD.Segm[4:length(FLGD.Segm)]
FLGD.QS <- sqlFetch(con, "FLGD_Liste_QS" )
FLGD.QS <- FLGD.QS[4:length(FLGD.QS)]

#Alle Tabellen kombinieren
Data <- data.frame(FLGD.list, FLGD.Segm, FLGD.QS)
#Alle Datensätze, wo Qualitaetsmangel gibt, loeschen
Data <-Data[Data$KD_QS=="",]

#Die zur Modellierung benötigten Spalten festlegen
IMMO_NomW <- Data$IMMO_NomW_eng + Data$IMMO_NomW_weit
Bars_NomW <- Data$BARS_NomW_eng + Data$BARS_NomW_weit
EVTF_NomW <- Data$EVTF_NomW_eng + Data$EVTF_NomW_weit
Data_NEW <- data.frame(Data[c(2:3, 10:13, 17:18)], IMMO_NomW, Data[21:22],
                       Bars_NomW, Data[25:26], EVTF_NomW, Data[5])

#Löschen alle Datensätze, wo leere Felder gibt
Data_NEW <-Data_NEW[complete.cases(Data_NEW),]

#Herkunftsländer in 3 Bereichen segmentieren
Euro_Land <- c("Österreich", "Schweiz", "Frankreich", 
               "Niederlande", "Dänemark", "Luxemburg", "Schweden", "Finnland", "Norwegen")
Data_NEW$KD_Herkunft <- as.character(Data_NEW$KD_Herkunft)
Data_NEW$KD_Herkunft[Data_NEW$KD_Herkunft %in% Euro_Land] <- "EU_Ausland"
Data_NEW$KD_Herkunft[Data_NEW$KD_Herkunft == "Inland"] <- "Deutschland"
Rest <- c("EU_Ausland", "Deutschland")
Data_NEW$KD_Herkunft[!(Data_NEW$KD_Herkunft %in% Rest)] <- "Sonstiges"
Data_NEW$KD_Herkunft <- as.factor(Data_NEW$KD_Herkunft)
str(Data_NEW)#Eigenschaften der Spalten

#Bereinigte Datei als .rda speichern
save(Data_NEW, file = "FLGD_NEW.rda")



#=========Grafische Darstellung des CART- und Bagging-Modell==========#

#Daten Hochladen
load("C:/Users/Himel/OneDrive/Studium/Bachelor (Wirtschaftsmathematik - B Sc.)/Extra/Finanz_EXTRA/Zeitreihenanalyse/An Introduction to Analysis of Financial Data with R/TXTDATEI/mieteRed.Rdata")

library(randomForest)
library(rpart)
library(sfsmisc)

#Wohnfläche pro m^2
wflUnique <- sort(unique(miete$wfl))
#Nettomiete pro m^2
means <- sapply(wflUnique,function(i)
{ mean(miete$nmqm[miete$wfl==i])})
df <- data.frame(means, wflUnique)

#nicht-lineares Modell
m <- nls(means ~ I(a*(1/wflUnique)+c), data = df,
         start = list(c = 0, a = 1), trace = T)

#CART-Modell
mm <- rpart(df$means~df$wflUnique, cp = 0, minsplit = 3)
#Bagging-Modell
bag <- randomForest(df$means~df$wflUnique, mtry = 1, ntree = 100, nodesize =3)

#Prognose der Trainingsdaten (Bagging-Modell)
bag_new <-as.numeric(predict(bag, df))
bag_new <- bag_new[1: length(bag_new)-1]
length(bag_new)

#Prognose der Trainingsdaten (CART-Modell)
l <-as.numeric(predict(mm, df))
l <- l[1: length(l)-1]
k <- as.numeric(df$means)
k<- k[1: length(k)-1]
par(mfrow = c(1,2), ps = 18, mar=c(5.1, 5.1, 4.1, 2.1))
s <- seq(from = 0, to = 200, by = 0.01)

#Grafik zur CART-Modell und nlM
plot(wflUnique,means, pch=20, cex = 2, col = "red", type = "p",
     xlab=expression("Wohnfläche["*m^2*"]"),
     ylab=expression("Nettomiete pro "*m^2*"[Euro]"))
plotStep(wflUnique, l, cad.lag = FALSE,  ylim = c(0,20), xlim = c(0,200), main = "", add = TRUE, col = "black")
lines(s, predict(m, list(wflUnique = s)), col = "green", lwd = 2)
legend("topright",legend  = c( "CART", "nlm"),
       col = c("black", "green"), lty = 1 , lwd = 2, cex =1, ncol = 2)

#Grafik zur Bagging-Modellund nlM
plot(wflUnique,means, pch=20, cex = 2, col = "red",xlab=expression("Wohnfläche["*m^2*"]"),
     ylab=expression("Nettomiete pro "*m^2*"[Euro]"))
plotStep(wflUnique, , cad.lag = FALSE,  ylim = c(0,20), xlim = c(0,200),
         xlab=expression("Wohnfläche["*m^2*"]"),
         ylab=expression("Nettomiete pro "*m^2*"[Euro]"), main = "", col = "blue", add = TRUE)
lines(s, predict(m, list(wflUnique = s)), col = "green", lwd = 2)
legend("topright",legend  = c("Bagging", "nlm"),col = c("blue", "green"), lwd = 2, cex =1, lty = 1, ncol = 2)



#============Zusammenfassung der Datenmenge=============#

#Pfade angeben
setwd("N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\FLGD")
#Daten Hochladen
load("FLGD_NEW.rda")

#Benögtigte Merkmale festlegen
Data_NEW <- Data_NEW[c(1:8,16)]
Data_NEW <-Data_NEW[Data_NEW$KD_Produkt !="Sonstiges",]

#Zuammenfassung der Datenmenge
x <-summary(Data_NEW$KD_Produkt)
x1 <-summary(Data_NEW$KD_Typ)
x3 <- summary(Data_NEW$KD_Herkunft)

#Zuammenfassung der Datenmenge als Tabelle speichern
stargazer(x, type = "text",
          out = "N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafiken\\Tebellen\\KD_Produkt.txt", summary = FALSE)
stargazer(x1, type = "text",
          out =  "N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafiken\\Tebellen\\KD_KD_Typ.txt", summary = FALSE, rownames = FALSE)
stargazer(x3, type = "text",
          out =  "N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafiken\\Tebellen\\KD_Herkunft.txt", summary = FALSE, rownames = FALSE)
stargazer(x, type = "text",
          out = "N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafiken\\Tebellen\\KD_Produkt.tex", summary = FALSE)
stargazer(x1, type = "latex",
          out =  "N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafiken\\Tebellen\\KD_KD_Typ.tex", summary = FALSE, rownames = FALSE)
stargazer(x3, type = "latex",
          out =  "N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafiken\\Tebellen\\KD_Herkunft.tex", summary = FALSE, rownames = FALSE)



#==================Test und Trainingsmenge=======================#

#Pfade angeben
setwd("N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\FLGD")
load("FLGD_NEW.rda")
Data_NEW <- Data_NEW[c(1:8,16)]

# Werden das Portfolio aus der Kundenprodukte Baufi. und Darlehen 
#zur Modellierung der LGD verwendet, so wird KD_Produkt
#ungleich "Sonstiges" eingesetzt.
Data_NEW <-Data_NEW[Data_NEW$KD_Produkt !="Sonstiges",]
#Data_NEW <-Data_NEW[Data_NEW$KD_Produkt =="Sonstiges",]
#Trainings- und Testmenge
set.seed(42)
rownames(Data_NEW)<-1:nrow(Data_NEW)
rows <- sample(x=1:nrow(Data_NEW),size=0.5 *  nrow(Data_NEW))
train <- Data_NEW[rows,]
test <-Data_NEW[! rownames(Data_NEW) %in% rows,]


#==========LGD vs Kundenherkunft für jedes Kundenprodukt===========#

#Notwendige Pakete Laden
library(ggplot2)
library(gridExtra)
#Pfade angeben

#Boxplot mit Trainingsdaten: LGD vs Kundenherkunft für jedes Kundenprodukt
png("N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\Grafiken\\boxplot.png", height = 300, width = 700)
ggplot(train)+
  geom_boxplot(aes(y= KD_FLGD,x=KD_Herkunft,fill=KD_Herkunft),outlier.size=0.1,notch=FALSE,notchwidth=0.8)+
  facet_grid(~KD_Produkt,margins=T)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.title.x = element_text( size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=15))+
  ylab(label='LGD')+
  xlab('Kundenherkunft')+ ylim(c(-0.8,2.2))+
  ggtitle('LGD vs Kundenherkunft für jedes Kundenprodukt') +  
  theme(plot.title = element_text(hjust = 0.5, color = "darkred", size = 16,face="bold")) 
dev.off()


#=========LGD-Verteilung nach Kundenherkunft und Kundenprodukt=========#

#Notwendige Pakete Laden
library(ggplot2)
library(gridExtra)

#LGD - Verteilung für jedes Kundenprodukt
png("N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\Grafiken\\LGD.png", height = 300, width = 700)
p2=ggplot(train)+
  geom_density(aes(x=KD_FLGD, fill=KD_Produkt),alpha=0.2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.title.x = element_text( size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=15))+
  xlab('LGD')+
  ylab('Dichte')+
  ggtitle('LGD - Verteilung für jedes Kundenprodukt')+ xlim(c(-0.5, 1.5))+
  theme(plot.title = element_text(hjust = 0.5, color = "darkred", size = 16,face="bold"))
p2
dev.off()

#LGD - Verteilung nach Kundenherkunft
png("N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\Grafiken\\LGD_Nach_Herkunft.png", height = 300, width = 700)
#Density plot: LGD
p3=ggplot(train)+
  geom_density(aes(x=KD_FLGD, fill=KD_Herkunft),alpha=0.2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.title.x = element_text( size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=15))+
  xlab('LGD')+
  ylab('Dichte')+
  ggtitle('LGD - Verteilung nach Kundenherkunft')+ xlim(c(-0.5, 1.5))+
  theme(plot.title = element_text(hjust = 0.5, color = "darkred", size = 16,face="bold"))
p3
dev.off()


#===================Complixity-Parameter Tabelle===================#

#Modell des Entscheidungsbäumes
mod <- rpart(train$KD_FLGD~., data = train,
             control = rpart.control(xval = 10, minsplit  = 50, cp = 0))

#Complixity-Parameter Tabelle
table_mod <- tail(mod$cptable, n = 6)
stargazer(table_mod, type = "latex", out = "N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafiken\\Tebellen\\table_cp_tex.tex", summary = FALSE, rownames = FALSE)




#================Anzahl der Splits gegen Kreuzvalidierungsfehler=================#

#Modell des Entscheidungsbäumes
mod <- rpart(train$KD_FLGD~., data = train,
             control = rpart.control(xval = 10, minsplit  = 50, cp = 0))

# nsplits vs Kreuzvalidierungsfehler
png("N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafikenSonst\\Grafiken\\nsplitvsXerror.png", height = 350, width =700)
par(mfrow=c(1,1), lwd=2, font.axis=2, ps=16)
plot(mod$cptable[,c(2,4)], type = "l", col = "red", xlab = "Anzahl der Splits", ylab = "Kreuzvalidierungsfehler")
lines(c(17,17), c(0.80,1.3), lty = 2, col = "blue")
text(70,1, "n = 17", col = "blue")
dev.off()


#===================Optimal zurückgeschnittener Subbaum===================#

#CART-Modell
mod <- rpart(train$KD_FLGD~., data = train,
             control = rpart.control(xval = 10, minsplit  = 50, cp = 0))

#Optimal zurückgeschnittener Subbaum:
#browseURL("http://www.statmethods.net/advstats/cart.html")
pruning <- prune(mod, cp = mod$cptable[which.min(mod$cptable[,"xerror"]), "CP"])
png("N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafiken\\Grafiken\\CART.png", height = 550, width =1200)
par(ps = 16)
rpart.plot(pruning,cex = 0.9)
dev.off()



#====Bar-Plot zur Variable-Wichtigkeit des Pruning-Verfahren=====#

#CART-Modell
mod <- rpart(train$KD_FLGD~., data = train,
             control = rpart.control(xval = 10, minsplit  = 50, cp = 0))

#Optimal zurückgeschnittener Baum
pruning <- prune(mod, cp = mod$cptable[which.min(mod$cptable[,"xerror"]), "CP"])
imp<-data.frame(vars=names(pruning$variable.importance),imp=as.numeric(pruning$variable.importance)/sum(pruning$variable.importance))
png("N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafiken\\Grafiken\\Varimp_Pruning.png", height = 300, width =700)
p<-ggplot(data=imp, aes(x=reorder(vars, imp), y=imp),col = "red") +
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text( hjust = 1, size = 12,face="bold"),
        axis.title.x = element_text( size=16, face="bold"),
        axis.title.y = element_text( size=16, face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=15))+ 
  ylab("Importance") + xlab("Variable")+
  theme(plot.title = element_text(hjust = 0.5, color = "darkred", 
                                  size = 16,face="bold"))

# Horizontaler Bar-plot
p + coord_flip()
dev.off()


#====Anzahl der Bäume gegen Testfehler (Bagging-, RF-Modell)=====#

#Modell für Randomforest
set.seed(421)
model_random1<-randomForest(train$KD_FLGD~., data = train, 
                            ntree = 200, importance = TRUE,  mtry = 1, xtest = test[1:8], ytest = test$KD_FLGD)
set.seed(423)
model_random2<-randomForest(train$KD_FLGD~., data = train, 
                            ntree = 200, importance = TRUE,  mtry = 2, xtest = test[1:8], ytest = test$KD_FLGD)
set.seed(422)
model_random<-randomForest(train$KD_FLGD~., data = train, 
                           ntree = 200, importance = TRUE,  mtry = 3, xtest = test[1:8], ytest = test$KD_FLGD)
set.seed(425)
model_random4<-randomForest(train$KD_FLGD~., data = train, 
                            ntree = 200, importance = TRUE,  mtry = 4, xtest = test[1:8], ytest = test$KD_FLGD)
set.seed(426)
model_random5<-randomForest(train$KD_FLGD~., data = train, 
                            ntree = 200, importance = TRUE,  mtry = 5, xtest = test[1:8], ytest = test$KD_FLGD)
set.seed(427)
model_random6<-randomForest(train$KD_FLGD~., data = train, 
                            ntree = 200, importance = TRUE,  mtry = 6, xtest = test[1:8], ytest = test$KD_FLGD)
set.seed(428)
model_random7<-randomForest(train$KD_FLGD~., data = train, 
                            ntree = 200, importance = TRUE,  mtry = 7, xtest = test[1:8], ytest = test$KD_FLGD)
model0 <- model_random7

#Anzahl der Bäume gegen MSE-Fehler
png("N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafikenSonst\\Grafiken\\Fehlerplot.PNG", height = 400, width =700)
par(mfrow=c(1,2), lwd=2, font.axis=2, ps=16)
plot(1:200, as.numeric(model_random1$test[[2]]), type = "l", ylim = c(0.174,0.202), ylab = "Testfehler", xlab = "Anzahl der Bäume")
lines(1:200, as.numeric(model_random2$test[[2]]), col = "red")
lines(1:200, as.numeric(model_random$test[[2]]), col = "blue")
lines(1:200, as.numeric(model_random4$test[[2]]), col = "gold")
lines(1:200, as.numeric(model_random5$test[[2]]), col = "brown")
lines(1:200, as.numeric(model_random6$test[[2]]), col = "pink")
lines(1:200, as.numeric(model0$test[[2]]), col = "cyan")
legend("topright",  c("p = 1", "p = 2", "p = 3", "p = 4",
                      "p = 5", "p = 6", "p = 7"), ncol = 2, 
       col = c("black", "red", "blue", "gold", "brown", "pink", "cyan"), lty = 1)

#Anzahl der Bäume gegen OOB-Fehler
plot(1:200, as.numeric(model_random1$mse), type = "l", ylim = c(0.166,0.197), ylab = "OOB-Fehler", xlab = "Anzahl der Bäume")
lines(1:200, as.numeric(model_random2$mse), col = "red")
lines(1:200, as.numeric(model_random$mse), col = "blue")
lines(1:200, as.numeric(model_random4$mse), col = "gold")
lines(1:200, as.numeric(model_random5$mse), col = "brown")
lines(1:200, as.numeric(model_random6$mse), col = "pink")
lines(1:200, as.numeric(model0$mse), col = "cyan")
legend("topright",  c("p = 1", "p = 2", "p = 3", "p = 4",
                      "p = 5", "p = 6", "p = 7"), ncol = 2, 
       col = c("black", "red", "blue", "gold", "brown", "pink", "cyan"), lty = 1)
dev.off()


#=======Variable-Wichtigkeitsplot (Bagging-, RF-Modell)=======#

#Bagging-modell
set.seed(148)
model0<-randomForest(train$KD_FLGD~., data = train, ntree = 200, mtry = 8)

#Variable-importance-plot (Bagging-Modell)
x <- importance(model0)
vars<-dimnames(x)[[1]];vars
imp<-data.frame(vars=vars,imp=as.numeric(x[,1])/sum(as.numeric(x[,1])));imp
imp<-imp[order(imp$imp,decreasing=T),];imp
png("N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafiken\\Grafiken\\Var_imp_bag.png", height = 350, width =700)
p<-ggplot(data=imp, aes(x=reorder(vars, imp), y=imp),col = "red") +
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text( hjust = 1, size = 12,face="bold"),
        axis.title.x = element_text( size=16, face="bold"),
        axis.title.y = element_text( size=16, face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=15))+ 
  ylab("Importance") + xlab("Variable")+
  theme(plot.title = element_text(hjust = 0.5, color = "darkred", 
                                  size = 16,face="bold"))
p2 <-p + coord_flip()

#Modell für Randomforest
set.seed(422)
model_random<-randomForest(train$KD_FLGD~., data = train, 
                           ntree = 200, importance = TRUE,  mtry = 3)

#Variable-importance-plot (RF-Modell)
x <- importance(model_random)
vars<-dimnames(x)[[1]];vars
imp<-data.frame(vars=vars,imp=as.numeric(x[,1])/sum(as.numeric(x[,1])));imp
imp<-imp[order(imp$imp,decreasing=T),];imp
png("N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafiken\\Grafiken\\Var_imp_random.png", height = 350, width =700)
p<-ggplot(data=imp, aes(x=reorder(vars, imp), y=imp),col = "red") +
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text( hjust = 1, size = 12,face="bold"),
        axis.title.x = element_text( size=16, face="bold"),
        axis.title.y = element_text( size=16, face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=15))+ 
  ylab("Importance") + xlab("Variable")+
  theme(plot.title = element_text(hjust = 0.5, color = "darkred", 
                                  size = 16,face="bold"))
p1 <-p + coord_flip()

# Horizontaler Bar-Plot zur Variable-Wichtigkeit (Bagging-, RF-Modell)
png("N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafiken\\Grafiken\\Var_Imp_Gesamt.png", height = 300, width =700)
#Code zur Multiplotfunktion siehe: 
#browseURL("http://www.cookbook-r.com/Multiple-graphs_on_one_page_(ggplot2)/")
multiplot(p2,p1, cols = 2)
dev.off()


#======================Tabelle der Fehlermaße========================#

#Modell des CART-Verfahrens
mod <- rpart(train$KD_FLGD~., data = train,
             control = rpart.control(xval = 10, minsplit  = 50, cp = 0))
#Bagging-modell
set.seed(148)
model0<-randomForest(train$KD_FLGD~., data = train, ntree = 200, mtry = 7)
#Model für Randomforest
set.seed(422)
model_random<-randomForest(train$KD_FLGD~., data = train, 
                           ntree = 200, importance = TRUE,  mtry = 3)

#Prognose der Trainings- und Testdaten
Prognose_pruning.training <- predict(object = pruning, newdata  = train)
Prognose_pruning.test <- predict(object = pruning, newdata  = test)
prognose_bag.training<-predict(object=model0,newdata=train)
prognose_bag.test<-predict(object=model0,newdata=test) 
prognose_random.test<-predict(object=model_random,newdata=test)
prognose_random.training<-predict(object=model_random,newdata=train)
#-----------------------------------------------------------------------
#MAE
n <- length(train$KD_FLGD)
MAE_Pruning_train <- sum(abs(train$KD_FLGD - Prognose_pruning.training))/n
MAE_bag_train <- sum(abs(train$KD_FLGD - prognose_bag.training))/n
MAE_random_train <- sum(abs(train$KD_FLGD - prognose_random.training))/n
#RMSE
RMSE_Pruning_train <- sqrt(sum((train$KD_FLGD - Prognose_pruning.training)^2)/n)
RMSE_bag_train <- sqrt(sum((train$KD_FLGD - prognose_bag.training)^2)/n)
RMSE_random_train<- sqrt(sum((train$KD_FLGD - prognose_random.training)^2)/n)
#TIC
TIC_Pruning_train <- RMSE_Pruning_train/(sqrt((sum(train$KD_FLGD^2)/n)) + sqrt((sum(Prognose_pruning.training^2)/n)))
TIC_bag_train <- RMSE_bag_train/(sqrt((sum(train$KD_FLGD^2)/n)) + sqrt((sum(prognose_bag.training^2)/n)))
TIC_random_train <- RMSE_random_train/(sqrt((sum(train$KD_FLGD^2)/n)) + sqrt((sum(prognose_random.training^2)/n)))

#MAE_Test
real <-  test$KD_FLGD
n <- length(test$KD_FLGD)
MAE_Pruning_test <- sum(abs(real - Prognose_pruning.test))/n
MAE_bag_test <- sum(abs(real - prognose_bag.test))/n
MAE_random_test<- sum(abs(real - prognose_random.test))/n
#RMSE_Test
RMSE_Pruning_test <- sqrt(sum((real - Prognose_pruning.test)^2)/n)
RMSE_bag_test <- sqrt(sum((real - prognose_bag.test)^2)/n)
RMSE_random_test<- sqrt(sum((real - prognose_random.test)^2)/n)
#TIC_Test
TIC_Pruning_test <- RMSE_Pruning_test/(sqrt((sum(real^2)/n)) + sqrt((sum(Prognose_pruning.test^2)/n)))
TIC_bag_test<- RMSE_bag_test/(sqrt((sum(real^2)/n)) + sqrt((sum(prognose_bag.test^2)/n)))
TIC_random_test <- RMSE_random_test/(sqrt((sum(real^2)/n)) + sqrt((sum(prognose_random.test^2)/n)))
#Janus
Janus_Pruning <- RMSE_Pruning_test/RMSE_Pruning_train
Janus_bag <- RMSE_bag_test/RMSE_bag_train
Janus_random <- RMSE_random_test/RMSE_random_train

#Data-Frame erstellen
MAE.Training <- c(MAE_Pruning_train, MAE_bag_train, MAE_random_train)
RMSE.Training <- c(RMSE_Pruning_train, RMSE_bag_train, RMSE_random_train)
TIC.Training <- c(TIC_Pruning_train, TIC_bag_train, TIC_random_train)
MAE.Test <- c(MAE_Pruning_test, MAE_bag_test, MAE_random_test)
RMSE.Test <- c(RMSE_Pruning_test, RMSE_bag_test, RMSE_random_test)
TIC.Test <- c(TIC_Pruning_test, TIC_bag_test, TIC_random_test)
Janus <- c(Janus_Pruning, Janus_bag, Janus_random)
Methode <- c("Pruning", "Bagging", "RF")
Tab <- data.frame(Methode, round(MAE.Training,3), round(RMSE.Training,3), round(TIC.Training,3)
                  , round(MAE.Test,3), round(RMSE.Test,3), round(TIC.Test,3), round(Janus,3))
names(Tab) <- c("Methode", "MAE", "RMSE", "TIC", "MAE", "RMSE", "TIC", "Janus")

#Speichern als Tabelle
stargazer(Tab, type = "latex",
          out =  "N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafiken\\Tebellen\\Result.tex", summary = FALSE, rownames = FALSE)



#==Grafische Darstellung der Prognose gegen Realisierung ==#
# Multiple-plot funktion für ggplot
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#-----------------------------------------------------------------

#Modell des CART-Verfahrens
mod <- rpart(train$KD_FLGD~., data = train,
             control = rpart.control(xval = 10, minsplit  = 50, cp = 0))
#Bagging-modell
set.seed(148)
model0<-randomForest(train$KD_FLGD~., data = train, ntree = 200, mtry = 7)
#Model für Randomforest
set.seed(422)
model_random<-randomForest(train$KD_FLGD~., data = train, 
                           ntree = 200, importance = TRUE,  mtry = 3)

#Prognose der Trainings- und Testdaten
Prognose_pruning.training <- predict(object = pruning, newdata  = train)
Prognose_pruning.test <- predict(object = pruning, newdata  = test)
prognose_bag.training<-predict(object=model0,newdata=train)
prognose_bag.test<-predict(object=model0,newdata=test) 
prognose_random.test<-predict(object=model_random,newdata=test)
prognose_random.training<-predict(object=model_random,newdata=train)

#-----------------------------------------------------------------------
#Prognose vs Realisiierung (Pruning)
#Die auskommenrierten R-Objekten werden verwendet, wenn eine Grafik der Prognose vs Realisiierung
#für die Testdaten erstellt werden muss.
data <- data.frame(x= train$KD_FLGD ,y =  Prognose_pruning.training) #Training
#data <- data.frame(x= test$KD_FLGD ,y =  Prognose_pruning.test) #Test
a0 <-ggplot(data)+ xlab('Realisierung') + ylab('Prognose')+ xlim(c(-0.25,1.25)) + ylim(c(-0.25,1.25))+
  geom_point(aes(x=x,y=y),alpha=0.7, lwd = 1.5, color = "blue")+ 
  geom_abline(intercept = 0, slope = 1, color = 'red', lwd = 0.75)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x = element_text( size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=15))
#------------------------------------------------------------------------------
#Mittelwert plot (Prognose vs Realisierung: Pruning)
#Die auskommenrierten R-Objekten werden verwendet, wenn eine Grafik der Prognose vs Realisiierung
#für die Testdaten erstellt werden muss.
actual1 <- train$KD_FLGD #Training
#actual1 <- test$KD_FLGD #Test
pred1 <-Prognose_pruning.training #Training
#pred1 <-Prognose_pruning.test    #Test
Prog_actual1 <- data.frame(pred1, actual1)
schneiden <- c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.75) #train Pruning (Baufi. und Darlehen)
#schneiden <- c(0, 0.3, 0.35, 0.4, 0.45, 0.5,0.55, 0.60, 0.65, 0.75) #test Pruning (Baufi. und Darlehen)
#schneiden <- c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.75) #train Pruning (Sonstige Kundenpridukte)
#schneiden <- c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.75) #train Pruning (Sonstige Kundenpridukte)
S1 <- cut(Prog_actual1$pred1, breaks = schneiden)
Prog_actual.new1 <- data.frame(pred1, actual1, S1)
Zusammenfassung1 <-Prog_actual.new1 %>%
  group_by(S1) %>%
  summarise(mean.pred1 = mean(pred1), mean.actual1 = mean(actual1), n = n())

data1 <- data.frame(Zusammenfassung1)
stargazer(data1, type = "latex",
          out = "N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafiken\\Tebellen\\Zusammenfassung_random.tex", summary = FALSE)


b0 <- ggplot(data1)+ xlab('Realisierung') + ylab('Prognose')+
  geom_point(aes(x=Zusammenfassung1$mean.actual1,y=Zusammenfassung1$mean.pred1),alpha=0.7, lwd = 4, color = "darkgreen")+ 
  xlim(c(0,0.8)) + ylim(c(0,0.8))+
  geom_abline(intercept = 0, slope = 1, color = 'red', lwd = 0.75)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x = element_text( size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=15))
#------------------------------------------------------------------------------------


#Prognose vs Realisiierung (Bagging)
#Die auskommenrierten R-Objekten werden verwendet, wenn eine Grafik der Prognose vs Realisiierung
#für die Testdaten erstellt werden muss.
actual<-train$KD_FLGD #Training
#actual<-train$KD_FLGD #Test
result<-data.frame(actual=actual,predicted=prognose_bag.training) #Training
#result<-data.frame(actual=actual,predicted=prognose_bag.test)     #Test
a <- ggplot(result)+
  geom_point(aes(x=actual,y=predicted),alpha=0.7, color = "blue")+
  geom_abline(intercept = 0, 
              slope = 1, lwd = 1, col = 'red') + 
  xlim(c(-0.25,1.25)) + ylim(c(-0.25,1.25))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.title.x = element_text( size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=15))+
  ylab(label='Prognose')+
  xlab('Realisierung')+ 
  theme(plot.title = element_text(hjust = 0.5, color = "darkred", size = 16,face="bold"))
#--------------------------------------------------------------------------
#Mittelwert plot (Prognose vs Realisierung: Bagging)
#Die auskommenrierten R-Objekten werden verwendet, wenn eine Grafik der Prognose vs Realisiierung
#für die Testdaten erstellt werden muss.
actual0 <- train$KD_FLGD #Training
actual0 <- test$KD_FLGD  #Test
pred0 <-prognose_bag.training #Training
pred0 <-prognose_bag.test     #Test
Prog_actual0 <- data.frame(pred0, actual0)
schneiden0 <- c(-0.2,0,0.05,0.1, 0.15, 0.2, 0.25,
                0.3, 0.35, 0.4, 0.45, 0.5,0.55, 0.6,0.65, 0.7, 1.5) #Train Bagging (Baufi. und Darlehen)
# schneiden0 <- c(-0.2,0,0.05,0.1, 0.15, 0.2, 0.25,
# 0.3, 0.35, 0.4, 0.45, 0.5,0.55, 0.6,0.65, 0.7, 1.5) #Test Bagging (Baufi. und Darlehen)
#schneiden0 <- c(-1.1, 0.1, 0.15, 0.2, 0.25,
#0.3, 0.35, 0.4, 0.45, 0.5,0.55, 0.6,0.65, 0.7, 0.75, 0.8, 0.85, 2.65) #Train Bagging (Sonstige Produkte)
#schneiden0 <- c(-1.1, 0.1, 0.15, 0.2, 0.25,
#0.3, 0.35, 0.4, 0.45, 0.5,0.55, 0.6,0.65, 0.7, 0.75, 0.8, 0.85, 2.65) #Test Bagging (Sonstige Produkte)

S0 <- cut(Prog_actual0$pred0, breaks = schneiden0 )
Prog_actual.new0 <- data.frame(pred0, actual0, S0)
Zusammenfassung0 <-Prog_actual.new0 %>%
  group_by(S0) %>%
  summarise(mean.pred0 = mean(pred0), mean.actual0 = mean(actual0), n = n())

data0 <- data.frame(Zusammenfassung0)
stargazer(data0, type = "latex",
          out = "N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafiken\\Tebellen\\Zusammenfassung_random.tex", summary = FALSE)


b <-ggplot(data0)+ xlab('Realisierung') + ylab('Prognose')+
  geom_point(aes(x=Zusammenfassung0$mean.actual0,y=Zusammenfassung0$mean.pred0),alpha=0.7, lwd = 4, color = "darkgreen")+ 
  xlim(c(-0.02,1)) + ylim(c(-0.01,1))+
  geom_abline(intercept = 0, slope = 1, color = 'red', lwd = 0.75)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x = element_text( size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=15))

#-------------------------------------------------------------------------------

#Prognose vs Realisiierung (RF)
#Die auskommenrierten R-Objekten werden verwendet, wenn eine Grafik der Prognose vs Realisiierung
#für die Testdaten erstellt werden muss.
actual<-train$KD_FLGD #Training
#actual <- test$KD_FLGD #Test
result<-data.frame(actual=actual,predicted=prognose_random.training)

#png("N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafiken\\Grafiken\\actual_VS_predict_random.png", height = 300, width =700)
d <-ggplot(result)+
  geom_point(aes(x=actual,y=predicted),alpha=0.7, color = "blue")+
  geom_abline(intercept = 0, 
              slope = 1, lwd = 1, col = 'red') + 
  xlim(c(-0.25,1.25)) + ylim(c(-0.25,1.25))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.title.x = element_text( size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=15))+
  ylab(label='Prognose')+
  xlab('Realisierung')+ 
  theme(plot.title = element_text(hjust = 0.5, color = "darkred", size = 16,face="bold"))

#-----------------------------------------------------------------------------

#Mittelwert plot (Prognose vs Realisierung: RF)
#Die auskommenrierten R-Objekten werden verwendet, wenn eine Grafik der Prognose vs Realisiierung
#für die Testdaten erstellt werden muss.
actual <- train$KD_FLGD
#actual <- test$KD_FLGD
pred <-prognose_random.training
#pred <-prognose_random.test
Prog_actual <- data.frame(pred, actual)
schneiden <- c(0, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.75) #train RF (Baufi. und Darlehen)
#schneiden <- c(-0.02, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.75) #test RF (Baufi und Darlehen)
#schneiden <- c(-0.02, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.75) #Train RF (Sonstige Produkte)
#schneiden <- c(-0.02, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.75) #test RF (Sonstige Produkte)
S <- cut(Prog_actual$pred, breaks = schneiden )
Prog_actual.new <- data.frame(pred, actual, S)
Zusammenfassung <-Prog_actual.new %>%
  group_by(S) %>%
  summarise(mean.pred = mean(pred), mean.actual = mean(actual), n = n())

data <- data.frame(Zusammenfassung)
stargazer(data, type = "latex",
          out = "N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafiken\\Tebellen\\Zusammenfassung_random.tex", summary = FALSE)

c <- ggplot(data)+ xlab('Realisierung') + ylab('Prognose')+
  geom_point(aes(x=Zusammenfassung$mean.actual,y=Zusammenfassung$mean.pred),alpha=0.7, lwd = 4, color = "darkgreen")+ 
  xlim(c(0,0.85)) + ylim(c(0,0.85))+
  geom_abline(intercept = 0, slope = 1, color = 'red', lwd = 0.75)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.x = element_text( size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=15))

png("N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafiken\\Grafiken_train\\actual_VS_predict_random_NEW.png", height = 700, width =700)
#Code zur Multiplotfunktion siehe: 
#browseURL("http://www.cookbook-r.com/Multiple-graphs_on_one_page_(ggplot2)/")
multiplot(a0,a,d, b0,b,c, cols = 2)
dev.off()


#==========Grafische Darstellung der Trennschärfe, Lorenzkurve und Gini-Koef===========#
#Die auskommenrierten R-Objekten werden verwendet, wenn eine Grafik der Trennschärfe
#für die Testdaten erstellt werden muss.
#Modell des CART-Verfahrens
mod <- rpart(train$KD_FLGD~., data = train,
             control = rpart.control(xval = 10, minsplit  = 50, cp = 0))
#Bagging-modell
set.seed(148)
model0<-randomForest(train$KD_FLGD~., data = train, ntree = 200, mtry = 7)
#Model für Randomforest
set.seed(422)
model_random<-randomForest(train$KD_FLGD~., data = train, 
                           ntree = 200, importance = TRUE,  mtry = 3)

#Prognose der Trainings- und Testdaten
Prognose_pruning.training <- predict(object = pruning, newdata  = train)
Prognose_pruning.test <- predict(object = pruning, newdata  = test)
prognose_bag.training<-predict(object=model0,newdata=train)
prognose_bag.test<-predict(object=model0,newdata=test) 
prognose_random.test<-predict(object=model_random,newdata=test)
prognose_random.training<-predict(object=model_random,newdata=train)

#-------------------------------------------------------------------------------

gini <- function(x, y) {                      # Funktion zum Gini-Index  
  area <- 0                                                 
  for (i in 2:n+1) area <- area + 0.5*((x[i]-x[i-1])*(y[i]+y[i-1]))
  gini <- 1 - 2*area; round(gini, 3) # Gini-Index                          
}
#------------------------------------------------------------------------------
#Trainingsdaten und Prognosen
actual0 <- train$KD_FLGD # realisierte training-LGD
actual <-sort(actual0) # Sortieren
pred0_random <- prognose_random.training # prognostizierte Training-LGD (RF)
pred0_prun <- Prognose_pruning.training  # prognostizierte Training-LGD (Pruning)
pred0_bag <- prognose_bag.training       # prognostizierte Training-LGD (Bagging)

#Trainingsdaten und Prognosen
#actual0 <- test$KD_FLGD # realisierte Test-LGD
#actual <-sort(actual0) # Sortieren
#pred0_random <- prognose_random.training # prognostizierte Test-LGD (RF)
#pred0_prun <- Prognose_pruning.training  # prognostizierte Test-LGD (Pruning)
#pred0_bag <- prognose_bag.training       # prognostizierte Test-LGD (Bagging)


#Data-Frame nach Prognose sortieren
real_prog0_prun <- data.frame(actual0, pred0_prun)
real_prog_prun <-real_prog0_prun[order(real_prog0_prun$pred0_prun),]
real_prog0_bag <- data.frame(actual0, pred0_bag)
real_prog_bag <-real_prog0_prun[order(real_prog0_bag$pred0_bag),]
real_prog0_random <- data.frame(actual0, pred0_random)
real_prog_random <-real_prog0_random[order(real_prog0_random$pred0),]

n <- length(actual)
a <- c(0, (1:n)/n)      # X-Achse - relativer Index
# Y-Achse - kumulierte rel. Anteile 
b <- c(0, (cumsum(actual) / sum(actual))) 
c<- c(0, (cumsum(real_prog_prun$actual0) / sum(real_prog_prun$actual0)))
d <- c(0, (cumsum(real_prog_bag$actual0) / sum(real_prog_bag$actual0)))
e <- c(0, (cumsum(real_prog_random$actual0) / sum(real_prog_random$actual0)))
#--------------------------------------------------------------------------------
#Plot der Trannschärfe, Gini-Koef und Lorenzkurve
png("N:\\FB_Rating\\090 MitarbeiterInnen\\Alom\\Anwenndung\\TabellenundGrafikenSonst\\Grafiken\\Lorenzkurve_Gemeinsam.png", height = 400, width =700)
par(mfrow=c(1,1), lwd=2, font.axis=2, bty="n", ps=16)
plot(a, b, type="l", cex=1.5, xlim=c(0,1), ylim=c(0,1), xlab="Prozentualer Anteil der Kreditnehmer", ylab="Prozentuale Anteil der LGD", col = "green")
abline(0,1, col="black", lty=2)
polygon(c(a,0), c(b,0), angle = -45, border=NA, density = 10, col = "green", lty = 1)

lines(a, c, type="l", col = "blue", lty = 2)
polygon(c(a,0), c(c,0), angle = 20, border=NA, density = 10, col = "blue", lty = 2)

lines(a, d, type="l", col = "red", lty = 2)
polygon(c(a,0), c(d,0), angle = 60, border=NA, density = 10, col = "red", lty = 2)

lines(a, e, type="l", col = "darkorange", lty = 4)
polygon(c(a,0), c(e,0), angle = -90, border=NA, density = 10, col = "darkorange", lty = 4)
legend("topleft",  c(paste("Realisierung     G =",round(gini(a,b), digits = 3) ),
                     paste("Pruning              T =",round(gini(a,c)/gini(a,b), digits = 3) ),
                     paste("Bagging            T =",round(gini(a,d)/gini(a,b), digits = 3)), 
                     paste("RF                      T =",round(gini(a,e)/gini(a,b), digits = 3))),
       col=c("green","blue","red", "darkorange" ), lty = c(1, 2,2, 4))
dev.off()