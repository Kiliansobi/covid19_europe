#Daten Einlesen
data <- update.data()
#Daten Übersetzen
Daten <- translate.DE(data)
#Daten Aufbereiten
transform.data(Daten)

##Daten anzeigen
View(data)
View(Daten)
##Bilden von Todesfall/Tag Tabelle (Beispiele)
deathcases.per.day <- daily.death.cases(Daten, end = "2020-12-30", relative = FALSE)

##Bilden von Fallzahl/Tag Tabelle (Beispiele)
cases.per.day <- daily.cases(Daten, end = "2020-12-30", relative = FALSE)
View(cases.per.day)
bayern.day.cases <- ggplot(data = cases.per.day, mapping = aes(x=Datum, y=Bayern))
bayern.day.cases + geom_line(colour="red")

belgien.day.cases <- ggplot(data = cases.per.day, mapping = aes(x=Datum, y=Belgien))
belgien.day.cases + geom_line(colour="blue")

##Bilden von Inzidenz Tabelle (Beispiele)
incidence30 <- daily.incedence(Daten, end = "2020-11-14", intervall = 30, relative = FALSE)
View(incidence30)

incidence7 <- daily.incedence(Daten, end = "2020-12-30", intervall = 7, relative = FALSE)
View(incidence7)
incidence7proEinwohner <- daily.incedence(Daten, end = "2020-12-30", intervall = 7, relative = TRUE)
View(incidence7proEinwohner)



--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###Vorpräsentation 

##Fälle nach Alter

#Anpassen der Daten

#Bayern

#Bayern Altesgruppe
bayern.age.and.sex <- (Daten$bavaria$lgl$bavaria_lgl_age)
#data correction
bayern.age.and.sex$weiblich <- replace(bayern.age.and.sex$weiblich, c(1:9), bayern.age.and.sex$weiblich*1000)
bayern.age.and.sex$maennlich <- replace(bayern.age.and.sex$maennlich, c(1:9), bayern.age.and.sex$maennlich*1000)
bayern.age.and.sex$unbekannt <- replace(bayern.age.and.sex$unbekannt, c(12), bayern.age.and.sex$unbekannt*1000)
bayern.age.and.sex %>% mutate_if(is.numeric, ~round(.))
bayern.age.and.sex <- head(bayern.age.and.sex,-1)

#add the lost row with data date21.12.2020
bayern.age.and.sex <- bayern.age.and.sex %>% add_row(Altersgruppe="0 bis 9", weiblich=7415, maennlich=8021, unbekannt=364, .before = 1)
bayer.age.casessum <- rowSums( bayern.age.and.sex[,2:4] )

bayern.sex.and.age <- melt(bayern.age.and.sex, id.vars = "Altersgruppe", variable.name = "sex", value.name = "cases")
bayern.sex.and.age
ggplot(bayern.sex.and.age,aes(Altersgruppe, cases, fill=sex))+geom_bar(stat="identity",position = "dodge")



Bayern.age <- data.frame(Altersgruppe = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99","Aelter als 100", "unbekannt", "test"),
                         Faelle = bayer.age.casessum)

#Belgien
belgien.age.sex <- (Daten$belgium$belgium_age.sex)

countingCasesBel <- function(dataset) {
  agegroup <- c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+")
  groupCountingResult <- rep(0, 10)
  counter <- c(1:10)
  for (x in counter) {
    groupCountingResult[x] <- sum((subset(dataset, Altersgruppe == agegroup[x]))$bel.Fälle)
  }
  return(groupCountingResult)
}

resultBelAge <- countingCasesBel(belgien.age.sex)

b.a.na <- subset(belgien.age.sex, is.na(Altersgruppe))
b.a.10 <- nrow(b.a.na)
resultBelAge <- append(resultBelAge, b.a.10)
Belgien.age <- data.frame(Altersgruppe = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+", "unbekannt"),
                          resultBelAge)

#Tschechien
Czech.all.cases <- (Daten$czech$czech_6)

#create age groups
countingCasesforAgeGroup <- function(dataset) {
  agegroup <- c(0:8)
  groupCountingResult <- rep(0, 9)
  for (x in agegroup) {
    groupCountingResult[x+1] <- nrow(subset(Czech.all.cases, Alter >= x*10 & Alter < (x+1)*10))
  }
  return(groupCountingResult)
}

Czech.age.group <- countingCasesforAgeGroup(Czech.all.cases)
czech.aelter100 <- nrow(Czech.all.cases) - sum(Czech.age.group)
#add the 90+ group to the groups vector
Czech.age.group <- append(Czech.age.group, czech.aelter100)

Czech.age <- data.frame(Altersgruppe = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+"),
                        Faelle = Czech.age.group)

#Schweden
Schweden.age <- (Daten$sweden$sweden_6)

#Fälle nach Alter Bayern
alter.balken.bayern <-ggplot(Bayern.age,aes(Altersgruppe, Faelle))+
  geom_bar(stat="identity",position = "dodge")+
  ggtitle("Bayern")+
  ylab("Faelle")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="25",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "20"),
        axis.title.y = element_text(colour = "black",size = "20"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5))


#Fälle nach Alter Belgien
alter.balken.belgien <- ggplot(Belgien.age,aes(Altersgruppe, resultBelAge))+
  geom_bar(stat="identity",position = "dodge")+
  ggtitle("Belgien")+
  ylab("Faelle")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="25",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "20"),
        axis.title.y = element_text(colour = "black",size = "20"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5))

#Fälle nach Alter Tschechien
alter.balken.tschechien <- ggplot(Czech.age,aes(Altersgruppe, Faelle))+
  geom_bar(stat="identity",position = "dodge")+
  ggtitle("Tschechien")+
  ylab("Faelle")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="25",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "20"),
        axis.title.y = element_text(colour = "black",size = "20"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5))


#Fälle nach Alter Schweden
alter.balken.schweden <- ggplot(Schweden.age,aes(Altersgruppe, Fälle))+
  geom_bar(stat="identity",position = "dodge")+
  ggtitle("Schweden")+
  ylab("Faelle")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="25",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "20"),
        axis.title.y = element_text(colour = "black",size = "20"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5))

  
#Plotten Balkendiagramme Fälle nach Alter
alter.balken.bayern
alter.balken.belgien
alter.balken.tschechien
alter.balken.schweden
  
##Todesbalken Fälle pro Tag Vorpräsentation

#Anpassen der Daten
Data01<-(Daten$bavaria$rki$bavaria_rki)  #Bayern  
sub <- subset(Data01,Bundesland=="Bayern")
Data2 <- (Daten$belgium$belgium_mortality)#Belgien
Data3 <- (Daten$czech$czech_9)#Tschechien
Data04 <- read.csv("data/sweden/Schweden02.csv") #Schweden


#Todesfall in Bayern pro Tag
todes.balken.bayern <- ggplot(data = sub,aes(as.Date(Meldedatum),AnzahlTodesfall))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="25",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "20"),
        axis.title.y = element_text(colour = "black",size = "20"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))+
  scale_y_continuous(breaks=c(0,50,100,150,200,250,300,350,400,450,500),expand = c(0,0))+
  xlab("Datum")+ylab("Todesfaelle pro Tag")+
  ggtitle("Bayern")+
  scale_x_date(date_labels = "%B")+
  geom_hline(yintercept = 50, linetype="dashed", color = "black")+
  geom_hline(yintercept = 150, linetype = "dashed", color = "black")+
  theme(plot.title = element_text(hjust = 0.5))

#Todesfall in Tschechien pro Tag
todes.balken.tschechien <- ggplot(data = Data3,aes(x=as.Date(Datum)))+
  geom_bar(stat="count",fill="red")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="25",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "20"),
        axis.title.y = element_text(colour = "black",size = "20"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))+
  scale_y_continuous(breaks=c(0,50,100,150,200,250,300,350,400,450,500),expand = c(0,0))+
  xlab("Datum")+
  ylab("Todesfaelle pro Tag")+
  ggtitle("Tschechien")+
  scale_x_date(date_labels = "%B (%Y)")+
  geom_hline(yintercept = 50, linetype="dashed", color = "black")+
  geom_hline(yintercept = 150, linetype = "dashed", color = "black")+
  theme(plot.title = element_text(hjust = 0.5))

#Todesfall in Belgium pro Tag
todes.balken.belgien <- ggplot(data = Data2,aes(as.Date(Datum),Todesfälle))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="25",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "20"),
        axis.title.y = element_text(colour = "black",size = "20"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))+
  scale_y_continuous(breaks=c(0,50,100,150,200,250,300,350,400,450,500),expand = c(0,0))+
  xlab("Datum")+ylab("Todesfaelle pro Tag")+
  ggtitle("Belgien")+
  scale_x_date(date_labels = "%B (%Y)")+
  geom_hline(yintercept = 50, linetype="dashed", color = "black")+
  geom_hline(yintercept = 150, linetype = "dashed", color = "black")+
  theme(plot.title = element_text(hjust = 0.5))

#Todesfall in Schweden pro Tag
todes.balken.schweden <- ggplot(data = Data04,aes(as.Date(Datum),Todesfall))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="25",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "20"),
        axis.title.y = element_text(colour = "black",size = "20"),
        axis.text.y = element_text(angle=0, hjust=1,vjust=1, size=10,colour = "black"))+
  scale_y_continuous(breaks=c(0,20,40,60,80,100,120),expand = c(0,0))+
  xlab("Date")+
  ylab("Todesfaelle pro Tag")+
  ggtitle("Schweden")+
  scale_x_date(date_labels = "%B (%Y)")+
  geom_hline(yintercept = 50, linetype="dashed", color = "black")+
  geom_hline(yintercept = 150, linetype = "dashed", color = "black")+
  theme(plot.title = element_text(hjust = 0.5))

#Plotten der Todesbalken

todes.balken.bayern
todes.balken.belgien
todes.balken.tschechien
todes.balken.schweden

##Balkendiagramm Fälle pro Tag Vorpräsentation 

#Normieren der Titel- und Labelschriftgrößen plus Datum in Worten
  #scale_x_date(date_labels = "%B (%Y)")+
  #theme(axis.title = element_text(size = 25))+
  #theme(plot.title = element_text(size = 40))

#Anpassen der Daten 
cases.per.day.belgium <- select(cases.per.day, -Bayern, -Schweden, -Tschechien)
cases.per.day.bayern <- select(cases.per.day, -Belgien, -Schweden, -Tschechien) 
cases.per.day.schweden <- select(cases.per.day, -Bayern, -Belgien, -Tschechien) 
cases.per.day.tschechien <- select(cases.per.day, -Bayern, -Schweden, -Belgien) 

#Belgien
balken.belgien <- ggplot(cases.per.day.belgium, aes(Datum, Belgien))+
  geom_col(fill = "steelblue")+
  ggtitle("Belgien")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Faelle pro Tag")+
  geom_hline(yintercept=1000, linetype="dashed", color = "red")+
  geom_hline(yintercept = 5000, linetype = "dashed", color = "red")+
  theme(axis.title = element_text(size = 25))+
  theme(plot.title = element_text(size = 40))+
  scale_x_date(date_labels = "%B (%Y)") 

#Bayern
balken.bayern  <- ggplot(cases.per.day.bayern, aes(Datum, Bayern))+
  geom_col(fill = "steelblue")+
  ggtitle("Bayern")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Faelle pro Tag")+
  geom_hline(yintercept=1000, linetype="dashed", color = "red")+
  geom_hline(yintercept = 5000, linetype = "dashed", color = "red")+
  scale_x_date(date_labels = "%B (%Y)")+
  theme(axis.title = element_text(size = 25))+
  theme(plot.title = element_text(size = 40))

#Tschechien
balken.tschechien <- ggplot(cases.per.day.tschechien, aes(Datum, Tschechien))+
  geom_col(fill = "steelblue")+
  ggtitle("Tschechien")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Faelle pro Tag")+
  geom_hline(yintercept=1000, linetype="dashed", color = "red")+
  geom_hline(yintercept = 5000, linetype = "dashed", color = "red")+
  scale_x_date(date_labels = "%B (%Y)")+
  theme(axis.title = element_text(size = 25))+
  theme(plot.title = element_text(size = 40))

#Schweden
balken.schweden <- ggplot(cases.per.day.schweden, aes(Datum, Schweden))+
  geom_col(fill = "steelblue")+
  ggtitle("Schweden")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Faelle pro Tag")+
  geom_hline(yintercept=1000, linetype="dashed", color = "red")+
  geom_hline(yintercept = 5000, linetype = "dashed", color = "red")+
  scale_x_date(date_labels = "%B (%Y)")+
  theme(axis.title = element_text(size = 25))+
  theme(plot.title = element_text(size = 40))

#Plotten der Balkendiagramme
balken.bayern
balken.schweden
balken.tschechien
balken.belgien 

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

###Komplikationen innerhalb 

##Visualisierung Fälle Bayern
  
#Verkleinern des Datensatzes zur besseren Dartstellung
cases.in.bayern <- subset(cases.per.day.bayern, Datum >= "2020-10-01") #Fälle Bayern nach 1. Okt
cases.in.sweden <- subset(cases.per.day.schweden, Datum >= "2020-10-01") #Fälle Schweden nach 1. Okt
cases.in.czech <- subset(cases.per.day.tschechien, Datum >= "2020-10-01") #Fälle Tschechien nach 1. Okt
cases.in.belgien <- subset(cases.per.day.belgium, Datum >= "2020-10-01") #Fälle Belgien nach 1. Okt

#Durchschnittliche Fallzahlen + Maximum ab 01. Oktober 2020 der jeweiligen Länder

#MW
mw.in.bay <- mean(cases.in.bayern$Bayern) #MW Bayern 
mw.in.bel <- mean(cases.in.belgien$Belgien) #MW Belgien
mw.in.swe <- mean(cases.in.sweden$Schweden) #MW Schweden
mw.in.cze <- mean(cases.in.czech$Tschechien) #MW Tschechein

#MAX
max.in.bay <- max(cases.in.bayern$Bayern) #MW Bayern 
max.in.bel <- max(cases.in.belgien$Belgien) #MW Belgien
max.in.swe <- max(cases.in.sweden$Schweden) #MW Schweden
max.in.cze <- max(cases.in.czech$Tschechien) #MW Tschechein

#Visualisierung der Kennzahlen mittels matrix absolut
mat_4land.in.abs <- matrix(
  c(mw.in.bay, max.in.bay, mw.in.bel, max.in.bel, mw.in.swe, max.in.swe, mw.in.cze, max.in.cze),
  nrow=2, ncol=4,
  dimnames = list(c("Arithmetisches Mittel", "Maximum"), c("Bayern", "Belgien", "Schweden", "Tschechien"))
)

knitr::kable(mat_4land.in.abs)

#visualisierung der Kennzahlen mittels Matrix relativ
mat_4land.in.rel <- matrix(
  c(mw.in.bay/131,25, max.in.bay/131,25, mw.in.bel/114,31, max.in.bel/114,31, mw.in.swe/103,28, max.in.swe/103,28, mw.in.cze/106,38, max.in.cze/106,38),
  nrow=2, ncol=4,
  dimnames = list(c("Arithmetisches Mittel", "Maximum"), c("Bayern", "Belgien", "Schweden", "Tschechien"))
)

knitr::kable(mat_4land.in.rel)
#Exportieren der Matrizen

write.csv(mat_4land.in.abs, "Matrix_4Länder.in.abs.csv")
write.csv(mat_4land.in.rel, "Matrix_4Länder.in.rel.csv")

#Balken Bayern Fälle pro Tag ab 1. Oktober 2020
balken.bayern.in  <- ggplot(cases.in.bayern, aes(Datum, Bayern))+
  geom_col(fill = "steelblue")+
  ggtitle("Bayern, Fälle ab 1.Oktober 2020")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Faelle pro Tag")+
  geom_hline(yintercept=1000, linetype="dashed", color = "red")+
  geom_hline(yintercept = 5000, linetype = "dashed", color = "red")+
  scale_x_date(date_labels = "%B (%Y)")+
  theme(axis.title = element_text(size = 20))+
  theme(plot.title = element_text(size = 15))

balken.bayern.in

#Durchschnitt + Max Wochentage Bayern ab 1. Oktober

#Anpassen der Daten

bay.mo <- cases.in.bayern$Bayern[seq(5, nrow(cases.in.bayern), 7)] #Montag
bay.di <- cases.in.bayern$Bayern[seq(6, nrow(cases.in.bayern), 7)] #Dienstag
bay.mi <- cases.in.bayern$Bayern[seq(7, nrow(cases.in.bayern), 7)] #Mittwoch
bay.do <- cases.in.bayern$Bayern[seq(1, nrow(cases.in.bayern), 7)] #Donnerstag
bay.fr <- cases.in.bayern$Bayern[seq(2, nrow(cases.in.bayern), 7)] #Freitag
bay.sa <- cases.in.bayern$Bayern[seq(3, nrow(cases.in.bayern), 7)] #Samtag
bay.so <- cases.in.bayern$Bayern[seq(4, nrow(cases.in.bayern), 7)] #Sonntag

#arithm. Mittel 
mw.mo <- mean(bay.mo) #MW mo
mw.di <- mean(bay.di) #MW di
mw.mi <- mean(bay.mi) #MW mi
mw.do <- mean(bay.do) #MW do
mw.fr <- mean(bay.fr) #MW fr
mw.sa <- mean(bay.sa) #MW sa
mw.so <- mean(bay.so) #MW so

#Maximum 
max.mo <- max(bay.mo) #MW mo
max.di <- max(bay.di) #MW di
max.mi <- max(bay.mi) #MW mi
max.do <- max(bay.do) #MW do
max.fr <- max(bay.fr) #MW fr
max.sa <- max(bay.sa) #MW sa
max.so <- max(bay.so) #MW so

#Anteil an Gesamtfällen ab 1. Okt. 

#Anpassen der Daten
ges.bay <- sum(cases.in.bayern$Bayern) #Gesamtfälle in Bayern ab 1.Okt
ges.mo <- sum(bay.mo) #Gesamtfälle in Bayern ab 1.Okt / Mo
ges.di <- sum(bay.di) #Gesamtfälle in Bayern ab 1.Okt / Di
ges.mi <- sum(bay.mi) #Gesamtfälle in Bayern ab 1.Okt / Mi
ges.do <- sum(bay.do) #Gesamtfälle in Bayern ab 1.Okt / Do
ges.fr <- sum(bay.fr) #Gesamtfälle in Bayern ab 1.Okt / Fr
ges.sa <- sum(bay.sa) #Gesamtfälle in Bayern ab 1.Okt / Sa
ges.so <- sum(bay.so) #Gesamtfälle in Bayern ab 1.Okt / So

#Berechnen der Anteile
ant.mo <- label_percent()(ges.mo/ges.bay) #Anteil Mo
ant.di <- label_percent()(ges.di/ges.bay) #Anteil Di
ant.mi <- label_percent()(ges.mi/ges.bay) #Anteil Mi
ant.do <- label_percent()(ges.do/ges.bay) #Anteil Do
ant.fr <- label_percent()(ges.fr/ges.bay) #Anteil Fr
ant.sa <- label_percent()(ges.sa/ges.bay) #Anteil Sa
ant.so <- label_percent()(ges.so/ges.bay) #Anteil So

#Darstellung der Kennwerte durch Matrix

mat_bay <- matrix(
  c(mw.mo, max.mo, ant.mo, mw.di, max.di, ant.di, mw.mi, max.mi, ant.mi, mw.do, max.do, ant.do, mw.fr, max.fr, ant.fr, mw.sa, max.sa, ant.sa, mw.so, max.so, ant.so),
  nrow=3, ncol=7,
  dimnames = list(c("Arithmetisches Mittel", "Maximum", "Anteil an Gesamtfällen"), c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))
)

knitr::kable(mat_bay)

#Exportieren der Matrix

write.csv(mat_bay, "Matrix_Bayern.csv")

#Sonstige Ausreißer Vergleich
sonstige.aus <- cases.per.day[c(310,354,357,365)]

#7-Tage Inzidenz 
cases.inzidenz.bayern <- select(incidence7, -Belgien, -Schweden, -Tschechien)
cases.inzidenz.bayern.in <- cases.in.bayern <- subset(cases.inzidenz.bayern, Datum >= "2020-10-01") #Fälle Bayern nach 1. Okt (7 Tage Inzidenz)

#Visualisierung 7 Tage Inzidenz
inzidenz.bayern.in <- ggplot(data = cases.inzidenz.bayern.in, show.legend=TRUE)+
  ggtitle("7-Tage-Inzidenz in Bayern ab 01. Oktober 2020")+
  xlab("Datum")+
  ylab("Faelle")+
  geom_line(aes(x=Datum,y=Bayern),colour="orange",show.legend =TRUE)+
  scale_x_date(date_labels = "%B (%Y)")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
          plot.title = element_text(size="25",lineheight =.6,face = "bold",hjust = 0.5 ,colour = "black"),
          axis.title.x = element_text(colour = "black",size = "20"),
          axis.title.y = element_text(colour = "black",size = "20"),
          axis.text.y = element_text(angle=0, hjust=1,vjust=1, size=10,colour = "black"))

inzidenz.bayern.in

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### Vergleich Fallzahlen und Todeszahlen

##Vergleich Fallzahlen

#Infektion pro Tag fourland --> Komplikationen innerhalb

casesfourland <- ggplot(show.legend=TRUE) +
  geom_line(data = cases.per.day, mapping = aes(x=Datum, y=Bayern), colour="red", size = 0.5,show.legend = TRUE) +
  geom_line(data = cases.per.day, mapping = aes(x=Datum, y=Belgien), colour="blue", size = 0.5,show.legend = TRUE)+
  geom_line(data = cases.per.day, mapping = aes(x=Datum, y=Schweden), colour="yellow", size = 0.5,show.legend = TRUE)+
  geom_line(data = cases.per.day, mapping = aes(x=Datum, y=Tschechien), colour="green", size = 0.5,show.legend = TRUE)+
  ggtitle("Fälle pro Tag in allen vier Ländern")+
  scale_x_date(date_labels = "%B (%Y)")+
  xlab("Datum")+ 
  ylab("Faelle pro Tag")+
  geom_hline(aes(yintercept=5000),colour="red",linetype="dashed")+
  geom_hline(aes(yintercept=1000),colour="red",linetype="dashed")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="25",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "20"),
        axis.title.y = element_text(colour = "black",size = "20"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5))

casesfourland

cases.per.day.relative100000 <- daily.cases(Daten, end = "2020-11-14", relative = TRUE)
View(cases.per.day.relative100000)

cases.per.day.relative1 <- daily.cases(Daten, end = "2020-11-14", relative = TRUE, reverenz = 1)
View(cases.per.day.relative1)

#Fälle pro Woche fourland

incidencefourland <- ggplot(data = incidence7, show.legend=TRUE)+
  ggtitle("Faelle pro Woche")+
  xlab("Meldewoche")+
  ylab("Faelle")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="25",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "20"),
        axis.title.y = element_text(colour = "black",size = "20"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))+
  geom_line(aes(x=Datum,y=Bayern),colour="red",show.legend =TRUE, size = 1.3)+
  geom_line(aes(x=Datum,y=Belgien),colour="blue",show.legend =TRUE, size = 1.3)+
  geom_line(aes(x=Datum,y=Schweden),colour="yellow",show.legend =TRUE, size = 1.3)+
  geom_line(aes(x=Datum,y=Tschechien),colour="green",show.legend =TRUE, size = 1.3)+
  scale_x_date(date_labels = "%B (%Y)")+
  scale_y_continuous(breaks=c(10000,20000,30000,40000,50000,60000,70000,80000,90000,100000,110000,120000))+
  scale_colour_discrete(name="Land",
                        breaks=c("Bayern","Belgien","Schweden","Tschechien"),
                        labels=c("Bayern","Belgien","Schweden","Tschechien"))+
  geom_hline(aes(yintercept=10000),colour="red",linetype="dashed")+
  geom_hline(aes(yintercept=75000),colour="red",linetype="dashed")+
  theme(plot.title = element_text(hjust = 0.5))

incidencefourland

#7-Tage-Inzidenz pro 100.000 EW fourlands

incidencefourland2 <- ggplot(data = incidence7proEinwohner, show.legend=TRUE)+
  ggtitle("7-Tage-Inzidenz pro 100.000 Einwohner")+
  xlab("Meldewoche")+
  ylab("Faelle pro 100.000 Einwohner")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = "black"),
        plot.title = element_text(size="25",lineheight =.6,face = "bold",colour = "black"),
        axis.title.x = element_text(colour = "black",size = "20"),
        axis.title.y = element_text(colour = "black",size = "20"),
        axis.text.y = element_text(angle=0, vjust=0.5, size=10,colour = "black"))+
  geom_line(aes(x=Datum,y=Bayern),colour="red",show.legend =TRUE, size = 1.3)+
  geom_line(aes(x=Datum,y=Belgien),colour="blue",show.legend =TRUE, size = 1.3)+
  geom_line(aes(x=Datum,y=Schweden),colour="yellow",show.legend =TRUE, size = 1.3)+
  geom_line(aes(x=Datum,y=Tschechien),colour="green",show.legend =TRUE, size = 1.3)+
  scale_x_date(date_labels = "%B (%Y)")+
  scale_y_continuous(breaks=c(100,200,300,400,500,600,700,800,900,1000))+
  scale_colour_discrete(name="Land",
                        breaks=c("Bayern","Belgien","Schweden","Tschechien"),
                        labels=c("Bayern","Belgien","Schweden","Tschechien"))+
  geom_hline(aes(yintercept=100),colour="red",linetype="dashed")+
  geom_hline(aes(yintercept=500),colour="red",linetype="dashed")+
  theme(plot.title = element_text(hjust = 0.5))



incidencefourland2

incidence3 <- daily.incedence(Daten, end = "2020-11-14", intervall = 3, relative = FALSE)
View(incidence3)

incidence7abs <- daily.incedence(Daten, end = "2020-11-14", intervall = 7, relative = TRUE)
View(incidence7abs)