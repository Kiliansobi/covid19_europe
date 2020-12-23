#Daten Einlesen
data <- update.data()
#Daten Übersetzen
Daten <- translate.DE(data)
#Daten Aufbereiten
transform.data(Daten)

##Daten anzeigen
View(datan)
View(Daten)

##Bilden von Fallzahl/Tag Tabelle (Beispiele)
cases.per.day <- daily.cases(Daten, end = "2020-11-14", relative = FALSE)
View(cases.per.day)

cases.per.day.relative100000 <- daily.cases(Daten, end = "2020-11-14", relative = TRUE)
View(cases.per.day.relative100000)

cases.per.day.relative1 <- daily.cases(Daten, end = "2020-11-14", relative = TRUE, reverenz = 1)
View(cases.per.day.relative1)

##Bilden von Inzidenz Tabelle (Beispiele)
incedence30 <- daily.incedence(Daten, end = "2020-11-14", intervall = 30, relative = FALSE)
View(incedence30)

incedence7 <- daily.incedence(Daten, end = "2020-11-14", intervall = 7, relative = FALSE)
View(incedence7)

incedence3 <- daily.incedence(Daten, end = "2020-11-14", intervall = 3, relative = FALSE)
View(incedence3)

incedence7abs <- daily.incedence(Daten, end = "2020-11-14", intervall = 7, relative = TRUE)
View(incedence7abs)

#Balkendiagramm Fälle pro Tag Vorpräsentation 
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
  ylab("Fälle pro Tag")+
  geom_hline(yintercept=1000, linetype="dashed", color = "red")+
  geom_hline(yintercept = 5000, linetype = "dashed", color = "red")                    

#Bayern
balken.bayern  <- ggplot(cases.per.day.bayern, aes(Datum, Bayern))+
  geom_col(fill = "steelblue")+
  ggtitle("Bayern")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Fälle pro Tag")+
  geom_hline(yintercept=1000, linetype="dashed", color = "red")+
  geom_hline(yintercept = 5000, linetype = "dashed", color = "red")                  

#Tschechien
balken.tschechien <- ggplot(cases.per.day.tschechien, aes(Datum, Tschechien))+
  geom_col(fill = "steelblue")+
  ggtitle("Tschechien")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Fälle pro Tag")+
  geom_hline(yintercept=1000, linetype="dashed", color = "red")+
  geom_hline(yintercept = 5000, linetype = "dashed", color = "red")        

#Schweden
balken.schweden <- ggplot(cases.per.day.schweden, aes(Datum, Schweden))+
  geom_col(fill = "steelblue")+
  ggtitle("Schweden")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Fälle pro Tag")+
  geom_hline(yintercept=1000, linetype="dashed", color = "red")+
  geom_hline(yintercept = 5000, linetype = "dashed", color = "red")
