require (foreign)
sociodemo <- read.dbf("C:\\Users\\SALA-C24\\Downloads\\SDEMT215.dbf")

table (sociodemo$CD_A)
table (sociodemo$CLASE1)

sociodemo$R_DEF1 <- as.numeric(as.character(sociodemo$R_DEF))
sociodemo$C_RES1 <- as.numeric(as.character(sociodemo$C_RES))
sociodemo$EDA1 <- as.numeric(as.character(sociodemo$EDA))

View(sociodemo)

precod <- subset(sociodemo,((R_DEF1 ==0) & (C_RES1 == 1  | C_RES1 == 3) & (EDA1 >= 15 & EDA1 <=98)),select = c(EDA1, SEX, HRSOCUP, CLASE2, CLASE1, CLASE3))

table (precod$R_DEF1)
table (precod$C_RES1)
table (precod$EDA1)

attach(precod)
CLASE2V1 <- table(CLASE2)
CLASE2V1
CLASE2V1

hist(CLASE2,)
hist(CLASE2, main= "Grafica 1. Distribucion de la Poblacion de 15 anios o mas, 2015",
     xlab="Tipo de Ocupada", ylab="Poblacion (miles)",
     xlim= c(1,4), ylim= c(0,200000), 
     border=T, pch=18, col= "deepskyblue4")

##Ejemplo de plot

CLASE2V <- as.numeric(as.character(CLASE2V1))

sd1 <- subset(sociodemo, CLASE2 == 1, select = c(EDA, SEX, HRSOCUP, CLASE2, CLASE1, CLASE3, EDA5C, IMSSISSSTE))
frec <- table(sd1$IMSSISSSTE)
frec

##GRafico pie

pie(frec)
##LABELS SE PONEN COMO VECTOR
pie(frec, labels=c("IMSS", "ISSSTE", "OTRA", "NO RECIBE", "NO ESPECIFICADO"))
##MAIN PARA TITULO
pie(frec, labels=c("IMSS", "ISSSTE", "OTRA", "NO RECIBE", "NO ESPECIFICADO"), main = "INSTITUCION DE ATENCION MEDICA")
##COL PARA PONER COLOR A CADA SECTOR
pie(frec, labels=c("IMSS", "ISSSTE", "OTRA", "NO RECIBE", "NO ESPECIFICADO"), 
    main = "INSTITUCION DE ATENCION MEDICA",
    col=c("green","red","blue","yellow","purple"))

#graficaS DE BARRAS
#sacamos las frecuencias de las variables a utilizar

frec1 <- table(sd1$EDA5C)
#VIEW FREC1

barplot(frec1)
#ponemos titulo
barplot(frec1,main = "CINCO GRUPOS DE EDAD")
#col similar al pie
barplot(frec1,main = "CINCO GRUPOS DE EDAD",
        col = c("green","red","blue","yellow","purple"))
#etiquetas se pueden poner con names.arg
barplot(frec1,main = "CINCO GRUPOS DE EDAD",
        col = c("green","red","blue","yellow","purple"),
        names.arg = c("MENOR", "14 A 24", "25 A 44", "45 A 64", "65 Y MAS", "N.E")
)
#poner limite al eje y
barplot(frec1,main = "CINCO GRUPOS DE EDAD",
        col = c("green","red","blue","yellow","purple"),
        names.arg = c("MENOR", "14 A 24", "25 A 44", "45 A 64", "65 Y MAS", "N.E"),
        ylim = c(0,90000)
)

names(frec1) <- c("MENOR", "14 A 24", "25 A 44", "45 A 64", "65 Y MAS", "N.E")

barplot (frec1,main = "CINCO GRUPOS DE EDAD",
         col = c("green","red","blue","yellow","purple"))
#etiquietas para los ejes
barplot (frec1,main = "CINCO GRUPOS DE EDAD",
         col = c("green","red","blue","yellow","purple"),
         xlab = "EDAD", ylab = "POBLACION",
         ylim=c(0,150000))