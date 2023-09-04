#Establecer el area de trabajo

setwd("G:/clases/UCC/2023_ucc_estad/Semana 13 agos")
library(readxl)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(glmm.hp)

al <- read_excel("finc.xlsx")
names(al)
View(al)

attach(al)
al1<-data.frame(FI1, FI2)

# un p < 0.05 indica que se acepta la asunción que existen diferencias o sea que la variables no son homg?neas

g1<-ggplot(al1, aes(FI2)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g2<-ggplot(al1, aes(log(FI2)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g3<-ggplot(al1, aes(sqrt(FI2)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

ggarrange(g1, g2, g3, ncol = 2, nrow = 2)


#podemos observar que los datos no cumplen normalidad

t.test(log(FI1), log(FI2),alternative="two.sided",var.equal=F) 

d2=al1$No
d3=al1$Si

boxplot(d2,d3,
        main = "HPG",
        names = c("No", "Si"),
        las = 1,
        col = c("orange","red"),
        border = "brown",
        Vertical = TRUE,
        notch = FALSE)
