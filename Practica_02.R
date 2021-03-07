
#Base de Datos "HairEyeColor"
data(HairEyeColor)
head(HairEyeColor)

#Datos que no contiene
HairEyeColor <- data.frame(HairEyeColor)
head(HairEyeColor$Hair)
head(HairEyeColor$Eye)
head(HairEyeColor$Sex)



#Tabla de frecuencias en consola
table(HairEyeColor)
HairEyeColor <- data.frame(HairEyeColor)


#Grafica de barras (Por cabello, color de ojos y Sexo)
hair <- aggregate(HairEyeColor$Freq, by=list(Category=HairEyeColor$Hair), FUN=sum)
barplot(hair[,2], width = 1, xlab = "Hair", ylab = "Cantidad", names.arg = hair[,1])


eyes <- aggregate(HairEyeColor$Freq, by=list(Category=HairEyeColor$Eye), FUN=sum)
barplot(eyes[,2], width = 1, xlab = "Eyes", ylab = "Cantidad", names.arg = eyes[,1])


sex <- aggregate(HairEyeColor$Freq, by=list(Category=HairEyeColor$Sex), FUN=sum)
barplot(sex[,2], width = 1, xlab = "Sex", ylab = "Cantidad", names.arg = sex[,1])

#Histograma 
hist(HairEyeColor$Freq,main="Histograma de Frecuencias",xlab="Numero de Frecuencias",ylab="Frecuencia",col="purple")

#Diagrama de dispersion
plot(x = HairEyeColor$Sex, y = HairEyeColor$Freq,main="Relacion entre Frecuencia y Sexo")
plot(x = HairEyeColor$Hair, y = HairEyeColor$Freq,main="Relacion entre Frecuencia y Color de Cabello")
plot(x = HairEyeColor$Eye, y = HairEyeColor$Freq,main="Relacion entre Frequencia y color de ojos")

#Diagrama de caja 
boxplot(x = HairEyeColor$Freq,main="Distribucion del numero de frecuencias de la base de datos")



#####################################################################################
#Otras Gradicas Relacionadas

library(ggplot2)
library(dplyr)

library(tidyr)
library(scales)
library(DT)

data<-tbl_df(HairEyeColor)

dim(data)

str(data)

summary(data)

qplot(data = data, Eye, Freq, geom="boxplot", color=Sex)

qplot(data = data, Hair, Freq, geom="boxplot", color=Sex)

B_M<-data %>% select(Hair, Sex, Freq) %>%filter(Sex=="Male" & Hair=="Brown") %>% summarise(Male_Brown=sum(Freq))

B_F<-data %>% select(Hair, Sex, Freq) %>%filter(Sex=="Female" & Hair=="Brown") %>% summarise(Female_Brown=sum(Freq))

TOT<-data %>% summarise(TotH=sum(Freq))

male_brown <-B_M/TOT*100

female_brown<- B_F/TOT*100

male_brown

female_brown

qplot(data=data, Hair, geom="density", fill=Hair, alpha=0.6)

qplot(data=data, Eye, geom="density", fill=Eye, alpha=0.6)

qplot(data=data, Sex, geom="density", fill=Sex, alpha=0.6)

#Bar chart of HairEyeColor dataset
library(reshape2)
mm = melt(HairEyeColor)
mm <- within(mm, {
  color <- tolower(Hair)
  color <- ifelse(color == 'blond', 'yellow', color)
  color1 <- tolower(Eye)
  color1 <- ifelse(color1 == 'hazel', 'gold', color1)
  value <- value / 2
  value1 <- value
})

mm <- melt(mm, id.vars = -(4:5))
cols <- c(apply(mm[1:16, c('color','color1')], 1, c))

library(ggplot2)
ggplot(data = mm, aes(x = interaction(Hair, Eye), y = value, fill = interaction(variable, interaction(Hair, Eye)))) +
  geom_bar(stat = 'identity') + facet_grid(Sex ~ .) + 
  theme(legend.position = 'none') + 
  scale_fill_manual(values = cols)

