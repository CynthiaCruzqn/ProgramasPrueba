#Base de Datos "HairEyeColor"
data(HairEyeColor)
head(HairEyeColor)


#Grafica de barras (Por cabello, color de ojos y Sexo)
data(HairEyeColor)
head(HairEyeColor)

male <- HairEyeColor[, ,"Male"]

# Subtabla para las mujeres
female <- HairEyeColor[, ,"Female"]
data <- as.table(male + female)
data

colnames(data) <- c("Marrón","Azul","Avellana", "Verdes")
rownames(data) <- c("Negro","Marrón","Pelirrojo","Rubio")

# otra forma de hacerlos es a través de dimnames
dimnames(data) <- list(
  Pelo <- c("Negro","Marrón","Pelirrojo","Rubio"),
  Ojos <- c("Marrón","Azul","Avellana", "Verdes")
)
data

plot(data, col = c("lightblue"), 
     main = "Diagrama de mosaico de la tabla bidimensional de frecuencias\n  de colores de cabello y ojos")


sum(data)

rowSums(data)

prop.table(rowSums(data))

colSums(data)

prop.table(colSums(data))

barplot(prop.table(colSums(data)),
        main = "Frecuencias relativas\n de colores de ojos", 
        col = c("burlywood4","lightblue","orange3","lightgreen"),
        ylim = c(0,0.4))

barplot(prop.table(rowSums(data)),
        main = "Frecuencias relativas\n de color de cabello",
        col = c("black","burlywood4","red","yellow"),
        ylim = c(0,0.4))

round(prop.table(data), 3)

round(prop.table(data, margin = 1),3)

round(prop.table(data, margin = 2),3)

barplot(prop.table(data, margin = 1),
        beside = TRUE, col = c("black","brown","red","gold"),
        legend.text = T, main = "Frecuencias relativas de colores de\n cabello para cada color de ojos",
        ylim = c(0,0.8))

barplot(t(prop.table(data, margin = 2)),
        beside = TRUE, 
        col = c("burlywood4","lightblue","gold","lightgreen"),
        legend.text = T, main = "Frecuencias relativas de colores de\n ojos para cada color de pelo",
        ylim = c(0,0.6))


#Datos que no contiene
HairEyeColor <- data.frame(HairEyeColor)
head(HairEyeColor$Hair)
head(HairEyeColor$Eye)
head(HairEyeColor$Sex)



#Tabla de frecuencias en consola
table(HairEyeColor)
HairEyeColor <- data.frame(HairEyeColor)


#Diagrama de dispersion/caja
plot(x = HairEyeColor$Sex, y = HairEyeColor$Freq,main="Relacion entre Frecuencia y Sexo")
plot(x = HairEyeColor$Hair, y = HairEyeColor$Freq,main="Relacion entre Frecuencia y Color de Cabello")
plot(x = HairEyeColor$Eye, y = HairEyeColor$Freq,main="Relacion entre Frequencia y color de ojos")



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

