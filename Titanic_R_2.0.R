setwd('C:/Projetos/Treinos_R_Dplyr_Tidyr')
getwd()

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)

train = fread("C:/Projetos/Titanic/train.csv")
test = fread("C:/Projetos/Titanic/test.csv")

df = bind_rows(train, test) # bind training & test data

View(df)

# Let's check the type of each variable
str(df)

# Checking the amount of people which survived the Titanic disaster
survived.matrix <- matrix( c(table(df$Survived)), nrow = 2, byrow = F)
rownames(survived.matrix) <- c('Died', 'Survived')
colnames(survived.matrix) <- c('died/survived')
survived.matrix
barplot(survived.matrix, legend.text = c('Died', 'Survived'), 
        beside = T, col = c('red','green'),main = 'Number of peole who died/survived at Titanic Disaster',
        ylim = c(0, 600))

survived.prop <- round(prop.table(table(df$Survived)), 3) * 100 # Proportion
survived.prop.matrix <- matrix(survived.prop , nrow = 2, byrow = F)
rownames(survived.prop.matrix) <- c('Died', 'Survived')
colnames(survived.prop.matrix) <- c('died/survived')
survived.prop.matrix

#Checking the classes, 1st, 2nd and 3rd class
data.table(table(df$Pclass))
classes <- round(prop.table(table(df$Pclass)), 3) * 100 # Proportion
rownames(classes) <- c('1st Class', '2nd Class', '3rd Class')
classes

# Checking how many 1st, 2nd and 3rd class survived

df_1st = filter(df, Pclass == 1) 
data.table(table(df_1st$Survived))
firsty <- round(prop.table(table(df_1st$Survived)), 3) * 100
rownames(firsty) <- c('Died', 'Survived')
firsty
# 63% of 1st class people survived

df_2nd = filter(df, Pclass == 2) 
data.table(table(df_2nd$Survived)) 
secondy <- round(prop.table(table(df_2nd$Survived)), 3) * 100
rownames(secondy) <- c('Died', 'Survived')
secondy
# 47.3% of 2nd class people survived

df_3rd = filter(df, Pclass == 3) 
data.table(table(df_3rd$Survived)) 
thirdy <- round(prop.table(table(df_3rd$Survived)), 3) * 100
rownames(thirdy) <- c('Died', 'Survived')
thirdy
# only 24.2% of 3rd class people survived

# Using values from 1st, 2nd and 3rd class survival rate and insert into a matrix
a = data.table(table(df_1st$Survived))[1,2]
b = data.table(table(df_1st$Survived))[2,2]
c = data.table(table(df_2nd$Survived))[1,2]
d = data.table(table(df_2nd$Survived))[2,2]
e = data.table(table(df_3rd$Survived))[1,2] 
f = data.table(table(df_3rd$Survived))[2,2]

matrix_class = matrix(c(a, b, c, d, e, f),nrow = 2, byrow = F)
rownames(matrix_class) = c('Died', 'Survived')
colnames(matrix_class) = c('1st Class', '2nd Class', '3rd Class')

barplot(matrix_class, legend.text = c('Died', 'Survived'), names.arg = NULL, 
        angle = c(45, 315), density = c(20,20), col = c('red','green'), border =T,
        main = 'Number of survivers per Class', ylab = 'Number of Passengers', 
        sub = 'Classes', ylim = c(0, 500), col.main = 52, cex.main = 2,
        col.sub = 52, cex.sub = 1.5)

matrix_class_died = matrix(c(a, c, e),nrow = 3)
lines(matrix_class_died, col = 'blue',lwd = 3)

#By this data we can inform that people which travelled in first class had more chances of surviving 
#than the others classes

# We will check the surviving rate for young and old people
df_young = filter(df, Age < 18)
data.table(table(df_young$Survived)) 
young <- round(prop.table(table(df_young$Survived)), 3) * 100
rownames(young) <- c('Died', 'Survived')
young
# 54% of young people survived

df_old = filter(df, Age >= 65)
data.table(table(df_old$Survived)) 
old <- round(prop.table(table(df_old$Survived)), 3) * 100
rownames(old) <- c('Died', 'Survived')
old
# only 90.9% of old people survived

young_old <- matrix(c(young, old), nrow = 2, byrow = T)
colnames(young_old) <- c('Died', 'Survived')
rownames(young_old) <- c('Young (< 18)  ', 'Old   (>= 65) ')
young_old
# 

#Lets also check if youngsters from 1st, 2nd and 3rd classes had the same chances of surviving

df_young1 = filter(df, (Age < 18) & (Pclass == 1))
data.table(table(df_young1$Survived)) 
young1 <- round(prop.table(table(df_young1$Survived)), 3) * 100
# 91.7% of youngsters in class 1 survived

df_young2 = filter(df, (Age < 18) & (Pclass == 2))
data.table(table(df_young2$Survived)) 
young2 <- round(prop.table(table(df_young2$Survived)), 3) * 100
# 91.3% of youngsters in class 2 survived

df_young3 = filter(df, (Age < 18) & (Pclass == 3))
data.table(table(df_young3$Survived)) 
young3 <- round(prop.table(table(df_young3$Survived)), 3) * 100
# only 37.2% of youngsters lived

young_final <- matrix(c(young1, young2, young3), nrow = 3, byrow = T)
colnames(young_final) <- c('Died', 'Survived')
rownames(young_final) <- c('1st Class', '2nd Class', '3rd Class')
young_final

# We can inform, based ont this numbers, that 3rd class children had less chances of surviving 


