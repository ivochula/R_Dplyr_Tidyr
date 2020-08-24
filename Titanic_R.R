setwd('C:/Projetos/Treinos_R_Dplyr_Tidyr')
getwd()

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)

df = fread("C:/Projetos/Titanic/train.csv")
View(df)

# Let's check the type of each variable
str(df)

# Checking the amount of people which survived the Titanic disaster
data.table(table(df$Survived)) 
round(prop.table(table(df$Survived)), 3) * 100 # Proportion

#Checking the classes, 1st, 2nd and 3rd class
data.table(table(df$Pclass)) 
round(prop.table(table(df$Pclass)), 3) * 100 # Proportion

# Checking how many 1st, 2nd and 3rd class survived

df_1st = filter(df, Pclass == 1) 
data.table(table(df_1st$Survived))
round(prop.table(table(df_1st$Survived)), 3) * 100
# 63% of 1st class people survived

df_2nd = filter(df, Pclass == 2) 
data.table(table(df_2nd$Survived)) 
round(prop.table(table(df_2nd$Survived)), 3) * 100
# 47.3% of 2nd class people survived

df_3rd = filter(df, Pclass == 3) 
data.table(table(df_3rd$Survived)) 
round(prop.table(table(df_3rd$Survived)), 3) * 100
# only 24.2% of 3rd class people survived

a = data.table(table(df_1st$Survived))[1,2]
b = data.table(table(df_1st$Survived))[2,2]
c = data.table(table(df_2nd$Survived))[1,2]
d = data.table(table(df_2nd$Survived))[2,2]
e = data.table(table(df_3rd$Survived))[1,2] 
f = data.table(table(df_3rd$Survived))[2,2]

vector_class = matrix(c(a, b, c, d, e, f),nrow = 2, byrow = F)
rownames(vector_class) = c('Died', 'Survived')
colnames(vector_class) = c('1st Class', '2nd Class', '3rd Class')

barplot(vector_class, legend.text = c('Died', 'Survived'), names.arg = NULL, 
        angle = c(45, 315), density = c(20,20), col = c('red','green'), border =T,
        main = 'Number of survivers per Class', ylab = 'Number of Passengers', 
        sub = 'Classes', ylim = c(0, 500), col.main = 52, cex.main = 2,
        col.sub = 52, cex.sub = 1.5)

vector_class_died = matrix(c(a, c, e),nrow = 3)
lines(vector_class_died, col = 'blue',lwd = 3)



#By this data we can inform that people which travelled in first class had more chances of surviving 
#than the others classes

# We will check the surviving rate for young and old people
df_young = filter(df, Age <= 18)
data.table(table(df_young$Survived)) 
round(prop.table(table(df_young$Survived)), 3) * 100
# 50.4% of young people survived

df_old = filter(df, Age >= 65)
data.table(table(df_old$Survived)) 
round(prop.table(table(df_old$Survived)), 3) * 100
# only 9.1% of old people survived

#Lets also check if youngsters from 1st, 2nd and 3rd classes had the same chances of surviving

df_young1 = filter(df, (Age <= 18) & (Pclass == 1))
data.table(table(df_young1$Survived)) 
round(prop.table(table(df_young1$Survived)), 3) * 100
# 87.5% of youngsters in class 2 survived

df_young2 = filter(df, (Age <= 18) & (Pclass == 2))
data.table(table(df_young2$Survived)) 
round(prop.table(table(df_young2$Survived)), 3) * 100
# 79.3% of youngsters in class 2 survived

df_young3 = filter(df, (Age <= 18) & (Pclass == 3))
data.table(table(df_young3$Survived)) 
round(prop.table(table(df_young3$Survived)), 3) * 100
# only 35.1% of youngsters lived


