# renv::install("caret")
# renv::install("mice")
# renv::install("dbscan")
# renv::install("ggplot2")
# renv::install("viridis")
# renv::install("spatstat")
# renv::install("dplyr")
# renv::install("ggforce") 
# renv::install("ggimage") 
# renv::install("magick") #QUEDÉ
# renv::install("grid")

library(ggplot2)
library(caret)
library(mice)
library(stats)
library(dbscan)
library(viridisLite)
library(viridis)
library(spatstat)
library(dplyr)
library(magick)

library(ggforce)
library(ggimage)
library(magick)
library(grid)
library(readxl)


df_one <- read_excel("data/DB_heatmap_one_session.xlsx")
df_all <- read_excel("data/DB_all_motor_points.xlsx")

# Cross Tables ----
df_one_calf <- df_one[,c("Patient","VM","KF_A")]
df_all_calf <- df_all[,c("Patient","VM","KF_A","KF_B_excel","LM_A")]

df_one_calf <- unique(df_one_calf)
df_all_calf <- unique(df_all_calf)

df_one_calf <- df_one_calf[!is.na(df_one_calf$KF_A),] # exclusión

# ====== #
df_final_one <- merge(df_one_calf, df_all_calf, by = c("Patient","VM","KF_A")) # medidas de one_calf

df_repetidos <- df_final_one %>% group_by(Patient) %>% filter(n()>1) %>% ungroup() # más de una vez

# Solo una vez x patient # TABLA FINAL
df_unicos <- df_final_one %>% distinct(Patient, .keep_all = TRUE)

# Normality Test - Dimensionality
str(df_unicos)
shapiro.test(df_unicos$LM_A)

str(df_one)

data <- df_one[,c("Patient","Leg","L_M","MP","VM","KF_A","Cm_VM","Cm_KF","Circ_A","Circ_B","Circ_KF","Circ_MP")]

data <- merge(data, df_unicos, by = c("Patient","VM","KF_A"))
length(unique(data$Patient))


# Normalized Calf ----
data$abscissa <- data$`Circ_A`*0.2 
data$ordinate <- data$KF_B_excel

# 
str(data)
data$measure_ordinate <- (-1)*data$`Cm_KF`
data$measure_abscissa <- ifelse(((data$Leg=='L') & (data$`L_M`=='L'))|
                                  ((data$Leg=='R') & (data$`L_M`=='M')),
                                (-1)*data$`Cm_VM`,
                                data$`Cm_VM`)

nrow(data)

data[(data$Cm_KF>data$ordinate) | (data$Cm_VM>data$abscissa),] # Son 6 registros
data <- data[(data$Cm_KF<=data$ordinate) & (data$Cm_VM<=data$abscissa),]

data$proportion_abscissa <- data$measure_abscissa/data$abscissa
data$proportion_ordinate <- data$measure_ordinate/data$ordinate

mean_a <- mean(data$KF_A[data$MP=="1"])
mean_y <- mean(data$KF_B_excel[data$MP=="1"])
mean_x <- mean(data$abscissa[data$MP=="1"])

length(unique(data$Patient))

data$normalized_abscissa <- data$proportion_abscissa*mean_x
data$normalized_ordinate <- data$proportion_ordinate*mean_y

