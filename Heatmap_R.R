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

# calf-normalized
df_one_calf <- df_one[,c("Patient","VM","KF_A")]
df_all_calf <- df_all[,c("Patient","VM","KF_A","KF_B_excel","LM_A")]

df_one_calf <- unique(df_one_calf)
df_all_calf <- unique(df_all_calf)

df_one_calf <- df_one_calf[!is.na(df_one_calf$KF_A),] # exclusión

# ====== #
df_final_one <- merge(df_one_calf, df_all_calf, by = c("Patient","VM","KF_A")) # medidas de one_calf

df_repetidos <- df_final_one %>% group_by(Patient) %>% filter(n()>1) %>% ungroup() # más de una vez

# Solo una vez x patient #
df_unicos <- df_final_one %>% distinct(Patient, .keep_all = TRUE)

# Normality Test - Dimensionality ----
str(df_unicos)
shapiro.test(df_unicos$LM_A)
df_all[df_all$Patient=="Drifa Thorvaldsdotter",]
table(df_final$Patient)

summary(df_one$VM)
str(df_one_calf)
