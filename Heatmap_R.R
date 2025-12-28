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



data_heatmap <- read_excel("data/DB_heatmap_one_session_251204.xlsx")
data <- data_heatmap[,c("Patient","Gender","Leg","L_M","MP","VM","KF_A","KF_B_excel","Cm_VM","Cm_KF",
                        "Circ_A","Circ_B","Circ_KF","Circ_MP","LM_A","LM_B","LM_KF")]
str(data_heatmap)
# Validación y filtro riguroso ----

data <- data[!is.na(data$Patient) & !is.na(data$Cm_KF) & !is.na(data$Cm_VM) & !(data$KF_B_excel==0),]

nrow(data)

#df_unicos[is.na(df_unicos$Circ_A),] # Validar
df_unicos <- data[data$MP==1,]
df_repeated <- df_unicos %>% group_by(Patient) %>% filter(n()>1) %>% ungroup() #validar pacientes con diferentes medidas anatómicas


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

data <- data[(data$Cm_KF<=data$ordinate) & (data$Cm_VM<=data$abscissa),]
data[is.na(data$Cm_KF),]

data$proportion_abscissa <- data$measure_abscissa/data$abscissa
data$proportion_ordinate <- data$measure_ordinate/data$ordinate

mean_a <- mean(data$KF_A[data$MP=="1"], na.rm=TRUE)
mean_y <- mean(data$KF_B_excel[data$MP=="1"])
mean_x <- median(data$abscissa[data$MP=="1"])

shapiro.test(data$KF_B_excel[data$MP=="1"])
shapiro.test(data$abscissa[data$MP=="1"])
length(unique(data$Patient))

data$normalized_abscissa <- data$proportion_abscissa*mean_x
data$normalized_ordinate <- data$proportion_ordinate*mean_y

# Subject Characteristics ----
patient <- length(unique(data$Patient))

gernder_f <- length(data$Gender[data$MP==1 & data$Gender=="F"])
gernder_m <- length(data$Gender[data$MP==1 & data$Gender=="M"])

leg_left <- length(data$Leg[data$MP==1 & data$Leg=="L"])
leg_right <- length(data$Leg[data$MP==1 & data$Leg=="R"])
L_M_lat <- length(data$L_M[data$MP==1 & data$L_M=="L"])
L_M_med <- length(data$L_M[data$MP==1 & data$L_M=="M"])

shapiro.test(data$VM[data$MP==1])
meas_VM <-  mean(data$VM[data$MP==1])
meas_KF_B <- mean(data$KF_B_excel[data$MP==1])
meas_KF_A <- mean(data$KF_A[data$MP==1], na.rm = TRUE)

meas_Circ_A <- median(data$Circ_A[data$MP==1])
meas_Circ_B <- median(data$Circ_B[data$MP==1])
meas_Circ_KF <-  mean(data$Circ_KF[data$MP==1])

length(data$MP[data$MP==5])
#Error
e <- qt(0.975, df = length(data$KF_A[data$MP == 1]) - 1) *
  (sd(data$KF_A[data$MP == 1], na.rm = TRUE) / sqrt(length(data$KF_A[data$MP == 1])))

meas_KF_A-e

quantile(data$Circ_B[data$MP==1],0.75)


# Bins Measure ----
zone1 <- sqrt(8.1)
zone12 <- sqrt(42.9)
zonex <- 3

square_size <- zonex

hi<-5

# ======== HEATMAP CUADROS (BINS) + CONTORNO DE DENSIDAD ----

library(ggplot2)
library(viridis)

subset_representative <- data
#subset_representative <- subset(data,data$Representative==1)

ggplot(subset_representative,
       aes(x = normalized_abscissa, y = normalized_ordinate)) +
  
  # Heatmap con cuadrados de lado = square_size
  stat_bin2d(
    binwidth = c(square_size, square_size),
    aes(fill = ..count..),
    color = "white"
  ) +
  
  # Colores del heatmap
  scale_fill_gradientn(
    colors = c("lightyellow","yellow","#FFD700", "#FFA500", "#FF4500", "red"),
    #values = scales::rescale(c(0,4,8,12,18,24))
    values = scales::rescale(c(0,70,90,100,110,120))
  ) +
  
  # Contornos de densidad
  geom_density2d(color = "black") +
  
  # Tema base
  theme_minimal(base_size = 10) +
  
  # Títulos
  labs(
    title = "Heatmap + Density Contours - 6.55cm x 6.55cm",
    x = "PMC",
    y = "MCC",
    fill = "Count"
  ) +
  
  # Estética general
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    axis.title       = element_text(face = "bold"),
    panel.grid       = element_blank(),
    panel.background = element_rect(fill = "white"),
    legend.position  = "right",
    legend.title     = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  
  # Ejes (solo breaks; sin limits, para que el corte lo haga coord_fixed)
  scale_x_continuous(
    breaks = seq(-9, 9, by = 3),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(-27, 3, by = 3),
    expand = c(0, 0)
  ) +
  
  # Líneas de referencia
  geom_vline(xintercept = 0,          linetype = "dashed", size = 0.2) +
  geom_hline(yintercept = 0,          linetype = "dashed", size = 0.2) +
  geom_hline(yintercept = -mean_y,    linetype = "solid",  color = "red", size = 0.2) +
  geom_vline(xintercept = -mean_x,    linetype = "solid",  color = "red", size = 0.2) +
  geom_vline(xintercept =  mean_x,    linetype = "solid",  color = "red", size = 0.2) +
  
  # Relación 1:1 y corte REAL de lo que esté fuera de los límites
  coord_fixed(
    ratio = 1,
    xlim  = c(-8, 8),
    ylim  = c(-25, 0)) +

  annotate("rect",
           xmin = -8, xmax =  8,
           ymin = -25, ymax =  0,
           fill = NA, color = "black", linewidth = 0.1)

# ------ COUNT + HEATMAP ----

library(ggplot2)
library(viridis)

data$MP <- as.factor(data$MP)
subset_representative <- data

# Mantengo tus inputs tal cual
razon_y <- square_size
razon_x <- square_size
limit_x <- 8
limit_y <- 25

ggplot(subset_representative, aes(x = normalized_abscissa, y = normalized_ordinate)) +
  
  # Heatmap con cuadrados (mismo concepto que el 1: tamaño por binwidth)
  stat_bin2d(
    binwidth = c(razon_x, razon_y),
    aes(fill = ..count..),
    color = "white"
  ) +
  
  # Colores (si quieres EXACTO como el 1, usa solo c("yellow","orange","red"))
  scale_fill_gradientn(
    colors = c("lightyellow","yellow","#FFD700", "#FFA500", "#FF4500", "red"),
    #values = scales::rescale(c(0,4,8,12,18,24))
    values = scales::rescale(c(0,70,90,100,110,120))
  ) +
  
  # Puntos encima (para no "ensuciar", uso un alpha leve)
  geom_point(
    color = "brown",
    size  = 1.2,
    alpha = 0.65
  ) +
  
  # Conteo dentro de cada cuadro (coincide con binwidth)
  geom_text(
    stat = "bin2d",
    aes(label = after_stat(count)),
    binwidth = c(razon_x, razon_y),
    vjust = 0.5,
    color = "black",
    size = 4,
    fontface = "bold",
    check_overlap = TRUE
  ) + 
  
  # Tema base como el 1
  theme_minimal(base_size = 10) +
  labs(
    title = "Both Calves - 6.55cm x 6.55cm",
    x = "\nPMC",
    y = "MCC\n",
    fill = "Count"
  ) +
  
  # Estética general estilo 1 (limpio, sin grid, fondo blanco, leyenda similar)
  theme(
    plot.title        = element_text(hjust = 0.5, face = "bold"),
    axis.title        = element_text(face = "bold"),
    axis.text.x       = element_text(size = 11),
    axis.text.y       = element_text(size = 11),
    legend.text       = element_text(size = 10),
    panel.grid        = element_blank(),
    panel.background  = element_rect(fill = "white"),
    legend.position   = "right",
    legend.title      = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = NA),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks        = element_line(linewidth = 0.5)
  ) +
  
  # Ejes: SOLO breaks + expand=0 (como el 1). El corte real lo hace coord_fixed.
  scale_x_continuous(
    breaks = seq(-9, 9, by = 3),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(-27, 3, by = 3),
    expand = c(0, 0)
  ) +
  
  # Líneas rojas (mantengo tu geometría)
  geom_segment(aes(x = -mean_x, y = -mean_y, xend = -mean_x, yend = 0), color = "red", size = 0.2) +
  geom_segment(aes(x =  mean_x, y = -mean_y, xend =  mean_x, yend = 0), color = "red", size = 0.2) +
  geom_segment(aes(x = -mean_x, y = -mean_y, xend =  mean_x, yend = -mean_y), color = "red", size = 0.2) +
  
  # Referencias
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  
  # Corte REAL (como el 1)
  coord_fixed(
    ratio = 1,
    xlim  = c(-limit_x, limit_x),
    ylim  = c(-limit_y, 0)
  ) +
  annotate("rect",
             xmin = -limit_x, xmax =  limit_x,
             ymin = -limit_y, ymax =  0,
             fill = NA, color = "black", linewidth = 0.1)

# ------ COUNT + HEX HEATMAP (HEXÁGONOS REGULARES) ----

library(ggplot2)
library(viridis)
library(hexbin)

data$MP <- as.factor(data$MP)
subset_representative <- data

# === INPUT ÚNICO: lado del hexágono regular ===
hex_side <- square_size   # o el valor que quieras (en la misma unidad de tus ejes)

# Conversión lado -> separación de centros (binwidth)
dx <- 1.5 * hex_side
dy <- sqrt(3) * hex_side

limit_x <- 8
limit_y <- 25

ggplot(subset_representative, aes(x = normalized_abscissa, y = normalized_ordinate)) +
  
  # Heatmap con hexágonos (conteo por celda)
  geom_hex(
    binwidth = c(dx, dy),
    aes(fill = after_stat(count)),
    color = "white"
  ) +
  
  # Colores
  scale_fill_gradientn(
    colors = c("lightyellow","yellow","#FFD700", "#FFA500", "#FF4500", "red"),
    values = scales::rescale(c(0,70,90,100,110,120))
  ) +
  
  # Puntos encima
  geom_point(
    color = "brown",
    size  = 1.2,
    alpha = 0.65
  ) +
  
  # Conteo dentro del hexágono
  geom_text(
    stat = "binhex",
    aes(label = after_stat(count)),
    binwidth = c(dx, dy),
    vjust = 0.5,
    color = "black",
    size = 4,
    fontface = "bold",
    check_overlap = TRUE
  ) +
  
  theme_minimal(base_size = 10) +
  labs(
    title = "Both Calves - Hexagons",
    x = "\nPMC",
    y = "MCC\n",
    fill = "Count"
  ) +
  
  theme(
    plot.title        = element_text(hjust = 0.5, face = "bold"),
    axis.title        = element_text(face = "bold"),
    axis.text.x       = element_text(size = 11),
    axis.text.y       = element_text(size = 11),
    legend.text       = element_text(size = 10),
    panel.grid        = element_blank(),
    panel.background  = element_rect(fill = "white"),
    legend.position   = "right",
    legend.title      = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = NA),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks        = element_line(linewidth = 0.5)
  ) +
  
  # Ejes (mismos breaks que vienes usando)
  scale_x_continuous(
    breaks = seq(-9, 9, by = 3),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(-27, 3, by = 3),
    expand = c(0, 0)
  ) +
  
  # Líneas rojas (si quieres quitar warnings, cambia a annotate("segment", ...) después)
  geom_segment(aes(x = -mean_x, y = -mean_y, xend = -mean_x, yend = 0), color = "red", size = 0.2) +
  geom_segment(aes(x =  mean_x, y = -mean_y, xend =  mean_x, yend = 0), color = "red", size = 0.2) +
  geom_segment(aes(x = -mean_x, y = -mean_y, xend =  mean_x, yend = -mean_y), color = "red", size = 0.2) +
  
  # Referencias
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  
  # Corte real + borde encima
  coord_fixed(
    ratio = 1,
    xlim  = c(-limit_x, limit_x),
    ylim  = c(-limit_y, 0)
  ) +
  annotate("rect",
           xmin = -limit_x, xmax =  limit_x,
           ymin = -limit_y, ymax =  0,
           fill = NA, color = "black", linewidth = 0.1)

# ======== GRÁFICA SOLO CONTORNOS DE DENSIDAD ----

library(spatstat)

subset_representative <- subset(data,(data$Leg=='R'))
subset_representative <- subset(data,(data$Leg=='L'))
subset_representative <- subset(data,)

x <- subset_representative$normalized_abscissa
y <- subset_representative$normalized_ordinate

ggplot(subset_representative, aes(x = normalized_abscissa, y = normalized_ordinate)) +
  geom_density2d(color = "black") + 
  theme_minimal(base_size = 10) +  
  labs(title = "Density Contours",
       x = "Normalized Abscissa",
       y = "Normalized Ordinate") + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.title = element_text(face = "bold"),  
    panel.grid = element_blank(),  
    panel.background = element_rect(fill = "white")
  ) +
  scale_x_continuous(breaks = seq(-9, 9, by = 3), limits = c(-9,9)) +
  scale_y_continuous(breaks = seq(-27, 0, by = 3), limits = c(-27, 0)) +
  coord_fixed(ratio = 1)




#  HEATMAP DENSIDAD (GAUSSIANO) =====
library(spatstat)
library(viridis)

paleta_colores <- c("red", "orange", "pink", "purple","brown")
data$MP <- as.factor(data$MP)

data_density <- data[,c("Leg","MP","normalized_abscissa","normalized_ordinate")]
subset_representative <- subset(data_density,data_density$Leg=='R')
subset_representative <- subset(data_density,data_density$Leg=='L')
subset_representative <- data_density

x <- subset_representative$normalized_abscissa
y <- subset_representative$normalized_ordinate


#xrange <- c(-7.5,7.5)
xrange <- c(-mean_x,mean_x)
#yrange <- c(-26,0)
yrange <- c(-mean_y,0)
window <- owin(xrange = xrange, yrange = yrange)
points_ppp <- ppp(x, y, window = window)

# Crear el gráfico de densidad
sigma_value <- 1.2
#density <- density.ppp(points_ppp, sigma = bw.diggle(points_ppp))
density <- density.ppp(points_ppp, sigma = sigma_value) #no elimina duplicados

# Convertir la densidad a un dataframe++++_
density_df <- as.data.frame(density)
colnames(density_df) <- c("x", "y", "density")

# Crear el heatmap mejorado con ggplot200

ggplot() +
  geom_tile(data = density_df, aes(x = x, y = y, fill = density)) +
  scale_fill_gradientn(colors = topo.colors(50)) +
  geom_point(data = subset_representative, 
             aes(x = normalized_abscissa, y = normalized_ordinate, color = MP),
             size = 2.5) +
  scale_color_manual(values = paleta_colores) +
  theme_minimal(base_size = 10) +
  # geom_point(data = subset_representative[(subset_representative$MP==1) | (subset_representative$MP==2),],
  #            aes(x = normalized_abscissa, y = normalized_ordinate),
  #            color = "black",
  #            size = 1) +
  labs(title = "Right Calf", #Lo cambiamos cuando elejimos otra pierna
       x = "PMC",
       y = "MCC",
       color = "   MP",
       fill = "Density") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10, angle = 90, vjust = 1.5, hjust = 0.448),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  ) +
  scale_x_continuous(breaks = seq(-9, 9, by = 3), limits = c(-8.3, 8.3)) +
  scale_y_continuous(breaks = seq(-27, 3, by = 3), limits = c((-1) * mean_y-0.1, 0.5)) + #ya que la media es decimal, le aumento un poco 0.1
  
  annotate("text", x = -3.8, y = 0, label = "Medial",
           vjust = -0.5, hjust = 0.5) +
  annotate("text", x = 3.8, y = 0, label = "Lateral",
           vjust = -0.5, hjust = 0.5) +
  
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  geom_hline(yintercept = (-1) * mean_a, linetype = "dashed", size = 0.2) +
  geom_hline(yintercept = (-1) * mean_y, linetype = "solid", color = "red", size = 0.2) +
  geom_vline(xintercept = (-1) * mean_x, linetype = "solid", color = "red", size = 0.2) +
  geom_vline(xintercept = mean_x, linetype = "solid", color = "red", size = 0.2)+
  coord_fixed(ratio = 1) +
  annotate("text", x = -8.3, y = 0, label = "KF", 
           vjust = -0.5, hjust = 0.5) 



# 3D Calf ----
str(data)

excluir <- data[is.na(data$Circ_A) | is.na(data$Circ_B) | is.na(data$Circ_KF),]
nrow(data)
length(unique(data$Patient))
rd <- data[!is.na(data$Circ_B) | !is.na(data$Circ_KF),]
nrow(rd)
length(unique(rd$Patient))

summary(rd$Circ_A*0.2)
summary(rd$LM_A)

# Normalized 3D calf ----
shapiro.test(data$Circ_A)
shapiro.test(data$Circ_B)
shapiro.test(data$Circ_KF)
shapiro.test(data$Circ_MP)

borrar <- data[is.na(data$Circ_MP),]


data[is.na(data$Circ_KF),]
# Excluyendo 
data <- data[!is.na(data$Circ_KF) & !is.na(data$Circ_B) & !is.na(data$Circ_A),]
str(data)

mean_circA <- mean(data$Circ_A)
mean_circB <- mean(data$Circ_B)
mean_circKF <- mean(data$Circ_KF)

## gráfica del plano
# ======== TRONCO 3D (KF -> A -> B) DESDE CIRCUNFERENCIAS PROMEDIO ========

# Paquetes
#install.packages("plotly")   # si no lo tienes
library(plotly)

# ---- Inputs (ya los tienes calculados) ----
# mean_circKF, mean_circA, mean_circB : circunferencias (cm)
# mean_a, mean_b : distancias verticales (cm) (positivas), pero en z serán negativas

C_KF <- mean_circKF
C_A  <- mean_circA
C_B  <- mean_circB

z_KF <- 0
z_A  <- -mean_a
z_B  <- -mean_y   # debe ser más negativo que z_A (más abajo)

# ---- Radios (circunferencia -> radio) ----
r_KF <- C_KF / (2*pi)
r_A  <- C_A  / (2*pi)
r_B  <- C_B  / (2*pi)

# ---- Función radio r(z) por tramos (KF->A y A->B) ----
r_of_z <- function(z) {
  if (z >= z_A) {
    # tramo KF -> A (z en [z_A, 0])
    t <- (z - z_KF) / (z_A - z_KF)   # z_KF=0, z_A<0
    (1 - t) * r_KF + t * r_A
  } else {
    # tramo A -> B (z en [z_B, z_A])
    t <- (z - z_A) / (z_B - z_A)
    (1 - t) * r_A + t * r_B
  }
}

# ---- Malla para superficie ----
n_theta <- 160
n_z     <- 120

theta <- seq(0, 2*pi, length.out = n_theta)      # círculo completo
z_seq <- seq(z_B, z_KF, length.out = n_z)        # de abajo hacia arriba (negativo -> 0)

# Matrices para plotly surface
X <- matrix(NA_real_, nrow = n_z, ncol = n_theta)
Y <- matrix(NA_real_, nrow = n_z, ncol = n_theta)
Z <- matrix(NA_real_, nrow = n_z, ncol = n_theta)

for (i in seq_along(z_seq)) {
  zi <- z_seq[i]
  ri <- r_of_z(zi)
  X[i, ] <- ri * cos(theta)
  Y[i, ] <- ri * sin(theta)
  Z[i, ] <- zi
}

# ---- Círculos de referencia en KF, A, B ----
circle_xyz <- function(r, z, n = 200) {
  th <- seq(0, 2*pi, length.out = n)
  list(
    x = r * cos(th),
    y = r * sin(th),
    z = rep(z, length(th))
  )
}

cKF <- circle_xyz(r_KF, z_KF)
cA  <- circle_xyz(r_A,  z_A)
cB  <- circle_xyz(r_B,  z_B)

# ---- Plot 3D ----
p <- plot_ly() %>%
  add_surface(
    x = X, y = Y, z = Z,
    opacity = 0.65,
    showscale = FALSE
  ) %>%
  add_trace(type = "scatter3d", mode = "lines", x = cKF$x, y = cKF$y, z = cKF$z,
            line = list(width = 6), name = "KF (z=0)") %>%
  add_trace(type = "scatter3d", mode = "lines", x = cA$x, y = cA$y, z = cA$z,
            line = list(width = 6), name = "A") %>%
  add_trace(type = "scatter3d", mode = "lines", x = cB$x, y = cB$y, z = cB$z,
            line = list(width = 6), name = "B") %>%
  layout(
    scene = list(
      xaxis = list(title = "X (cm)"),
      yaxis = list(title = "Y (cm)"),
      zaxis = list(title = "Height z (cm)"),
      aspectmode = "data"
    ),
    title = "3D Calf Trunk (centered, negative heights)"
  )

p
