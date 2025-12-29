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


# Normalized 3D calf 
shapiro.test(data$Circ_A)
shapiro.test(data$Circ_B)
shapiro.test(data$Circ_KF)
shapiro.test(data$Circ_MP)

borrar <- data[is.na(data$Circ_MP),]

# Filtros 

data <- data[!is.na(data$Circ_KF) & !is.na(data$Circ_B) & !is.na(data$Circ_A),]
data <- data[!is.na(data$Circ_MP),]
data[is.na(data$Circ_KF),]
str(data)

mean_circA <- mean(data$Circ_A)
mean_circB <- mean(data$Circ_B)
mean_circKF <- mean(data$Circ_KF)

## gráfica del plano
# ======== TRONCO 3D (KF -> A -> B) DESDE CIRCUNFERENCIAS PROMEDIO ========

# Paquetes
#install.packages("plotly")   # si no lo tienes
library(plotly)

# ---- Inputs (ya los tienes calculados)
# mean_circKF, mean_circA, mean_circB : circunferencias (cm)
# mean_a, mean_b : distancias verticales (cm) (positivas), pero en z serán negativas

C_KF <- mean_circKF
C_A  <- mean_circA
C_B  <- mean_circB

z_KF <- 0
z_A  <- -mean_a
z_B  <- -mean_y   # debe ser más negativo que z_A (más abajo)

# ---- Radios (circunferencia -> radio) 
r_KF <- C_KF / (2*pi)
r_A  <- C_A  / (2*pi)
r_B  <- C_B  / (2*pi)

# ---- Función radio r(z) por tramos (KF->A y A->B) 
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

# ---- Malla para superficie 
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

# ---- Círculos de referencia en KF, A, B 
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

# ---- Plot 3D 
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

# Filtros y generaciòn de parámetros ----
min(-data$ordinate)
mean_y
max(data$normalized_ordinate)
mean(data$Circ_MP)
# La idea es quedarse solo con una observación por sujeto en el eje Y
a <- unique(data[,c("Patient","normalized_ordinate","Circ_MP")])
b <- unique(data[,c("Patient","normalized_ordinate")])

anti_join(a, b, by = c("Patient", "normalized_ordinate"))

library(dplyr)

full_join(
  a %>% count(Patient, normalized_ordinate, name = "n_a"),
  b %>% count(Patient, normalized_ordinate, name = "n_b"),
  by = c("Patient", "normalized_ordinate")
) %>%
  filter(is.na(n_a) | is.na(n_b) | n_a != n_b)

nrow(data)
data[,c("Patient","normalized_ordinate","Cm_KF","Circ_MP")][data$Patient=="Ilya Talashkevich",]

# Mega importante para poder generar el resto de gráficas:
circ_by_h <- a %>%
  group_by(normalized_ordinate, Patient) %>%
  summarise(circ_patient = mean(Circ_MP), .groups = "drop") %>%
  group_by(normalized_ordinate) %>%
  summarise(
    n_patients = n(),
    circ_h = ifelse(n_patients > 1,
                    mean(circ_patient),
                    circ_patient),
    .groups = "drop"
  )


# Gráfica con todas las circunferencias superpuestas : ----
library(plotly)
library(dplyr)

# anclas
z_KF <- 0
z_A  <- -mean_a
z_B  <- -mean_y

r_KF <- mean_circKF / (2*pi)
r_A  <- mean_circA  / (2*pi)
r_B  <- mean_circB  / (2*pi)

# radio por tramos (tronco)
r_of_z <- function(z){
  if (z >= z_A){
    t <- (z - z_KF) / (z_A - z_KF)
    (1 - t)*r_KF + t*r_A
  } else {
    t <- (z - z_A) / (z_B - z_A)
    (1 - t)*r_A + t*r_B
  }
}

# superficie del tronco
theta <- seq(0, 2*pi, length.out = 160)
z_seq <- seq(z_B, z_KF, length.out = 120)

X <- Y <- Z <- matrix(NA, nrow = length(z_seq), ncol = length(theta))

for (i in seq_along(z_seq)){
  ri <- r_of_z(z_seq[i])
  X[i,] <- ri * cos(theta)
  Y[i,] <- ri * sin(theta)
  Z[i,] <- z_seq[i]
}

# circunferencias adicionales por altura (intra -> inter ya calculadas)
circ_lines <- circ_by_h %>%
  mutate(
    z = normalized_ordinate,
    r = circ_h / (2*pi)
  )

make_circle <- function(r, z, n = 200){
  th <- seq(0, 2*pi, length.out = n)
  list(
    x = r * cos(th),
    y = r * sin(th),
    z = rep(z, length(th))
  )
}

p <- plot_ly() %>%
  add_surface(x = X, y = Y, z = Z, opacity = 0.6, showscale = FALSE)

for (i in seq_len(nrow(circ_lines))){
  cci <- make_circle(circ_lines$r[i], circ_lines$z[i])
  p <- p %>% add_trace(
    type = "scatter3d",
    mode = "lines",
    x = cci$x,
    y = cci$y,
    z = cci$z,
    line = list(width = 2),
    showlegend = FALSE
  )
}

p <- p %>% layout(
  scene = list(
    aspectmode = "data",
    xaxis = list(title = "X"),
    yaxis = list(title = "Y"),
    zaxis = list(title = "Height")
  ),
  title = "3D Trunk with Circumferences by Height"
)

p

# Suavizado : ----
library(dplyr)
library(plotly)

SMOOTH_DF <- 6

z_KF <- 0
z_A  <- -mean_a
z_B  <- -mean_y

circ_by_h <- data %>%
  group_by(normalized_ordinate, Patient) %>%
  summarise(circ_patient = mean(Circ_MP), .groups = "drop") %>%
  group_by(normalized_ordinate) %>%
  summarise(
    n_patients = n(),
    circ_h = ifelse(n_patients > 1, mean(circ_patient), circ_patient),
    .groups = "drop"
  )

anchors <- tibble(
  z = c(z_KF, z_A, z_B),
  circ = c(mean_circKF, mean_circA, mean_circB),
  w = c(1e6, 1e6, 1e6)
)

fit_dat <- bind_rows(
  circ_by_h %>% transmute(z = normalized_ordinate, circ = circ_h, w = 1),
  anchors
) %>% arrange(z)

sp <- smooth.spline(
  x = fit_dat$z,
  y = fit_dat$circ,
  w = fit_dat$w,
  df = SMOOTH_DF
)

z_seq <- seq(min(fit_dat$z), max(fit_dat$z), length.out = 180)
circ_seq <- predict(sp, x = z_seq)$y
r_seq <- circ_seq / (2*pi)

r_of_z <- function(z)
  approx(x = z_seq, y = r_seq, xout = z, rule = 2)$y

theta <- seq(0, 2*pi, length.out = 220)
z_grid <- seq(z_B, z_KF, length.out = 180)

X <- Y <- Z <- matrix(NA_real_, nrow = length(z_grid), ncol = length(theta))
for (i in seq_along(z_grid)) {
  ri <- r_of_z(z_grid[i])
  X[i,] <- ri * cos(theta)
  Y[i,] <- ri * sin(theta)
  Z[i,] <- z_grid[i]
}

make_circle <- function(r, z, n = 260){
  th <- seq(0, 2*pi, length.out = n)
  list(x = r*cos(th), y = r*sin(th), z = rep(z, length(th)))
}

cKF <- make_circle(mean_circKF/(2*pi), z_KF)
cA  <- make_circle(mean_circA /(2*pi), z_A)
cB  <- make_circle(mean_circB /(2*pi), z_B)

plot_ly() %>%
  add_surface(x = X, y = Y, z = Z, opacity = 0.65, showscale = FALSE) %>%
  add_trace(type="scatter3d", mode="lines", x=cKF$x, y=cKF$y, z=cKF$z, line=list(width=8), showlegend=FALSE) %>%
  add_trace(type="scatter3d", mode="lines", x=cA$x,  y=cA$y,  z=cA$z,  line=list(width=8), showlegend=FALSE) %>%
  add_trace(type="scatter3d", mode="lines", x=cB$x,  y=cB$y,  z=cB$z,  line=list(width=8), showlegend=FALSE) %>%
  layout(
    scene = list(
      aspectmode = "data",
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "Height")
    ),
    title = "3D Calf - Smoothed Trunk"
  )


# Validación:  ----
cbind(
  which_max = c("KF","A","B")[which.max(c(mean_circKF, mean_circA, mean_circB))],
  max_value = max(mean_circKF, mean_circA, mean_circB),
  KF = mean_circKF, A = mean_circA, B = mean_circB
)
mean_y
stopifnot(mean_circA >= mean_circKF, mean_circA >= mean_circB) # Importante para detener

# Transformando : generatriz = mean_y, se hace diferencial para calcular z (altura) ----
library(dplyr)
library(plotly)

SMOOTH_DF <- 6

# s = distancia sobre superficie desde KF (positiva hacia abajo)
# OJO: si normalized_ordinate ya es negativo hacia abajo, lo vuelvo positivo
circ_by_s <- data %>%
  group_by(normalized_ordinate, Patient) %>%
  summarise(circ_patient = mean(Circ_MP), .groups = "drop") %>%
  group_by(normalized_ordinate) %>%
  summarise(
    n_patients = n(),
    circ_h = ifelse(n_patients > 1, mean(circ_patient), circ_patient),
    .groups = "drop"
  ) %>%
  transmute(s = -normalized_ordinate, circ = circ_h) %>%
  arrange(s)

s_KF <- 0
s_A  <- mean_a
s_B  <- mean_y   # tu "mean_y" es la distancia superficial hasta B (según lo que dijiste)

anchors <- tibble(
  s = c(s_KF, s_A, s_B),
  circ = c(mean_circKF, mean_circA, mean_circB),
  w = c(1e6, 1e6, 1e6)
)

fit_dat <- bind_rows(
  circ_by_s %>% mutate(w = 1),
  anchors
) %>% arrange(s)

sp <- smooth.spline(x = fit_dat$s, y = fit_dat$circ, w = fit_dat$w, df = SMOOTH_DF)

s_grid <- seq(min(fit_dat$s), max(fit_dat$s), length.out = 400)

circ_grid <- predict(sp, x = s_grid)$y
r_grid <- circ_grid / (2*pi)

drds <- predict(sp, x = s_grid, deriv = 1)$y / (2*pi)

dzds <- sqrt(pmax(0, 1 - drds^2))

z_grid_pos <- c(0, cumsum((dzds[-1] + dzds[-length(dzds)]) / 2 * diff(s_grid)))

# altura axial negativa hacia abajo (KF=0, abajo negativo)
z_grid <- -z_grid_pos

r_of_s <- function(s) approx(s_grid, r_grid, xout = s, rule = 2)$y
z_of_s <- function(s) approx(s_grid, z_grid, xout = s, rule = 2)$y

theta <- seq(0, 2*pi, length.out = 220)
s_surf <- seq(0, max(s_grid), length.out = 180)

X <- Y <- Z <- matrix(NA_real_, nrow = length(s_surf), ncol = length(theta))

for (i in seq_along(s_surf)) {
  ri <- r_of_s(s_surf[i])
  zi <- z_of_s(s_surf[i])
  X[i,] <- ri * cos(theta)
  Y[i,] <- ri * sin(theta)
  Z[i,] <- zi
}

make_circle <- function(r, z, n = 260){
  th <- seq(0, 2*pi, length.out = n)
  list(x = r*cos(th), y = r*sin(th), z = rep(z, length(th)))
}

z_KF <- 0
z_Ax <- z_of_s(s_A)
z_Bx <- z_of_s(s_B)

cKF <- make_circle(mean_circKF/(2*pi), z_KF)
cA  <- make_circle(mean_circA /(2*pi), z_Ax)
cB  <- make_circle(mean_circB /(2*pi), z_Bx)

plot_ly() %>%
  add_surface(x = X, y = Y, z = Z, opacity = 0.65, showscale = FALSE) %>%
  add_trace(type="scatter3d", mode="lines", x=cKF$x, y=cKF$y, z=cKF$z, line=list(width=8), showlegend=FALSE) %>%
  add_trace(type="scatter3d", mode="lines", x=cA$x,  y=cA$y,  z=cA$z,  line=list(width=8), showlegend=FALSE) %>%
  add_trace(type="scatter3d", mode="lines", x=cB$x,  y=cB$y,  z=cB$z,  line=list(width=8), showlegend=FALSE) %>%
  layout(
    scene = list(aspectmode = "data",
                 xaxis = list(title = "X"),
                 yaxis = list(title = "Y"),
                 zaxis = list(title = "Axial height z")),
    title = "3D Calf: same curvature, axial height recovered from surface length"
  )


# validación de distancia generatriz y longitud axial de la ordenada ----
tail(s_grid, 1)          # debe ser ~ mean_y
tail(z_grid_pos, 1)      # altura axial total

sum(sqrt( (diff(z_grid_pos))^2 + (diff(r_grid))^2 )) # Todo está bien calculado, el resultado da la generatriz
# Sector encima del prototype : ----
library(dplyr)
library(plotly)

SMOOTH_DF <- 6

FACTOR_KF <- 0.15*2
FACTOR_A  <- 0.20*2
FACTOR_B  <- 0.20*2

THETA0 <- 0            # dirección central (rad). Cambia si quieres rotar el "hexágono"
N_THETA_TRUNK <- 220
N_S_TRUNK     <- 180

N_THETA_PATCH <- 120
N_S_PATCH     <- 180

TRUNK_OPACITY <- 0.55
PATCH_OPACITY <- 0.85

TRUNK_COLOR <- "gray90"
PATCH_COLOR <- "brown"

circ_by_s <- data %>%
  group_by(normalized_ordinate, Patient) %>%
  summarise(circ_patient = mean(Circ_MP), .groups = "drop") %>%
  group_by(normalized_ordinate) %>%
  summarise(
    n_patients = n(),
    circ_h = ifelse(n_patients > 1, mean(circ_patient), circ_patient),
    .groups = "drop"
  ) %>%
  transmute(s = -normalized_ordinate, circ = circ_h) %>%
  arrange(s)

s_KF <- 0
s_A  <- mean_a
s_B  <- mean_y

anchors <- tibble(
  s = c(s_KF, s_A, s_B),
  circ = c(mean_circKF, mean_circA, mean_circB),
  w = c(1e6, 1e6, 1e6)
)

fit_dat <- bind_rows(
  circ_by_s %>% mutate(w = 1),
  anchors
) %>% arrange(s)

sp <- smooth.spline(x = fit_dat$s, y = fit_dat$circ, w = fit_dat$w, df = SMOOTH_DF)

s_grid <- seq(min(fit_dat$s), max(fit_dat$s), length.out = 400)

circ_grid <- predict(sp, x = s_grid)$y
r_grid <- circ_grid / (2*pi)

drds <- predict(sp, x = s_grid, deriv = 1)$y / (2*pi)
dzds <- sqrt(pmax(0, 1 - drds^2))

z_grid_pos <- c(0, cumsum((dzds[-1] + dzds[-length(dzds)]) / 2 * diff(s_grid)))
z_grid <- -z_grid_pos

r_of_s <- function(s) approx(s_grid, r_grid, xout = s, rule = 2)$y
z_of_s <- function(s) approx(s_grid, z_grid, xout = s, rule = 2)$y

make_circle <- function(r, z, n = 260){
  th <- seq(0, 2*pi, length.out = n)
  list(x = r*cos(th), y = r*sin(th), z = rep(z, length(th)))
}

z_KF <- 0
z_Ax <- z_of_s(s_A)
z_Bx <- z_of_s(s_B)

cKF <- make_circle(mean_circKF/(2*pi), z_KF)
cA  <- make_circle(mean_circA /(2*pi), z_Ax)
cB  <- make_circle(mean_circB /(2*pi), z_Bx)

theta_trunk <- seq(0, 2*pi, length.out = N_THETA_TRUNK)
s_trunk <- seq(0, max(s_grid), length.out = N_S_TRUNK)

X <- Y <- Z <- matrix(NA_real_, nrow = length(s_trunk), ncol = length(theta_trunk))
for (i in seq_along(s_trunk)) {
  ri <- r_of_s(s_trunk[i])
  zi <- z_of_s(s_trunk[i])
  X[i,] <- ri * cos(theta_trunk)
  Y[i,] <- ri * sin(theta_trunk)
  Z[i,] <- zi
}

w_KF <- 2*pi*FACTOR_KF
w_A  <- 2*pi*FACTOR_A
w_B  <- 2*pi*FACTOR_B

width_of_z <- function(z){
  if (z >= z_Ax) {
    t <- (z - z_KF) / (z_Ax - z_KF)
    (1 - t)*w_KF + t*w_A
  } else {
    t <- (z - z_Ax) / (z_Bx - z_Ax)
    (1 - t)*w_A + t*w_B
  }
}

theta_unit <- seq(0, 1, length.out = N_THETA_PATCH)
s_patch <- seq(0, s_B, length.out = N_S_PATCH)

Xp <- Yp <- Zp <- matrix(NA_real_, nrow = length(s_patch), ncol = length(theta_unit))
Sp <- matrix(1, nrow = length(s_patch), ncol = length(theta_unit))

for (i in seq_along(s_patch)) {
  ri <- r_of_s(s_patch[i])
  zi <- z_of_s(s_patch[i])
  wi <- width_of_z(zi)
  th_min <- THETA0 - wi/2
  th_max <- THETA0 + wi/2
  th_row <- th_min + (th_max - th_min) * theta_unit
  Xp[i,] <- ri * cos(th_row)
  Yp[i,] <- ri * sin(th_row)
  Zp[i,] <- zi
}

p <- plot_ly() %>%
  add_surface(
    x = X, y = Y, z = Z,
    surfacecolor = matrix(1, nrow = nrow(X), ncol = ncol(X)),
    colorscale = list(list(0, TRUNK_COLOR), list(1, TRUNK_COLOR)),
    opacity = TRUNK_OPACITY,
    showscale = FALSE,
    name = "Trunk"
  ) %>%
  add_surface(
    x = Xp, y = Yp, z = Zp,
    surfacecolor = Sp,
    colorscale = list(list(0, PATCH_COLOR), list(1, PATCH_COLOR)),
    opacity = PATCH_OPACITY,
    showscale = FALSE,
    name = "Curved hex patch"
  ) %>%
  add_trace(type="scatter3d", mode="lines", x=cKF$x, y=cKF$y, z=cKF$z, line=list(width=7), showlegend=FALSE) %>%
  add_trace(type="scatter3d", mode="lines", x=cA$x,  y=cA$y,  z=cA$z,  line=list(width=7), showlegend=FALSE) %>%
  add_trace(type="scatter3d", mode="lines", x=cB$x,  y=cB$y,  z=cB$z,  line=list(width=7), showlegend=FALSE) %>%
  layout(
    scene = list(
      aspectmode = "data",
      xaxis = list(title = "X"),
      yaxis = list(title = "Y"),
      zaxis = list(title = "Axial height z")
    ),
    title = "3D Calf + Curved Hexagon Patch"
  )

p


# --- NUEVO:----
library(dplyr)
library(plotly)
library(spatstat)
# library(spatstat.geom)
# library(spatstat.core)

SMOOTH_DF <- 6

FACTOR_KF <- 0.15*2
FACTOR_A  <- 0.20*2
FACTOR_B  <- 0.20*2

THETA0 <- 0

N_THETA_TRUNK <- 220
N_S_TRUNK     <- 180

N_THETA_PATCH <- 140
N_S_PATCH     <- 220

TRUNK_OPACITY <- 0.40
PATCH_OPACITY <- 0.98

TRUNK_COLOR <- "black"

SIGMA_VALUE <- 1.2
DENS_COLORSCALE <- "Viridis"

SHOW_POINTS <- TRUE
POINT_SIZE <- 2.5
PALETTE_MP <- c("red", "orange", "pink", "purple", "brown")

data$MP <- as.factor(data$MP)

circ_by_s <- data %>%
  group_by(normalized_ordinate, Patient) %>%
  summarise(circ_patient = mean(Circ_MP), .groups = "drop") %>%
  group_by(normalized_ordinate) %>%
  summarise(
    n_patients = n(),
    circ_h = ifelse(n_patients > 1, mean(circ_patient), circ_patient),
    .groups = "drop"
  ) %>%
  transmute(s = -normalized_ordinate, circ = circ_h) %>%
  arrange(s)

s_KF <- 0
s_A  <- mean_a
s_B  <- mean_y

anchors <- tibble(
  s = c(s_KF, s_A, s_B),
  circ = c(mean_circKF, mean_circA, mean_circB),
  w = c(1e6, 1e6, 1e6)
)

fit_dat <- bind_rows(
  circ_by_s %>% mutate(w = 1),
  anchors
) %>% arrange(s)

sp <- smooth.spline(x = fit_dat$s, y = fit_dat$circ, w = fit_dat$w, df = SMOOTH_DF)

s_grid <- seq(min(fit_dat$s), max(fit_dat$s), length.out = 600)
circ_grid <- predict(sp, x = s_grid)$y
r_grid <- circ_grid / (2*pi)

drds <- predict(sp, x = s_grid, deriv = 1)$y / (2*pi)
dzds <- sqrt(pmax(0, 1 - drds^2))

z_grid_pos <- c(0, cumsum((dzds[-1] + dzds[-length(dzds)]) / 2 * diff(s_grid)))
z_grid <- -z_grid_pos

r_of_s <- function(s) approx(s_grid, r_grid, xout = s, rule = 2)$y
z_of_s <- function(s) approx(s_grid, z_grid, xout = s, rule = 2)$y

z_KF <- 0
z_Ax <- z_of_s(s_A)
z_Bx <- z_of_s(s_B)

make_circle <- function(r, z, n = 260){
  th <- seq(0, 2*pi, length.out = n)
  list(x = r*cos(th), y = r*sin(th), z = rep(z, length(th)))
}

cKF <- make_circle(mean_circKF/(2*pi), z_KF)
cA  <- make_circle(mean_circA /(2*pi), z_Ax)
cB  <- make_circle(mean_circB /(2*pi), z_Bx)

theta_trunk <- seq(0, 2*pi, length.out = N_THETA_TRUNK)
s_trunk <- seq(0, max(s_grid), length.out = N_S_TRUNK)

X <- Y <- Z <- matrix(NA_real_, nrow = length(s_trunk), ncol = length(theta_trunk))
for (i in seq_along(s_trunk)) {
  ri <- r_of_s(s_trunk[i])
  zi <- z_of_s(s_trunk[i])
  X[i,] <- ri * cos(theta_trunk)
  Y[i,] <- ri * sin(theta_trunk)
  Z[i,] <- zi
}

w_KF <- 2*pi*FACTOR_KF
w_A  <- 2*pi*FACTOR_A
w_B  <- 2*pi*FACTOR_B

width_of_z <- function(z){
  if (z >= z_Ax) {
    t <- (z - z_KF) / (z_Ax - z_KF)
    (1 - t)*w_KF + t*w_A
  } else {
    t <- (z - z_Ax) / (z_Bx - z_Ax)
    (1 - t)*w_A + t*w_B
  }
}

theta_unit <- seq(0, 1, length.out = N_THETA_PATCH)
s_patch <- seq(0, s_B, length.out = N_S_PATCH)

Xp <- Yp <- Zp <- matrix(NA_real_, nrow = length(s_patch), ncol = length(theta_unit))
ThetaPatch <- matrix(NA_real_, nrow = length(s_patch), ncol = length(theta_unit))
Rpatch <- matrix(NA_real_, nrow = length(s_patch), ncol = length(theta_unit))

for (i in seq_along(s_patch)) {
  ri <- r_of_s(s_patch[i])
  zi <- z_of_s(s_patch[i])
  wi <- width_of_z(zi)
  th_min <- THETA0 - wi/2
  th_max <- THETA0 + wi/2
  th_row <- th_min + (th_max - th_min) * theta_unit
  Xp[i,] <- ri * cos(th_row)
  Yp[i,] <- ri * sin(th_row)
  Zp[i,] <- zi
  ThetaPatch[i,] <- th_row
  Rpatch[i,] <- ri
}

xrange <- c(-mean_x, mean_x)
yrange <- c(-mean_y, 0)
win <- owin(xrange = xrange, yrange = yrange)
pp <- ppp(x = data$normalized_abscissa, y = data$normalized_ordinate, window = win)

dens_im <- density.ppp(pp, sigma = SIGMA_VALUE)

x2_mat <- (ThetaPatch - THETA0) * Rpatch
y2_mat <- -matrix(s_patch, nrow = length(s_patch), ncol = length(theta_unit))  # vuelve a ordinada negativa

dens_vals <- spatstat.geom::lookup.im(dens_im, x = as.vector(x2_mat), y = as.vector(y2_mat))
Dmat <- matrix(dens_vals, nrow = nrow(x2_mat), ncol = ncol(x2_mat), byrow = FALSE)

p <- plot_ly() %>%
  add_surface(
    x = X, y = Y, z = Z,
    surfacecolor = matrix(1, nrow = nrow(X), ncol = ncol(X)),
    colorscale = list(list(0, TRUNK_COLOR), list(1, TRUNK_COLOR)),
    opacity = TRUNK_OPACITY,
    showscale = FALSE,
    name = "Trunk"
  ) %>%
  add_surface(
    x = Xp, y = Yp, z = Zp,
    surfacecolor = Dmat,
    colorscale = DENS_COLORSCALE,
    opacity = PATCH_OPACITY,
    showscale = TRUE,
    name = "Density on patch",
    colorbar = list(title = "Density")
  ) %>%
  add_trace(type="scatter3d", mode="lines", x=cKF$x, y=cKF$y, z=cKF$z, line=list(width=7), showlegend=FALSE) %>%
  add_trace(type="scatter3d", mode="lines", x=cA$x,  y=cA$y,  z=cA$z,  line=list(width=7), showlegend=FALSE) %>%
  add_trace(type="scatter3d", mode="lines", x=cB$x,  y=cB$y,  z=cB$z,  line=list(width=7), showlegend=FALSE)

if (SHOW_POINTS) {
  pts <- data %>%
    transmute(
      s = -normalized_ordinate,
      r = r_of_s(s),
      theta = THETA0 + normalized_abscissa / pmax(r, 1e-9),
      x3 = r * cos(theta),
      y3 = r * sin(theta),
      z3 = z_of_s(s),
      MP = MP
    )
  
  p <- p %>%
    add_trace(
      data = pts,
      type = "scatter3d",
      mode = "markers",
      x = ~x3, y = ~y3, z = ~z3,
      color = ~MP,
      colors = PALETTE_MP,
      marker = list(size = POINT_SIZE, opacity = 0.95),
      name = "MP"
    )
}

p <- p %>% layout(
  scene = list(
    aspectmode = "data",
    xaxis = list(title = "X"),
    yaxis = list(title = "Y"),
    zaxis = list(title = "Axial height z")
  ),
  title = "Prototype 3D"
)

p
