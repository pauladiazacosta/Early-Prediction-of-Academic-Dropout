install.packages("cluster")
install.packages("NbClust")
install.packages("ggplot2")
install.packages("writexl")
library(cluster)
library(NbClust)
library(ggplot2)
library(writexl)

names(data_v2) <- gsub("[^A-Za-z0-9_]", "_", names(data_v2))
names(data_v2) <- gsub("_+", "_", names(data_v2))
names(data_v2) <- gsub("^_|_$", "", names(data_v2))
names(data_v2)

data_v2$Marital_status = factor(data_v2$Marital_status, levels=c("1","2","3","4","5","6"))
levels(data_v2$Marital_status)
data_v2$Course=factor(data_v2$Course, levels=c("33","171","8014","9003","9070","9085","9119","9130","9147","9238","9254","9500","9556","9670","9773","9853","9991"))
levels(data_v2$Course)
data_v2$Daytime_evening_attendance=factor(data_v2$Daytime_evening_attendance, levels=c("1","0"))
levels(data_v2$Daytime_evening_attendance)
data_v2$Nacionality=factor(data_v2$Nacionality, levels=c("1","2","6","11","13","14","17","21","22","24","25","26","32","41","62","100","101","103","105","108","109"))
levels(data_v2$Nacionality)
data_v2$Mother_s_occupation=factor(data_v2$Mother_s_occupation, levels=c("1","2","3","4","5","6","9","10","11","12","14","18","19","26","27","29","30","34","35","36","37","38","39","40","41","42","43","44"))
levels(data_v2$Mother_s_occupation)
data_v2$Father_s_occupation=factor(data_v2$Father_s_occupation, levels=c("1","2","3","4","5","6","9","10","11","12","13","14","18","19","22","25","26","27","29","30","31","33","34","35","36","37","38","39","40","41","42","43","44"))
levels(data_v2$Father_s_occupation)
data_v2$Displaced=factor(data_v2$Displaced, levels=c("0","1"))
levels(data_v2$Displaced)
data_v2$Educational_special_needs=factor(data_v2$Educational_special_needs, levels=c("0","1"))
levels(data_v2$Educational_special_needs)
data_v2$Gender=factor(data_v2$Gender, levels=c("0","1"))
levels(data_v2$Gender)
data_v2$Scholarship_holder=factor(data_v2$Scholarship_holder, levels=c("0","1"))
levels(data_v2$Scholarship_holder)
data_v2$International=factor(data_v2$International, levels=c("0","1"))
levels(data_v2$International)
data_v2$Target=factor(data_v2$Target, levels=c("Dropout","Enrolled","Graduate"))
levels(data_v2$Target)

summary(data_v2[, 1:18])

set.seed(123)

n_muestra <- 1500
idx_muestra <- sample(1:nrow(data_v2), n_muestra)

daisy_sample <- cluster::daisy(
  x      = data_v2[idx_muestra, 1:18],
  metric = "gower",
  type   = list(ordratio = 1:18)
)

gower_dist_sample <- as.dist(daisy_sample)

indices <- c("silhouette", "dunn", "cindex", "frey", "mcclain")
k_por_indice <- sapply(indices, function(idx) {
  res <- NbClust(
    diss     = gower_dist_sample,
    distance = NULL,
    min.nc   = 2,
    max.nc   = 10,
    method   = "ward.D",
    index    = idx
  )
  as.numeric(res$Best.nc[1])   
})
k_por_indice

votos <- sort(table(k_por_indice), decreasing = TRUE)
votos
k_optimo <- as.numeric(names(votos)[1])
k_optimo

pam_sample <- cluster::pam(
  x    = daisy_sample,
  k    = k_optimo,        
  diss = TRUE      
)
pam_sample

data_v2$PAM_1500 <- NA
data_v2$PAM_1500[idx_muestra] <- pam_sample$clustering
table(data_v2$PAM_1500, useNA = "ifany")

plot(pam_sample, which = 2, main = "Silueta PAM (submuestra 1500, distancia Gower)")

clusplot(pam_sample,
         main  = "Clusplot PAM (submuestra 1500, distancia Gower)",
         lines = 0, shade = TRUE)

subsample_df <- data_v2[idx_muestra,]
table(subsample_df$Target, subsample_df$PAM_1500)

a<-ggplot(subsample_df) +
  aes(x = factor(PAM_1500), fill = Target) +
  geom_bar() +
  xlab("Cluster PAM (submuestra 1500)") +
  ylab("Número de estudiantes") +
  ggtitle("Distribución de Target por cluster PAM (submuestra 1500)")
a

ruta_salida <- "C:/Users/paula/Desktop/TFG_Paula/Datos/data_v2_PAM1500.csv"
write.csv(
  data_v2,
  file = ruta_salida,
  row.names = FALSE
)
ruta_salida <- "C:/Users/paula/Desktop/TFG_Paula/Datos/data_v3_PAM1500.xlsx"
write_xlsx(data_v2, ruta_salida)
