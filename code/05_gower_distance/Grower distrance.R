install.packages("tidyr")
install.packages("factoextra")
install.packages("ggplot2")
install.packages("cluster")
install.packages("dplyr")
install.packages("NbClust")
library(cluster)
library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)
library(NbClust)

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

daisy_data_v2 <- cluster::daisy(
  x = data_v2[, 1:18],
  metric = "gower"
)

gower_dist <- as.dist(daisy_data_v2)

methods <- c("ward.D", "single")
list_hclust <- lapply(methods, function(m) hclust(gower_dist, method = m))
names(list_hclust) <- methods

set.seed(123)

indices <- c("silhouette", "dunn", "cindex", "frey", "mcclain")
k_por_indice <- sapply(indices, function(idx) {
  res <- NbClust(
    diss     = gower_dist,
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

data_v2$ward.D<-cutree(
  tree = list_hclust[["ward.D"]],
  k=k_optimo
)
data_v2$single<-cutree(
  tree = list_hclust[["single"]],
  k=k_optimo
)

long_v2 <- pivot_longer(
  data_v2,
  cols = c(ward.D, single),
  names_to = "Method",
  values_to = "Cluster"
)
require(ggplot2)

table(long_v2[,c("Target","Cluster","Method")])

p<-ggplot(long_v2)+
  aes(x = factor(Cluster),fill = Target)+
  geom_bar()+
  facet_grid(~Method)+
  ggtitle("Diagrama de barras del conjutno de datos ordenados por clusters")
p

