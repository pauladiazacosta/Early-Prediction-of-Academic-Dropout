install.packages("compareGroups")
library(compareGroups)

names(data_v2_PAM1500) <- gsub("[^A-Za-z0-9_]", "_", names(data_v2_PAM1500))
names(data_v2_PAM1500) <- gsub("_+", "_", names(data_v2_PAM1500))
names(data_v2_PAM1500) <- gsub("^_|_$", "", names(data_v2_PAM1500))
names(data_v2_PAM1500)

data_v2_PAM1500$Marital_status = factor(data_v2_PAM1500$Marital_status, levels=c("1","2","3","4","5","6"))
levels(data_v2_PAM1500$Marital_status)
data_v2_PAM1500$Course=factor(data_v2_PAM1500$Course, levels=c("33","171","8014","9003","9070","9085","9119","9130","9147","9238","9254","9500","9556","9670","9773","9853","9991"))
levels(data_v2_PAM1500$Course)
data_v2_PAM1500$Daytime_evening_attendance=factor(data_v2_PAM1500$Daytime_evening_attendance, levels=c("1","0"))
levels(data_v2_PAM1500$Daytime_evening_attendance)
data_v2_PAM1500$Nacionality=factor(data_v2_PAM1500$Nacionality, levels=c("1","2","6","11","13","14","17","21","22","24","25","26","32","41","62","100","101","103","105","108","109"))
levels(data_v2_PAM1500$Nacionality)
data_v2_PAM1500$Mother_s_occupation=factor(data_v2_PAM1500$Mother_s_occupation, levels=c("1","2","3","4","5","6","9","10","11","12","14","18","19","26","27","29","30","34","35","36","37","38","39","40","41","42","43","44"))
levels(data_v2_PAM1500$Mother_s_occupation)
data_v2_PAM1500$Father_s_occupation=factor(data_v2_PAM1500$Father_s_occupation, levels=c("1","2","3","4","5","6","9","10","11","12","13","14","18","19","22","25","26","27","29","30","31","33","34","35","36","37","38","39","40","41","42","43","44"))
levels(data_v2_PAM1500$Father_s_occupation)
data_v2_PAM1500$Displaced=factor(data_v2_PAM1500$Displaced, levels=c("0","1"))
levels(data_v2_PAM1500$Displaced)
data_v2_PAM1500$Educational_special_needs=factor(data_v2_PAM1500$Educational_special_needs, levels=c("0","1"))
levels(data_v2_PAM1500$Educational_special_needs)
data_v2_PAM1500$Gender=factor(data_v2_PAM1500$Gender, levels=c("0","1"))
levels(data_v2_PAM1500$Gender)
data_v2_PAM1500$Scholarship_holder=factor(data_v2_PAM1500$Scholarship_holder, levels=c("0","1"))
levels(data_v2_PAM1500$Scholarship_holder)
data_v2_PAM1500$International=factor(data_v2_PAM1500$International, levels=c("0","1"))
levels(data_v2_PAM1500$International)
data_v2_PAM1500$Target=factor(data_v2_PAM1500$Target, levels=c("Dropout","Enrolled","Graduate"))
levels(data_v2_PAM1500$Target)

data_v2_PAM1500$PAM_1500 <- factor(
  data_v2_PAM1500$PAM_1500,
  levels = sort(unique(data_v2_PAM1500$PAM_1500)),
  labels = paste("Cluster", sort(unique(data_v2_PAM1500$PAM_1500)))
)

formula_clusters <- PAM_1500 ~ . - Target - ward_D - single

res_clusters <- compareGroups(
  formula = formula_clusters,
  data    = data_v2_PAM1500
)
res_clusters

tabla_clusters <- createTable(
  res_clusters,
  show.all = TRUE 
)
print(
  tabla_clusters,
  which.table   = "descr",
  header.labels = c(
    "all"       = "Total",
    "p.overall" = "p global"
  )
)

export2xls(
  x            = tabla_clusters,
  file         = "Tabla1_Clusters_PAM.xlsx",
  header.labels = c(
    "all"       = "Total",
    "p.overall" = "p global"
  )
)

setwd("C:/Users/paula/Desktop/TFG_Paula")
dir.create("figuras_clusters_PAM", showWarnings = FALSE)

plot(
  res_clusters,
  bivar = TRUE,
  file  = "figuras_clusters_PAM",
  type  = "png",
  perc  = TRUE
)
