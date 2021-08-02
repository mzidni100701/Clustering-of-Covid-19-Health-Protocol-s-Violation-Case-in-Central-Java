library(readxl)
library(factoextra)
library(ggplot2)
library (tidyverse)
library (cluster)
data <- read_excel("C:/Users/HP/Downloads/data belum scaling.xlsx")
View(data)
#ganti nama kolom
colnames(data)=c("dirawat", "sembuh", "meninggal","L","P","PNS","Pelajar","Swasta","<20","20-39",">40")
view(data)
data = subset(data, select = -c(, kab))
rownames(datafix)=c('BANJARNEGARA', 'BANYUMAS', 'BATANG', 'BLORA', 'BOYOLALI', 'BREBES', 'CILACAP', 'DEMAK', 'GROBOGAN', 'JEPARA', 'KARANGANYAR', 'KEBUMEN', 'KENDAL', 'KLATEN', 'KOTA MAGELANG', 'KOTA PEKALONGAN', 'KOTA SALATIGA', 'KOTA SEMARANG', 'KOTA SURAKARTA', 'KOTA TEGAL', 'KUDUS', 'MAGELANG', 'PATI', 'PEKALONGAN', 'PEMALANG', 'PURBALINGGA', 'PURWOREJO', 'REMBANG', 'SEMARANG', 'SRAGEN', 'SUKOHARJO', 'TEGAL', 'TEMANGGUNG', 'WONOGIRI', 'WONOSOBO')
#persiapan data
data.numerik<-data[2:12]
view(data.numerik)
datafix<- data[2:12]
datafix<-scale(datafix)
view(datafix)
#menentukan K
fviz_nbclust(datafix, kmeans, method = "wss")
fviz_nbclust(datafix, kmeans, method = "silhouette")
# metode gap statistik
set.seed(123)
gap_stat <- clusGap(datafix, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50) # metode gap statistic
fviz_gap_stat(gap_stat)
#kmeans
final <- kmeans(datafix, 2, nstart = 25)
print(final)
final
fviz_cluster(final, data=datafix)

library(NbClust)
fviz_nbclust(datafix, kmeans, method = c("silhouette", "wss", "gap_stat"))
# Elbow method
fviz_nbclust(datafix, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(datafix, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(datafix, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")
#data cluster ke tabel
finall=data.frame(datafix, final$cluster)
#ringkasan cluster
datafix %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
View(finall)
library (writexl)
write_xlsx(finall, "C:/Users/HP/Downloads/clusterfinallfixrevisi1.xlsx")

