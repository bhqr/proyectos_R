tien <- read.csv("C:/Users/o1/Downloads/Data_Tiendas.csv", head=T)

tien_sc <- scale(tien[,2:3])
tien_sc <- cbind(tien,tien_sc)

dist_tien <- dist(tien_sc[,4:5], method = 'euclidian')
hc_tien <- hclust(dist_tien, 'complete')
hc_tien
plot(hc_tien)

rect.hclust(hc_tien,k=8,border=2:10)
rect.hclust(hc_tien,h=5,border=2:10)



clusters <- kmeans(tien_sc[,4:5],6) #Proponer un k grupos
plot(tien_sc[,4:5],col=clusters$cluster)

install.packages("factoextra")
library(factoextra)

fviz_cluster(clusters, data = tien_sc[,4:5])


wss <- sum(kmeans(tien_sc[,4:5],centers=1)$withinss)
for(i in 2:15) wss[i]<- sum(kmeans(tien_sc[,4:5],centers=i)$withinss)
plot(1:15, wss, type="b", xlab = '# de clusters', ylab='Suma de cuadrados')
### en el gráfico se observa que pueden ser desde 5 hasta 8
library(cluster)
tienvf<-tien[,2:3]
pr4 <- pam(tienvf, 4)
str(si <- silhouette(pr4))
(ssi <- summary(si))
plot(si) # silhouette plot
plot(si, col = c("red", "green", "blue", "purple"))# with cluster-wise coloring

si2 <- silhouette(pr4$clustering, dist(tienvf, "canberra"))
summary(si2) # has small values: "canberra"'s fault
plot(si2, nmax= 800, cex.names=0.4)

op <- par(mfrow= c(3,2), oma= c(0,0, 3, 0),
          mgp= c(1.6,.8,0), mar= .1+c(4,2,2,2))

for(k in 4:8)
  plot(silhouette(pam(tienvf, k=k)), main = paste("k = ",k), do.n.k=FALSE)
mtext("PAM(tienvf) as in Kaufman & Rousseeuw, p.101",
      outer = TRUE, font = par("font.main"), cex = par("cex.main")); frame()

## the same with cluster-wise colours:
c6 <- c("tomato", "forest green", "dark blue", "purple2", "goldenrod4", "gray20","red", "green")
for(k in 4:8)
  plot(silhouette(pam(tienvf, k=k)), main = paste("k = ",k), do.n.k=FALSE,
       col = c6[1:k],border=NA)
par(op)

## se observa que la clusterización con menores valores negativos se logra con el k=6

clusters <- kmeans(tien_sc[,4:5],6) #Proponer un k grupos
tien_sc$Grupo <- as.factor(clusters$cluster)
## En la tabla tien_sc se puede observar a que grupo pertenece cada elemento
##PREGUNTA 1: Los grupos a formar son 6
##PREGUNTA 2: Los grupos tienen 294,347,315,287,130,127 elementos respectivamente;
##además se podrá observar una columna adicional donde está el grupo al que pertenece, en 
## la tabla tien_sc al finalizar de correr el código.
