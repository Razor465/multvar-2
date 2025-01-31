library(factoextra)
library(readxl)
library(CCA)
library(rgl)
library(cluster)
library(fossil) # ��������� ��������

# ������� ����
data <- read.table('C:\\Users\\Razor\\Desktop\\����������� ��������\\������������ ����� ������������� �����\\lab1\\mult6.txt')

# ������ ������ �������
col = c('black', 'red', 'green', 'blue', 'orange', 
        'purple', 'yellow', 'brown', 'burlywood', 
        'deepskyblue', 'darkseagreen', 'deeppink', 
        'salmon', 'turquoise1', 'darkblue', 'darkred',
        'aquamarine', 'grey', 'chocolate', 'magenta')

# �������� �������� ������������� � ������ ������ (1 �������)

# ����� ��������, � = 3
km.res3 <- kmeans(data, 3, nstart = 25)

# ����� ��������, � = 5
km.res5 <- kmeans(data, 5, nstart = 25)

# ����� ������, � = 5
pam.res5 <- pam(data, 5)

# ����������� ���� � ������� ���������� 
d_eucl <- dist(data, 'euclidean')
d_mannh <- dist(data, 'manhattan')
d_max <- dist(data, 'maximum')

# �������� ����������
fit_eucl <- cmdscale(d_eucl, eig = TRUE, k = 2)
fit_mannh <- cmdscale(d_mannh, eig = TRUE, k = 2)
fit_max <- cmdscale(d_max, eig = TRUE, k = 2)

# �������� ������� ����������

# �������� �������
x_eucl <- fit_eucl$points[,1]
y_eucl <- fit_eucl$points[,2]
# k = 3
plot(x_eucl, y_eucl, col = col[km.res3$cluster], cex = 0.3)
# k = 5, centroids
plot(x_eucl, y_eucl, col = col[km.res5$cluster], cex = 0.3, main = '5 clusters, centroids')
# k = 5, medoids
plot(x_eucl, y_eucl, col = col[pam.res5$cluster], cex = 0.3, main = '5 clusters, medoids')

# ������������� �������
x_mannh <- fit_mannh$points[,1]
y_mannh <- fit_mannh$points[,2]
# k = 3
plot(x_mannh, y_mannh, col = col[km.res3$cluster], cex = 0.3, main = '3 clusters')
# k = 5, centroids
plot(x_mannh, y_mannh, col = col[km.res5$cluster], cex = 0.3, main = '5 clusters, centroids')
# k = 5, medoids
plot(x_mannh, y_mannh, col = col[pam.res5$cluster], cex = 0.3, main = '5 clusters, medoids')

# ����������� �������
x_max <- fit_max$points[,1]
y_max <- fit_max$points[,2]
# k = 3
plot(x_max, y_max, col = col[km.res3$cluster], cex = 0.3, main = '3 clusters')
# k = 5, centroids
plot(x_max, y_max, col = col[km.res5$cluster], cex = 0.3, main = '5 clusters, centroids')
# k = 5, medoids
plot(x_max, y_max, col = col[pam.res5$cluster], cex = 0.3, main = '5 clusters, medoids')

# 2 �������

# ������� ����
data1 <- read_excel('data.xlsx')
rows <- t(data1[,1])
data1 <- data1[,-1]

# ����������� � ����������

data1 <- as.data.frame(scale(data1))
row.names(data1) <- rows

# ����� ������, k = 4
pam.res41 <- pam(data1, 4)

# ����� ������, k = 9
pam.res91 <- pam(data1, 9)

# ����� ��������, k = 9
km.res91 <- kmeans(data1, 9, nstart = 25)

# ����������� ���� � ������� ���������� 
d_eucl1 <- dist(data1, 'euclidean')
d_mannh1 <- dist(data1, 'manhattan')
d_max1 <- dist(data1, 'maximum')

# �������� ����������
fit_eucl1 <- cmdscale(d_eucl1, eig = TRUE, k = 2)
fit_mannh1 <- cmdscale(d_mannh1, eig = TRUE, k = 2)
fit_max1 <- cmdscale(d_max1, eig = TRUE, k = 2)

# �������� ������� ����������

# �������� �������
x_eucl1 <- fit_eucl1$points[,1]
y_eucl1 <- fit_eucl1$points[,2]
# k = 4, medoids
plot(x_eucl1, y_eucl1, col = col[pam.res41$cluster], cex = 1, main = '4 clusters, medoids')
# k = 9, medoids
plot(x_eucl1, y_eucl1, col = col[pam.res91$cluster], cex = 1, main = '9 clusters, medoids')
# k = 9, centroids
plot(x_eucl1, y_eucl1, col = col[km.res91$cluster], cex = 1, main = '9 clusters, centroids')

# ������������� �������
x_mannh1 <- fit_mannh1$points[,1]
y_mannh1 <- fit_mannh1$points[,2]
# k = 4, medoids
plot(x_mannh1, y_mannh1, col = col[pam.res41$cluster], cex = 1, main = '4 clusters, medoids')
# k = 9, medoids
plot(x_mannh1, y_mannh1, col = col[pam.res91$cluster], cex = 1, main = '9 clusters, medoids')
# k = 9, centroids
plot(x_mannh1, y_mannh1, col = col[km.res91$cluster], cex = 1, main = '9 clusters, centroids')

# ����������� �������
x_max1 <- fit_max1$points[,1]
y_max1 <- fit_max1$points[,2]
# k = 4, medoids
plot(x_max1, y_max1, col = col[pam.res41$cluster], cex = 1, main = '4 clusters, medoids')
# k = 9, medoids
plot(x_max1, y_max1, col = col[pam.res91$cluster], cex = 1, main = '9 clusters, medoids')
# k = 9, centroids
plot(x_max1, y_max1, col = col[km.res91$cluster], cex = 1, main = '9 clusters, centroids')

#
# 2 �������
#

cca_cmd_plot <- function(data, k, cl_method, dist_function){
  if(cl_method == 'centroid'){
    clust <- kmeans(data, k, nstart = 25)
  } else if(cl_method == 'medoid'){
    clust <- pam(data, k)
  }
  d <- dist(data, method = dist_function)
  cl <- clust$cluster
  k <- length(levels(as.factor(cl)))
  data_cms <- cmdscale(d, k = 20, eig = TRUE)$points
  n <- nrow(data_cms)
  C <- matrix(data = as.numeric(rep(cl, k) == rep(1:k, each = n)), ncol = k, nrow = n)
  cc_res <- rcc(data_cms,C,0.1,0.1)
  # ������� ���������� ������ ���� ���������� ���������
  plot(cc_res$scores$xscores[,1:2], col = col[cl], cex=0.2, main = c(k, cl_method))
}

# �������� �������
cca_cmd_plot(data, 3, 'centroid', 'euclidean')
cca_cmd_plot(data, 5, 'centroid', 'euclidean')
cca_cmd_plot(data, 5, 'medoid', 'euclidean')

# ������������� �������
cca_cmd_plot(data, 3, 'centroid', 'manhattan')
cca_cmd_plot(data, 5, 'centroid', 'manhattan')
cca_cmd_plot(data, 5, 'medoid', 'manhattan')

# ����������� �������
cca_cmd_plot(data, 3, 'centroid', 'maximum')
cca_cmd_plot(data, 5, 'centroid', 'maximum')
cca_cmd_plot(data, 5, 'medoid', 'maximum')
