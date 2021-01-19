library(tidyverse)
library(factoextra)
library(cluster)

df <- readr::read_csv("main_data.csv", col_names = c("College.Major", "Starting.Median.Salary", 
                                                     "Mid.Career.Median.Salary", "Career.Percent.Growth", 
                                                     "Percentile.10", "Percentile.25", "Percentile.75", 
                                                     "Percentile.90"), skip=1)

head(df)

k.means.data <- df %>% 
    select(c("Starting.Median.Salary", "Mid.Career.Median.Salary", "Percentile.10", "Percentile.90")) %>%
    scale()

fviz_nbclust(k.means.data, kmeans , method = "wss")

fviz_nbclust(k.means.data, kmeans , method = "silhouette")

gap_stat <- cluster::clusGap(k.means.data, FUNcluster = kmeans, nstart=25, K.max=10, B=50)

fviz_gap_stat(gap_stat)

num_cluster <- 3
set.seed(3)
k_means <- kmeans(k.means.data, centers=num_cluster, iter.max = 15, nstart = 25)

df.labelled <- df %>% 
    mutate(clusters = k_means$cluster)

ggplot(df.labelled, aes(Mid.Career.Median.Salary, Starting.Median.Salary, color=clusters))+
    geom_point()+
    xlab("Median Salary mid career")+
    ylab("Median salary start career")+
    scale_x_continuous(labels= scales::dollar)+
    scale_y_continuous(labels= scales::dollar)
    
df_perc <- df.labelled%>% select(c("College.Major", "Percentile.10", "Percentile.25", "Mid.Career.Median.Salary", "Percentile.75", "Percentile.90", "clusters")) %>%
    gather(key = "Percentile", value="Salary", -c(College.Major, clusters))%>%
    mutate(Percentile=factor(Percentile,levels=c('Percentile.10','Percentile.25',
                                                 'Mid.Career.Median.Salary',
                                                 'Percentile.75','Percentile.90')))

ggplot(df_perc[df_perc$clusters==1,], aes(x=Percentile,y=Salary, group=College.Major, color=College.Major, order=Salary)) +
    geom_point() +
    geom_line() +
    ggtitle('Cluster 1:  The Liberal Arts') +
    theme(axis.text.x = element_text(size=7, angle=25)) 

ggplot(df_perc[df_perc$clusters==2,], aes(x=Percentile,y=Salary, group=College.Major, color=College.Major, order=Salary)) +
    geom_point() +
    geom_line() +
    ggtitle('Cluster 2:  The Goldilocks') +
    theme(axis.text.x = element_text(size=7, angle=25)) 

ggplot(df_perc[df_perc$clusters==3,], aes(x=Percentile,y=Salary, group=College.Major, color=College.Major, order=Salary)) +
    geom_point() +
    geom_line() +
    ggtitle('Cluster 2: The Over Achievers') +
    theme(axis.text.x = element_text(size=7, angle=25))

arrange(df.labelled,desc(Career.Percent.Growth))
















