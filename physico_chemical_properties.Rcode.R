#Fundamental libraries for EDA----
library(tidyverse)# for summarizing the data
library(ggsci)# for color palette
library(ggpubr) #for theme publication
#Assigning names to the columns in the dataset----
col_headers <- c(
  "target", "alcohol", "malic_acid", "ash", 
  "alkalinity", "magnesium", "total_phenols",
  "flavonoids", "non_flav", "proanthocyanine",
  "color_intensity", "hue", "distilled_wines", "proline")
#importing the data set----
wine <- read_delim(
  "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
  col_names = FALSE, delim = ",", show_col_types = FALSE) %>% 
  set_names(col_headers) %>%  # set the variable names
  mutate(across(everything(), as.numeric)) %>%  # convert all the data to numeric
  mutate(target = as.factor(target)) %>%   # convert target to factor
  print()
#checking the structure of the dataset----
dim(wine)
glimpse(wine)
summary(wine)
# check if there is NA----
is.na(wine)
which(is.na(wine))           
#check for the duplicate column
 duplicated(wine)
 #Reordering of the barchart
 reordering <- wine %>%
   count(target, name="count")%>%
   mutate(target=case_when(
     target=="1"~"0",
     target=="2"~"1",
     target=="3"~"2"
   ))%>%
   mutate(target=reorder(target,-count))%>%
   ggplot(aes(x=target,y=count))+
   geom_bar(stat="identity",width=0.5,
            fill="#d0533d")+
   labs(x="Wine type",
        title="Value counts of the target variable")
 reordering

barchart
# plotting of the histogram
hist <- wine%>% ggplot()+
  geom_histogram(mapping=aes(magnesium),binwidth=1,color="black", fill="blue")+
  labs(title="Distribution of Magnesium",
       caption="Data: wine data set by Monsurat", fill="target")+
  theme_classic()
hist
#with density(did nt work)
dense <- wine %>% ggplot()+
  geom_histogram(mapping=aes(magnesium),binwidth=1,color="black", fill="blue")+
  labs(title="value counts of the target variable",
       caption="Data: wine data set by Monsurat", fill="target")+
  theme_classic()+
  geom_density(alpha=.2, fill="transparent")
dense

# Installing GGally package  (for pairplot)
install.packages("GGally")
library(GGally)
pairplot <- wine[1:7]%>% ggpairs() # I had to separate the data frame into two
pairplot
pairplot2 <- wine[8:14]%>% ggpairs()
pairplot2
# Boxplot for target and proline----
box <- wine %>% ggplot(aes(x=target,y=proline,fill=target))+
  geom_boxplot()+
  labs(title="Boxplot for target and proline")+
  theme_classic()
box
# Boxplot for target and flavonoids----
flavonoids <- wine %>% ggplot(aes(x=target,y=flavonoids,fill=target))+
  geom_boxplot()+
  labs(title="Boxplot for target and flavonoids")+
  theme_classic()
flavonoids
#Relationship between proline, flavonoids and target----
plot <- wine %>% ggplot()+
  geom_point(mapping=aes(x=proline,y=flavonoids,color=target))+
  labs(title="Relationship between proline, flavonoids and target") 
plot
#correlation matrix 
install.packages("ggcorrplot")
library(ggcorrplot)
# I had to convert the target variable into a numeric

wine[["target"]] <- as.numeric(wine[["target"]])
wine

#calculate the correlation measures
#rounding to one decimal
correlation_matrix <- round(cor(wine),1)
correlation_matrix
#computing correlation matrix with p-values
corrp.mat <- cor_pmat(wine)
# visualizing the correlation matrix using square and circle methods
correlation_matrix1 <- ggcorrplot(correlation_matrix, method="square",lab=TRUE)
ggcorrplot(correlation_matrix, method="circle")
#using heatmap
col <- colorRampPalette(c("blue","white","red"))(20)
heatmap(correlation_matrix, col=col, symm=TRUE)

