

##############################
#     Analyzing data        #
##############################


### LIBRARIES ###
library(dplyr)
library(ggplot2)
library(reshape)
library(corrplot)
library(stringr)



### EXPLORATORY DATA ANALYSIS ###

# Loading csv files
mlb_Hitting<-readRDS("data_preprocessed.rds")

## Univariate analysis ##

#using ggplot2
meltData <- melt(mlb_Hitting)
bxp <- ggplot(meltData, aes(factor(variable), value)) + 
  geom_boxplot(col="blue", outlier.colour="red", fill="lightblue") + 
  facet_wrap(~variable, ncol = 6, scales="free") +
  ggtitle("") + xlab("") + ylab("") +
  theme_minimal() + 
  theme(axis.text=element_text(size = rel(.5)), axis.title=element_text(size=rel(1),face="italic"), 
        plot.title = element_text(hjust = 0.5))     

hist <- ggplot(meltData, aes(x = value)) + 
  geom_histogram(col="blue", fill="lightblue" , aes(y=..density..), bins = 30) + 
  geom_density(col = "darkred", fill="lightpink", alpha=.3 )+ 
  facet_wrap(~variable, ncol = 6, scales="free") + 
  ggtitle("") + xlab("") + ylab("") +
  theme_minimal() + 
  theme(axis.text=element_text(size = rel(.5)), axis.title=element_text(size=rel(1),face="italic"), 
        plot.title = element_text(hjust = 0.5))


## Bivariate analysis ##

#correlations
cor_matrix <- cor(mlb_Hitting[,3:18])

#more correlated variables (over .9)
k <- arrayInd(which(cor_matrix > .9), dim(cor_matrix))
a <- rownames(cor_matrix)[k[,1]]
b <- colnames(cor_matrix)[k[,2]]

more_correlated <- paste(a, b, sep="-")
more_correlated <- more_correlated[more_correlated %>% str_extract("^[:alnum:]+") != 
                                     more_correlated %>% str_extract("[:alnum:]+$")]



#somme relevant insigts

#como batean los jugadores por posiciones (SS pueden estar entre los mejores)
p_playerByPos <- ggplot(mlb_Hitting, aes(x = G, y =	AVG*1000, color=POS, 
                                         size=SO, label=PLAYER)) + 
  geom_point() + xlab('Número de juegos') + ylab('AVG') + ggtitle(label = "G vs AVG")

p_playerByPosNames <- ggplot(mlb_Hitting, aes(x = G, y = AVG*1000, label=PLAYER)) + 
  geom_text(size=3, col="blue")

#como impulsan por posiciones (SS no deben estar entre los mejores)
p_impulseByPos <- ggplot(mlb_Hitting, aes(x = G, y = RBI, color=POS, 
                                          alpha=1.0, size=SO, label=PLAYER)) +
  geom_point() + xlab('Número de juegos') + ylab('RBI')

p_impulseByPosNames <- ggplot(mlb_Hitting, aes(x = G, y =RBI, label=PLAYER)) + 
  geom_text(size=3, col="blue")

#relación de home runs y juegos
p_hrByG <- ggplot(mlb_Hitting, aes(x = G, y = HR, color=POS, alpha=1.0, 
                                   size=RBI, label=PLAYER)) +
  geom_point() + xlab('Número de juegos') + ylab('HR')

p_hrByGNames <- ggplot(mlb_Hitting, aes(x = G, y = HR, label=PLAYER)) + 
  geom_text(size=3, col="blue")

#como roban los bateadores las bases por posiciones (SS suelen estar entre los mejores)
p_sbByPos <- ggplot(mlb_Hitting, aes(x = G, y =SB, color=POS, alpha=1.0, 
                                     size=R, label=PLAYER)) +
  geom_point() + xlab('Número de juegos') + ylab('SB')

p_sbByPosNames <- ggplot(mlb_Hitting, aes(x = G, y = SB, label=PLAYER)) + 
  geom_text(size=3, col="blue")

#cuanto batea un jugador comparado con la cantidad de veces al bate
p_avgByAb <- ggplot(mlb_Hitting, aes(x = AB , y = AVG*1000, color=H, size=SO, label=PLAYER)) + 
  geom_point() + scale_color_gradient(low="lightblue", high="darkblue") + 
  xlab('AB') + ylab('AVG(%)')

p_avgByAbNames <- ggplot(mlb_Hitting, aes(x = AB, y = AVG*1000, label=PLAYER)) + 
  geom_text(size=3, col="blue")



### MULTIVARIATE ANALYSIS ### 

# PCA
pca <- prcomp(mlb_Hitting[,3:18], scale=TRUE)
summary(pca)

screeplot(pca, main='scatterplot',col='blue',type='barplot',pch=19)
plot(pca,type="lines") # scree plot

eigen(cor_matrix) #the same, with numeric format
eigen(cor_matrix)$values #eigenvalues denote variances
eigen(cor_matrix)$vectors %>% round(2) #eigenvectors denote loadings
cumsum(pca$sdev^2)/dim(mlb_Hitting)[2]

biplot(pca)

pca$rotation[,1]



