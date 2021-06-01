

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

#more correlated variables (over threshold)
more_corelated_pairs<-function(threshold=.9, sing="P"){ #"P"=positive, "N"=negative
  
  switch (sing,
    "P" = {
      cor_df <- arrayInd(which(cor_matrix > threshold), dim(cor_matrix), useNames = T) %>% 
        as.data.frame()
      cor_df <- cor_df[-which(cor_df[1]==cor_df[2]),]
    },
    "N" = {
      cor_df <- arrayInd(which(cor_matrix < -threshold), dim(cor_matrix), useNames = T) %>% 
        as.data.frame()
    }
  )

  if(nrow(cor_df) == 0){
    colnames(cor_df) <- c("Variable1","Variable2")
    cor_df[nrow(cor_df)+1, ] <- c("Empty", "Empty")
    return(cor_df)
  }

  rownames(cor_df) <- NULL

  cor_df$Variable1 <- rownames(cor_matrix)[cor_df[,1]]
  cor_df$Variable2 <- colnames(cor_matrix)[cor_df[,2]]
  
  for (i in 1:nrow(cor_df)) {
    cor_df$Coef_correlacion[i] <- cor_matrix[cor_df$row[i],cor_df$col[i]]
  }
  
  #deleting repeated pairs
  repeated_pairs<-c()
  cor_df_final<-cor_df[FALSE,c("Variable1", "Variable2",  "Coef_correlacion")]

  for (v in 1:nrow(cor_df) ) {

    if(! paste(cor_df$Variable1[v], cor_df$Variable2[v], sep="-") %in% repeated_pairs){

      v1 <- cor_df$Variable1[v]
      v2 <- cor_df$Variable2[v]
    
      repeated_pairs<-c(repeated_pairs,paste(v1, v2, sep="-"))
      repeated_pairs<-c(repeated_pairs,paste(v2, v1, sep="-"))
    
      cor_df_final[nrow(cor_df_final)+1, ] <- c(v1, v2, cor_df$Coef_correlacion[v])
    }
  }
  return(cor_df_final)
}

positive_correlated<-more_corelated_pairs()
negative_correlated<-more_corelated_pairs(sing="N")



#somme relevant insigts

#como batean los jugadores por posiciones (SS pueden estar entre los mejores)
p_playerByPos <- ggplot(mlb_Hitting, aes(x = G, y =	AVG*1000, color=POS, 
                                         size=SO, label=PLAYER)) + 
  geom_point() + xlab('Juegos') + ylab('Average') + ggtitle(label = "G vs AVG")

p_playerByPosNames <- ggplot(mlb_Hitting, aes(x = G, y = AVG*1000, label=PLAYER)) + 
  geom_text(size=3, col="blue") + xlab('Juegos') + ylab('Average')

#como impulsan por posiciones (SS no deben estar entre los mejores)
p_impulseByPos <- ggplot(mlb_Hitting, aes(x = G, y = RBI, color=POS, 
                                          alpha=1.0, size=SO, label=PLAYER)) +
  geom_point() + xlab('Juegos') + ylab('Carreras Impulsadas')

p_impulseByPosNames <- ggplot(mlb_Hitting, aes(x = G, y =RBI, label=PLAYER)) + 
  geom_text(size=3, col="blue") + xlab('Juegos') + ylab('Carreras Impulsadas')

#relación de home runs y juegos
p_hrByG <- ggplot(mlb_Hitting, aes(x = G, y = HR, color=POS, alpha=1.0, 
                                   size=RBI, label=PLAYER)) +
  geom_point() + xlab('Juegos') + ylab('Home runs')

p_hrByGNames <- ggplot(mlb_Hitting, aes(x = G, y = HR, label=PLAYER)) + 
  geom_text(size=3, col="blue") + xlab('Juegos') + ylab('Home runs')

#como roban los bateadores las bases por posiciones (SS suelen estar entre los mejores)
p_sbByPos <- ggplot(mlb_Hitting, aes(x = G, y =SB, color=POS, alpha=1.0, 
                                     size=R, label=PLAYER)) +
  geom_point() + xlab('Juegos') + ylab('Bases Robadas')

p_sbByPosNames <- ggplot(mlb_Hitting, aes(x = G, y = SB, label=PLAYER)) + 
  geom_text(size=3, col="blue") + xlab('Juegos') + ylab('Bases Robadas')

#cuanto batea un jugador comparado con la cantidad de veces al bate
p_avgByAb <- ggplot(mlb_Hitting, aes(x = AB , y = AVG*1000, color=H, size=SO, label=PLAYER)) + 
  geom_point() + scale_color_gradient(low="lightblue", high="darkblue") + 
  xlab('Veces al bate') + ylab('Average')

p_avgByAbNames <- ggplot(mlb_Hitting, aes(x = AB, y = AVG*1000, label=PLAYER)) + 
  geom_text(size=3, col="blue") + xlab('Veces al bate') + ylab('Average')











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

#fist component
barplot(-1*pca$rotation[,1], las=2, col="blue", main = "PCA1") #jugadores que más batean y que más impulsan

players<-mlb_Hitting[,1]
players[order(pca$x[,1])][1:10]

qplot(-pca$x[,1],mlb_Hitting$G,label=players) +
  labs(title="Performance", x="PC1", y="") + theme_bw() +
  theme(legend.position = "bottom", panel.border = element_blank()) +
  geom_text(size=3, hjust=0, vjust=0, check_overlap = FALSE)

#segunda componente
barplot(-1*pca$rotation[,2], las=2, col="blue", main = "PCA2" ) #
players[order(pca$x[,2])][1:10]

qplot(-pca$x[,2],mlb_Hitting$G,label=players) +
  labs(title="Performance", x="PC1", y="") + theme_bw() +
  theme(legend.position = "bottom", panel.border = element_blank()) +
  geom_text(size=3, hjust=0, vjust=0, check_overlap = FALSE)

#tercera componente
barplot(-1*pca$rotation[,3], las=2, col="blue", main = "PCA3" ) #
players[order(pca$x[,3])][1:10]

qplot(-pca$x[,3],mlb_Hitting$G,label=players) +
  labs(title="Performance", x="PC1", y="") + theme_bw() +
  theme(legend.position = "bottom", panel.border = element_blank()) +
  geom_text(size=3, hjust=0, vjust=0, check_overlap = FALSE)

pairs(pca$rotation[,1:3], panel = function(x,y) text(x,y, labels = names(mlb_Hitting)))

# primeras 2 componentes
qplot(-pca$x[,1],-pca$x[,2],label=players) + labs(title="PCA", x="PC1", y="PC2") +
  theme_bw() + theme(legend.position="bottom", panel.border = element_blank()) +
  geom_text(size=3, hjust=0, vjust=0, check_overlap = FALSE) #las primeras 2 componentes son independientes

# PCA Variable Factor Map
library(FactoMineR)
result <- PCA(mlb_Hitting[,3:18]) # graphs generated automatically

##### Factor analysis: MLE ####
# Note: MLE always scales the data.

# Number of Factors to Extract
library(nFactors)
ev <- eigen(cor(mlb_Hitting[,3:18])) # get eigenvalues
ap <- parallel(subject=nrow(mlb_Hitting[,3:18]),var=ncol(mlb_Hitting[,3:18]),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# with varimax rotation
AF.varimax <- factanal(mlb_Hitting[,3:18], factors = 3, rotation="varimax",  method="mle",
                       scores="regression", lower = 0.1)
print(AF.varimax,digits=5,cutoff=.22,sort=T)

loadings <- AF.varimax$loadings
loadings
plot(loadings, type="n") #blank plot to add names in next line
text(loadings,labels=names(mlb_Hitting[,3:18]),
     cex=.8, col = "blue") # add variable names

par(mfrow=c(2,2))
barplot(AF.varimax$loadings[,1], names=F, las=2, col=rainbow(9), ylim = c(-1, 1))
barplot(AF.varimax$loadings[,2], las=2, col=rainbow(9), ylim = c(-1, 1))
barplot(AF.varimax$loadings[,3], las=2, col=rainbow(9), ylim = c(-1, 1))
dev.off()

## Varianza específica
VS <- diag(AF.varimax$uniquenesses)
VS
diag(VS)
mean(1-diag(VS)/diag(cor(mlb_Hitting[,3:18])))

# varianza compartida (Comunalidad)
VC <-1-diag(VS)
VC

#factor 1: jugadores de contacto
qplot(AF.varimax$scores[,1],mlb_Hitting$G,label=players) +
  labs(title="Performance", x="FACTOR1. Jugadores de contacto", y="") + theme_bw() +
  theme(legend.position = "bottom", panel.border = element_blank()) +
  geom_text(size=3, hjust=0, vjust=0, check_overlap = FALSE)

#factor 2: jugadores de poder
qplot(AF.varimax$scores[,2],mlb_Hitting$G,label=players) +
  labs(title="Performance", x="FACTOR2. Jugadores de poder", y="") + theme_bw() +
  theme(legend.position = "bottom", panel.border = element_blank()) +
  geom_text(size=3, hjust=0, vjust=0, check_overlap = FALSE)

#factor 3: jugadores oportunos
qplot(AF.varimax$scores[,3],mlb_Hitting$G,label=players) +
  labs(title="Performance", x="FACTOR3. Jugadores oportunos", y="") + theme_bw() +
  theme(legend.position = "bottom", panel.border = element_blank()) +
  geom_text(size=3, hjust=0, vjust=0, check_overlap = FALSE)

# Scores
plot(AF.varimax$scores[,1],AF.varimax$scores[,2],xlab="First factor",ylab="Second factor",
     main="Scores",pch=19,col="blue")

plot(AF.varimax$scores[,1],AF.varimax$scores[,2],xlab="First factor",ylab="Second factor",
     main="Scores", type="n")
text(AF.varimax$scores[,c(1,2)],labels=mlb_Hitting[,1],pos = 4, col="blue")


# Plot the scatter plot between the loadings:
qplot(AF.varimax$loadings[,1], AF.varimax$loadings[,2], label="") +
  labs(title="First two loadings", x="L1", y="L2") +
  geom_text(size=3, hjust=-.5, vjust=-.3)
# nonlinear relationship between players in this 2D-factor space



