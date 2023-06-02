#Mini projet

library(ggplot2)
rm(list=objects());graphics.off()

#importation des données
Music_2023 <- read.csv("~/ENSTA/STA203/Music_2023.txt", header=TRUE, sep=";")
View(Music_2023)

complete=Music_2023
style=Music_2023[,192]
M=Music_2023[,1:191]
#on réécrit les styles avec des nombres
style_en_num=ifelse(style=="Jazz",0,1)
complete$GENRE=style_en_num
p = ncol(complete)-1          # nombre de variables explicatives
n = nrow(complete)


###partie I#######################################################
##################################################################

#question 1#######################################################


#analyse decriptive

#on créé une boîte à moustaches pour chaque variable explicative
#on retrouve le nom des variables dans l'énoncé
#on fait pour les paramètres 4 à 37 mais on pourrait faire pour tous à condition
#d'avoir le nom de la variable
pdf("musique-boxplot-pairs.pdf")
par(mfrow=c(3,3))
apply(cbind(paste("PAR_ASE",1:34,sep="")),1, function(x)  
{boxplot(complete[,x]~complete$GENRE,xlab=x, horizontal=TRUE);return()  })
dev.off()

#maintenant on plot les corrélations
pdf("musique-corrplot.pdf")
par(mfrow=c(1,1))
corrplot::corrplot(cor(complete),method = "square")
dev.off()
#remarque: on peut vaguement distinguer des couleurs sur le résulat mais c'est quand
#même un peu illisible

#proportion
prop <- mean(style=="Jazz")
#[1] 0.4689107

#regression log 
#on extrait les valeurs concernées et on regarde ce qui pourrait justifier d'appliquer
#le log
par_sc_v=M$PAR_SC_V #grandes valeurs
summary(par_sc_v)
par_asc_v=M$PAR_ASC_V #valeurs très proches de 0
summary(par_asc_v)
#DONC on va vouloir appliquer le log sur ces variables quand on va chercher un modèle

##variables 148 à 167
#DONC on les supprime car en annexe on voit que c'est les mêmes que 128 à 147
new_M=cbind(complete[,1:147],complete[,168:192])

#identification des variables très corrélées
#En ce qui concerne les variables très corrélées, il est possible de les supprimer pour éviter la redondance d'informations
#quelques variables très corrélées
C = cor(new_M[,1:171])-diag(1,171)
#comme la matrice est symetrique on ne garde que la triangulaire inferieur pour éviter la redondance d'information
C[upper.tri(C)] <- 0
paires_cor=cbind(which(C>0.99)%/%171 +1, which(C>0.99)%%171)
#on recup les forts corrélations et on met sous forme de pair de variables
#on obtient
#[,1] [,2]
#[1,]   36   37
#[2,]   71   72
#[3,]  160  164
#DONC on peut supprimer les variables 37, 72, 164 de new_M

#variables PAR_ASE_M, PAR_ASE_MV, PAR_SFM_M et PAR_SFM_MV
#ce sont des valeurs moyennes, elles dépendent linéairement des autres valeurs
#DONC on peut donc également les retirer

#définition du modèle logistique
res = glm(GENRE~.,data=complete,family=binomial)
summary(res) 

#hypothèses sur ce jeu de données
# GENRE_i ~ B(1, pi(x_i)) i=1,.., n indépendants
# logit(pi(x_i))=x_i\ theta
# où x_i=(1, x_i1, .., x_i57) les valeurs de covariables pour l'individu i
# données individuelles
# modèle de dimension 1+191
X = as.matrix(cbind(1,complete[,-192]) )        # le plan d'expérience
#Xinv = solve(t(X)%*%X)                    
#une erreur s'affiche ce qui est normal car certaines des variables explicatives sont parfaitement 
#corrélées ou linéairement dépendantes les unes des autres (même identiques)
#Error in solve.default(t(X) %*% X) : 
#  Lapack routine dgesv: system is exactly singular: U[175,175] = 0
#DONC les hypotheses ne sont pas vérifiées

######question 2########################################################################

#définition échantillon d'apprentissage

set.seed(103)
train=sample(c(TRUE,FALSE),n,rep=TRUE,prob=c(2/3,1/3))
#Cette commande permet de créer un vecteur logique de longueur n avec des valeurs TRUE et FALSE 
#en utilisant une distribution de probabilité donnée.

#######question 3##################################################################
#estimation de différents modèles logistiques 

#########M0
#variables données

Mod0=glm(GENRE~PAR_TC+PAR_SC+PAR_SC_V+PAR_ASE_M+PAR_ASE_MV+PAR_SFM_M+PAR_SFM_MV,data=complete, subset=train)

#########MT
#variables retenues dans la question 1

#on redéfini complete
#1: on applique le log
#2: on retire les doublons
#3: on retire les variables très corrélées 37, 72, 164
#4: on retire les variables moyennes

#1
new_complete_1=complete
new_complete_1$PAR_SC_V=log(complete$PAR_SC_V)
new_complete_1$PAR_ASC_V=log(complete$PAR_ASC_V)
#2
new_complete_2=cbind(new_complete_1[,1:147],new_complete_1[,168:192])
#3
new_complete_3=new_complete_2[,c(-37, -72, -164)]
new_complete <- new_complete_3[, setdiff(names(new_complete_3), c("PAR_ASE_M", "PAR_ASE_MV", "PAR_SFM_M", "PAR_SFM_MV"))]

#finalement on obtient le modèle
ModT <- glm(GENRE ~ ., data = new_complete , family = binomial, subset = train)

#######MOD1 et MOD2
#on retient seulement certaines variables significatives du précédent modèle

train.M <- complete[train,]
test.M <- complete[!train,]
sum.ModT  = summary(ModT)
names_0.05 <- names(which(sum.ModT$coefficients[,4] <= 0.05))
names_0.2 <- names(which(sum.ModT$coefficients[,4] <= 0.2))

#finalement on obtient les modèles
Mod1<- glm(GENRE~. , 
           data = train.M[, c("GENRE", names_0.05[-1])],
           family = binomial)

Mod2<- glm(GENRE~. , 
           data = train.M[, c("GENRE", names_0.2[-1])],
           family = binomial)

#ModAIC
#utilisation de la fonction STEPAIC
library(MASS)
st1 = stepAIC(ModT)

#On obtient:
#GENRE ~ PAR_TC + PAR_SC + PAR_SC_V + PAR_ASE1 + PAR_ASE2 + PAR_ASE3 + 
# PAR_ASE4 + PAR_ASE5 + PAR_ASE6 + PAR_ASE7 + PAR_ASE8 + PAR_ASE9 + 
#   PAR_ASE10 + PAR_ASE11 + PAR_ASE12 + PAR_ASE13 + PAR_ASE14 + 
#   PAR_ASE15 + PAR_ASE16 + PAR_ASE17 + PAR_ASE18 + PAR_ASE19 + 
#   PAR_ASE20 + PAR_ASE21 + PAR_ASE22 + PAR_ASE23 + PAR_ASE24 + 
#   PAR_ASE25 + PAR_ASE26 + PAR_ASE27 + PAR_ASE28 + PAR_ASE29 + 
#   PAR_ASE30 + PAR_ASE31 + PAR_ASE32 + PAR_ASE33 + PAR_ASEV1 + 
#   PAR_ASEV2 + PAR_ASEV3 + PAR_ASEV4 + PAR_ASEV5 + PAR_ASEV6 + 
#   PAR_ASEV7 + PAR_ASEV8 + PAR_ASEV9 + PAR_ASEV10 + PAR_ASEV11 + 
#   PAR_ASEV12 + PAR_ASEV13 + PAR_ASEV14 + PAR_ASEV15 + PAR_ASEV16 + 
#   PAR_ASEV17 + PAR_ASEV18 + PAR_ASEV19 + PAR_ASEV20 + PAR_ASEV21 + 
#   PAR_ASEV22 + PAR_ASEV23 + PAR_ASEV24 + PAR_ASEV25 + PAR_ASEV26 + 
#   PAR_ASEV27 + PAR_ASEV28 + PAR_ASEV29 + PAR_ASEV30 + PAR_ASEV31 + 
#   PAR_ASEV32 + PAR_ASEV33 + PAR_ASC + PAR_ASC_V + PAR_ASS + 
#   PAR_ASS_V + PAR_SFM1 + PAR_SFM2 + PAR_SFM3 + PAR_SFM4 + PAR_SFM5 + 
#   PAR_SFM6 + PAR_SFM7 + PAR_SFM8 + PAR_SFM9 + PAR_SFM10 + PAR_SFM11 + 
#   PAR_SFM12 + PAR_SFM13 + PAR_SFM14 + PAR_SFM15 + PAR_SFM16 + 
#   PAR_SFM17 + PAR_SFM18 + PAR_SFM19 + PAR_SFM20 + PAR_SFM21 + 
#   PAR_SFM22 + PAR_SFM23 + PAR_SFM24 + PAR_SFMV1 + PAR_SFMV2 + 
#   PAR_SFMV3 + PAR_SFMV4 + PAR_SFMV5 + PAR_SFMV6 + PAR_SFMV7 + 
#   PAR_SFMV8 + PAR_SFMV9 + PAR_SFMV10 + PAR_SFMV11 + PAR_SFMV12 + 
#   PAR_SFMV13 + PAR_SFMV14 + PAR_SFMV15 + PAR_SFMV16 + PAR_SFMV17 + 
#   PAR_SFMV18 + PAR_SFMV19 + PAR_SFMV20 + PAR_SFMV21 + PAR_SFMV22 + 
#   PAR_SFMV23 + PAR_SFMV24 + PAR_MFCC1 + PAR_MFCC2 + PAR_MFCC3 + 
#   PAR_MFCC4 + PAR_MFCC5 + PAR_MFCC6 + PAR_MFCC7 + PAR_MFCC8 + 
#   PAR_MFCC9 + PAR_MFCC10 + PAR_MFCC11 + PAR_MFCC12 + PAR_MFCC13 + 
#   PAR_MFCC14 + PAR_MFCC15 + PAR_MFCC16 + PAR_MFCC17 + PAR_MFCC18 + 
#   PAR_MFCC19 + PAR_MFCC20 + PAR_THR_1RMS_TOT + PAR_THR_2RMS_TOT + 
#   PAR_THR_3RMS_TOT + PAR_THR_1RMS_10FR_MEAN + PAR_THR_1RMS_10FR_VAR + 
#   PAR_THR_2RMS_10FR_MEAN + PAR_THR_2RMS_10FR_VAR + PAR_THR_3RMS_10FR_MEAN + 
#   PAR_THR_3RMS_10FR_VAR + PAR_PEAK_RMS_TOT + PAR_PEAK_RMS10FR_MEAN + 
#   PAR_PEAK_RMS10FR_VAR + PAR_ZCD + PAR_1RMS_TCD + PAR_2RMS_TCD + 
#   PAR_3RMS_TCD + PAR_ZCD_10FR_VAR + PAR_1RMS_TCD_10FR_MEAN + 
#   PAR_1RMS_TCD_10FR_VAR + PAR_2RMS_TCD_10FR_MEAN + PAR_2RMS_TCD_10FR_VAR + 
#   PAR_3RMS_TCD_10FR_MEAN + PAR_3RMS_TCD_10FR_VAR
ModAIC = glm(GENRE~.,data=st1$model) # data=st1$model sous sélectionne la matrice du plan d'exp


####### question 4#############################################################

############# courbes ROC sur l'échantillon d'apprentissage et sur l'échantillon 
#de test pour le modèle ModT

library(ROCR)

# Prédictions sur l'échantillon d'apprentissage
pred_train <- prediction(predict(ModT, newdata = complete[train,]), new_complete$GENRE[train])

# Prédictions sur l'échantillon de test
pred_test <- prediction(predict(ModT, newdata = complete[which(!train),]), new_complete$GENRE[which(!train)])
pred_test <- predict(ModT, newdata = test.M)
# Tracer les courbes ROC pour l'échantillon d'apprentissage et l'échantillon de test

plot(performance(pred_train, "tpr", "fpr"), col = "blue", main = "Courbes ROC pour ModT", lwd = 2, xlim = c(0,1), ylim = c(0,1), xlab = "Taux de faux positifs", ylab = "Taux de vrais positifs")
plot(performance(pred_test, "tpr", "fpr"), col = "red", add = TRUE, lwd = 2)

# Ajouter la courbe de la règle parfaite
#même prédiction que valeurs
plot(performance(prediction(complete$GENRE, complete$GENRE), "tpr", "fpr"), col = "black", add = TRUE, lty = 2, lwd = 2)

# Ajouter la courbe de la règle aléatoire
#génération de valeurs aléatoires avec runif
plot(performance(prediction(runif(nrow(new_complete)), new_complete$GENRE), "tpr", "fpr"), col = "green", add = TRUE, lty = 2, lwd = 2)
legend("bottomright", legend = c("Apprentissage", "Test", "Règle parfaite", "Règle aléatoire"), col = c("blue", "red", "black", "green"), lty = c(1,1,2,2), lwd = 2)


##############courbes ROC pour les autres modèles

test <- which(!train)

# Prédiction des classes positives pour chaque modèle
pred_mod0 <- predict(Mod0, newdata = complete[test,], type = "response")
pred_mod1 <- predict(Mod1, newdata = complete[test,], type = "response")
pred_mod2 <- predict(Mod2, newdata = complete[test,], type = "response")
pred_modAIC <- predict(ModAIC, newdata = complete[test,], type = "response")

# Calcul des performances de classification pour chaque modèle
perf_mod0 <- performance(prediction(pred_mod0, complete[test,]$GENRE), measure = "sens", x.measure = "fpr")
perf_mod1 <- performance(prediction(pred_mod1, complete[test,]$GENRE), measure = "sens", x.measure = "fpr")
perf_mod2 <- performance(prediction(pred_mod2, complete[test,]$GENRE), measure = "sens", x.measure = "fpr")
perf_modAIC <- performance(prediction(pred_modAIC, complete[test,]$GENRE), measure = "sens", x.measure = "fpr")

#On calcul ensuite les aires sous la courbe ROC

auc_mod0 <- round(auc(perf_mod0), 2)
auc_mod1 <- round(auc(perf_mod1), 2)
auc_mod2 <- round(auc(perf_mod2), 2)
auc_modAIC <- round(auc(perf_modAIC), 2)

# Tracé des courbes ROC pour chaque modèle
plot(perf_mod0, col = "blue", main = "Courbes ROC pour tous les modèles", xlim = c(0,1), ylim = c(0,1), lwd = 2)
plot(perf_mod1, add = TRUE,col = 'red',lwd = 2)
plot(perf_mod2, add = TRUE,col = 'green',lwd = 2)
plot(perf_modAIC, add = TRUE,col = 'purple',lwd = 2)

# Ajout de la légende
legend("bottomright", legend = c("Mod0", "Mod1", "Mod2", "ModAIC"), col = c("blue", "red", "green", "purple"), lwd = 2)
text(0.9, 0.6, paste("AUC0 = ", auc_mod0 ), font = 2, col = 'blue')
text(0.9, 0.55, paste("AUC1 = ", auc_mod1 ), font = 2, col = 'red')
text(0.9, 0.5, paste("AUC2 = ", auc_mod2), font = 2, col = 'green')
text(0.9, 0.4, paste("AUC AIC = ", perf_modAIC), font = 2, col = 'purple')


######question 5###############################################################""
#calcul de l'erreur sur les différents échantillons

err0_test <- 1 - mean(ifelse(pred_mod0>0.5,1,0) == test.M$GENRE) #0.2627533
errT_test <- 1 - mean(ifelse(pred_test>0.5,1,0) == test.M$GENRE) #0.5366876
err1_test <- 1 - mean(ifelse(pred_mod1>0.5,1,0) == test.M$GENRE) #0.1111111
err2_test <- 1 - mean(ifelse(pred_mod2>0.5,1,0) == test.M$GENRE) #0.1027254
errAIC_test <- 1 - mean(ifelse(pred_modAIC>0.5,1,0) == test.df$GENRE) #pas pu rerun car trop long



############################### PARTIE 2 ############################################
#####################################################################################

#### Question 1 : voir rapport 

#### Question 2

library(glmnet)

grid = 10^seq(10,-2,length=100)
x = as.matrix(complete[,-192]) # touit sauf dernière colonne de Music_2023
y = complete[,192] # dernière colonne de Music_2023
ridge.fit = glmnet(x,y,alpha=0,lambda=grid) # régression ridge
plot(ridge.fit)

#### Question 3

set.seed(314)
train=sample(c(TRUE,FALSE),n,rep=TRUE,prob=c(2/3,1/3))
test = !train
y.test = y[test]

# Validation croisée en 10 segments pour estimer le paramètre de régularisation

cvfit = cv.glmnet(x[train,], y[train], alpha=0, lambda=grid, nfolds=10)
bestlam = cvfit$lambda.min  # 0.01, faible, donc les coefficients sont pas forcément faibles aussi
bestlam
log(bestlam)                # -4.60517
ridge.pred = predict(cvfit,s=bestlam,newx=x[train==FALSE,])
# calcul de l'erreur quadratique moyenne entre les prédictions et les valeurs réelles
rmse = mean((ridge.pred - y.test)^2)
rmse # affiche la valeur de l'erreur quadratique moyenne : 0.09585632
class1 = ifelse(ridge.pred >0.5,1,0)
mean(class1!=y.test) # 0.09652236

#### Question 4

# Estimation du modèle sur tout le jeu de données avec le meilleur lambda
set.seed(4658)
ridge.fit.fin = glmnet(x, y, alpha=0, lambda=bestlam, thresh=1e-12)
prediction=predict(ridge.fit.fin,s=bestlam,newx=x[train==FALSE,])
class = ifelse(prediction >0.5,1,0)
mean(class!=y.test) # erreur de classification de 0.08374734 : modèle a une erreur de 8,37 %

############################### PARTIE 3 ######################################

#### Question 1 

library(MASS)
library(class)

x=complete[,-192]
xtrain=x[train==TRUE,]
xtest=x[train==FALSE,]
y=as.factor(complete[,192])

y.train1=y[train==TRUE]
y.test1=y[train==FALSE]

# création du modèle avec K=1

# donnees d'entrainement
model_k1_train=knn(train=xtrain, test=xtrain, cl=y.train1, k=1)
# calcul de l'erreur de classification pour les données d'entraînement 
error.rate_train=mean(model_k1_train!=y.train1) # = 0
# donnees de test
model_k1_test=knn(train=xtrain,test=xtest,cl=y.train1,k=1)
# calcul de l'erreur de classification pour les données de test 
error.rate_test=mean(model_k1_test!=y.test1) # = 0.3804273

# Recherche du meilleur k 
errors <- rep(0, 20)

for (i in 1:20) {
  errors[i] <- mean(knn(train=xtrain,test=xtest,cl=y.train1,k=i)!=y.test1)
}
bestk <- which.min(errors)
# le meilleur k est 1, pas terrible comme erreur et on n'utilise quasi pas les données

######conclusion###########################################################################
###########################################################################################

#on garde mod2

# Chargement des données de test
Music_test <- read.csv("~/ENSTA/STA203/Music_test.txt", header=TRUE, sep=";")

# Suppression de la variable à deviner
Music_t <- Music_test[, -1]

# Prédiction des genres avec le modèle Mod2
pred_genres <- predict(Mod2, newdata = Music_t)

# Création d'un nouveau data frame avec les identifiants et les genres prédits
resultats <- data.frame( ID=1:2169,GENRE = ifelse(pred_genres > 0.5, "Jazz", "Classique"))

# Écriture des résultats dans un fichier texte
write.table(resultats, file = "CHARRY-CHABERT_test.txt", sep = "\t", row.names = FALSE, quote = FALSE)




