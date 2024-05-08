# Indice CVS-CJO de la production industrielle (base 100 en 2021) 
# Industries alimentaires (NAF rév. 2, niveau division, poste 10)

# Serie décrivant l’évolution mensuelle de la production 
# alimentaire sur le territoire metropolitain francais 

# Série aggregée qui normalise le niveau de production à 100 en 2021
#, corrigée des variations saisonnières (CVS) et des jours ouvrés (CJO)

##  Partie I : Les donnees

require(readxl)
require(zoo)
require(tseries)
require(ggplot2)

data <- read_xlsx("/Users/y.boukhateb/Desktop/Times series/serietemp/data_projet.xlsx",  skip = 3)

# Range les données par date + exclut période covid
data[[1]] = as.yearmon(data[[1]])
data <- data[order(data[[1]] ), ]
data <- data[data[[1]] < as.yearmon("Jan 2020"), ]


x = zoo(data[[2]])
autoplot(x)


# Test de racine unitaire de Phillips-Perron
pp.test(x)
# p-value = 0.01 -> on rejette H0 = racine unitaire à 5%

# Differenciation et demoyennisation de la série pour supprimer tendance
# deterministe
w = x - lag(x,1)
y <- w - mean(w) #
autoplot(y)

# Test de racine unitaire de Phillips-Perron
pp.test(y)
# p-value = 0.01 -> on rejette H0 = racine unitaire à 5%



# Plot ACF et PACF sur même graphique
par(mfrow = c(1,2))
acf(y)
pacf(y)

qmax= 1
pmax =7

## Partie II : Modèles ARMA

# Test Ljung-Box sur l'absence d'autocorrélation de l'ordre 1 à k
Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}

# Premier modèle ARMA(7,1)
arma71 = arima( y,order = c(7,0,1))

# Test Ljung-Box sur l'absence d'autocorrélation de ARMA(7,1)
Qtests(arma71$residuals, 24, fitdf = length(arma71$coefficients))
# On accepte HO: BB faible -> modèle est valide

# Significativité des coefficients estimés
arma71
# Dernier coefficient MA n'est pas significatif -> modèle n'est pas pertinent

# Identifie les degrés p, q de l'ARMA valide le plus pertinent 
pqs <- expand.grid(0:pmax,0:qmax) #combinaisons possibles de p<=p* et q<=q*
mat <- matrix(NA, nrow=pmax+1, ncol=pmax+1)
rownames(mat) <- paste0("p=",0:pmax) 
colnames(mat) <- paste0("q=",0:pmax) 
AICs <- mat #matrice ou assigner les AIC
BICs <- mat #matrice ou assigner les BIC
for (row in 1:dim(pqs)[1]){
  p <- pqs[row,1]
  q <- pqs[row,2]
  estim <- try(arima(y,order = c(p,0,q),include.mean = T)) #estime l'ARMA
  pvals <- Qtests(estim$residuals, 24, fitdf = length(estim$coefficients))[, 2]
  if (sum(pvals < 0.05, na.rm = TRUE) == 0) {
    noautocorr <- 1
  } else noautocorr <- 0
  if (class(estim) == "try-error" | noautocorr != 1 ) {
    AICs[p + 1, q + 1] <- NA
    BICs[p + 1, q + 1] <- NA
  } else {
    AICs[p + 1, q + 1] <- estim$aic
    BICs[p + 1, q + 1] <- BIC(estim)
  }
}
AICs
BICs

# Determine le modèle qui minimise AIC
AICs==min(AICs,  na.rm = TRUE)
# p = 1 / q = 1

# Determine le modèle qui minimise BIC
BICs==min(BICs,  na.rm = TRUE)
# p = 0 / q = 1 


# ARIMA(X,1,X) sur la série initiale
arima011 <- arima(x,c(0,1,1))
arima111 <- arima(x,c(1,1,1))


# Validité du modèle + Pertinence 
Qtests(arima011$residuals, 24, length(arima011$coefficients))
arima011
# Accepte absence d'autocorrelation à tous les ordres à 5% mais pas à 10%
# Dernier coefficient MA est significatif à 5%

Qtests(arima111$residuals, 24, length(arima111$coefficients))
arima111
# Accepte absence d'autocorrelation à tous les ordres à 10%
# Dernier coefficient AR est significatif à 10% mais pas à 5% 

##  Partie III : Prevision

# Ajuster un modèle ARIMA(1,1,1) sur vos données 'x'
model <- arima(x, order=c(1,1,1))

# Extraire les coefficients du modèle
ar1 <- coef(model)["ar1"]
ma1 <- coef(model)["ma1"]


# Définir les coefficients phi1, phi2 et theta1
phi1 <- 1 + ar1
phi2 <- ar1
theta1 <- ma1

# Afficher les coefficients
print(paste("phi1 =", phi1))
print(paste("phi2 =", phi2))
print(paste("theta1 =", theta1))


# Obtenir la variance des résidus (bruit blanc)
residuals <- residuals(model)

variance_residus <- var(residuals)

# Afficher la variance des résidus
print(paste("Variance du bruit blanc =", variance_residus))

# Calculer les éléments de la matrice
element11 <- variance_residus
element12 <- variance_residus * (phi1 - theta1)
element21 <- variance_residus * (phi1 - theta1)
element22 <- variance_residus * (1 + (phi1 - theta1)^2)

# Créer la matrice
Sigma <- matrix(c(element11, element12, element21, element22), nrow=2, ncol=2, byrow=TRUE)

library(ellipse)

# Prédire les valeurs à T+1 et T+2 avec le modèle ARIMA(1,1,1)
forecast_values <- predict(model, n.ahead=2)

# Extraire les prédictions
prediction_T1 <- forecast_values$pred[1]
prediction_T2 <- forecast_values$pred[2]

# Tracer l'ellipse de confiance
plot(ellipse(Sigma, centre=c(prediction_T1, prediction_T2), level=0.95),
     xlab="Prédiction à T+1", ylab="Prédiction à T+2",
     main="Ellipse de confiance pour ARIMA(1,1,1)")

points(x=prediction_T1, y=prediction_T2, col="red", pch=19)
legend("topright", legend="Prédictions", pch=19, col="red", cex=0.8)

