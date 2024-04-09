
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

data <- read_xlsx("/home/onyxia/work/statapp/data_projet.xlsx",  skip = 3)

# Range les données par date
data[[1]] = as.yearmon(data[[1]])
data <- data[order(data[[1]] ), ]


x = zoo(data[[2]])
autoplot(x)


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
?adfTest

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
  estim <- try(arima(y,order = c(p,0,q))) #estime l'ARMA
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
# p = 1 / q = 1 

print(min(AICs))

# ARIMA(X,1,X) sur la série initiale qui avait été différenciée
arima111 <- arima(x,c(1,1,1),include.mean=T)

# Validité du modèle
Qtests(arima111$residuals, 24, length(estim$coefficients))
# H0 est rejeté à tous les ordres -> Modèle est valide

# Pertinence du modèle 
arima111
# Derniers coefficients MA/AR sont significatifs -> modèle pertinent

##  Partie III : Prevision

T = length(x)
