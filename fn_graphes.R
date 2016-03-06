graph_var_logistique <- function(df, modl, y, var, titre) {
    
    # En 2 parties, d'abord des calculs, puis le graphe en tant que tel.
    
    # 1ère partie : calcul des éléments à mettre sur la graphe
    
    X <- as.factor(df[,var])
    FREQ <- levels(X)
    
    ncvar <- nchar(var)
    
    # Initialisation des résultats
    univ <- n <- rep(NA,length(FREQ))
    coef_modl <- IC1 <- IC2 <- rep(0, length(FREQ))
    
    # Exploitation des résu du modèles    
    nom <- names(modl$coefficients)
    k <- which(substr(nom, 1, ncvar) == var)
    if(length(k)==0){return(NA)}
    co <- modl$coefficients[k]
    v <- summary(modl)$coefficients[k, 2]

    nom_modl <- unlist(lapply(names(co), function(s) substr(s, ncvar + 1, nchar(s))))

    for(k in 1:length(FREQ)){
        
        # Stats univariés : nb de cas, fréquence
        n[k] <- length(X[X == FREQ[k]])
        univ[k] <- sum(df[X == FREQ[k], y]) / n[k] 
        
        # Stats du modèle, par modalité de la variable explicative : coef estimé et écart type
        i <- which(nom_modl == FREQ[k])
        if (!(length(i) == 0)) {
            coef_modl[k] <- exp(co[i]) - 1
            IC1[k] <- exp(co[i] + 1.96 * v[i]) - 1
            IC2[k] <- exp(co[i] - 1.96 * v[i]) - 1
        } else { inull <- k}
        
    }
    t <- univ[inull]
    univ <- univ / t - 1
    
    
    # 2ème partie : le graphe (enfin !)
    # très largement inspiré d'un élément trouvé sur le blog d'Arthur Charpentier
    # à l'origine devant représenter des stats de fréquence sinistres
    # cf http://freakonometrics.hypotheses.org/20128
    
    # Les effectifs
    par(mar=c(7, 4, 2, 2) + 0.2)
    w <- barplot(n, col="yellow", axes = FALSE
                 , xlim = c(0, 1.2*length(FREQ)+.5), ylim = c(0, 1.5*max(n))
                 , main = titre, cex.main = 1
                 , names.arg = FREQ, cex.names = 0.8, las = 2)
#    text(srt = 60, xpd = TRUE, labels = FREQ, cex = 0.65)
    mid <- w[, 1]
    axis(4, )
    
    # Le modèle
    par(new=TRUE)
    plot(mid, coef_modl
         , ylim = c(min(c(univ, min(c(IC1, IC2) - diff(range(c(IC1, IC2))) / 4))), max(c(IC1, IC2, univ)))
         , type = "l", lwd=5, col = "#FF00FF", axes = FALSE, xlab = "", ylab = ""
         , xlim = c(0, 1.2*length(FREQ)+.5))
    points(mid, coef_modl, pch = 15, col = "#FF00FF")
    
    segments(mid,IC1,mid,IC2,col="darkorchid4")
    segments(mid-.1,IC1,mid+.1,IC1,col="darkorchid4")
    segments(mid-.1,IC2,mid+.1,IC2,col="darkorchid4")

    # L'univarié
    par(new=TRUE)
    plot(mid, univ
         , ylim = c(min(c(univ, min(c(IC1, IC2) - diff(range(c(IC1, IC2))) / 4))), max(c(IC1, IC2, univ)))
#         , ylim = c(min(c(IC1, IC2) - diff(range(c(IC1, IC2))) / 4), max(c(IC1, IC2)))
         , type = "l", col = "blue", axes = FALSE, xlab = "", ylab = ""
         , xlim = c(0, 1.2*length(FREQ)+.5))
    points(mid,univ,pch=15,col="blue")
    
    # un quadrillage
    #    for(i in (-10):20) segments(min(mid)-.8,i/40,max(mid)+.8,i/40,lty=2,col="grey")
    
    # L'axe de gauche 
    y.at <- round(seq(round(min(c(IC1, IC2)), 1), max(c(IC1, IC2)),by=.1), 1)
    axis(2, at = y.at)
    
    # Une ligne à 0 sur l'axe de gauche
    abline(h = 0, lty = 2, col = "darkorchid3")
    
    # Légendes
    mtext("Effectif", 4, line = -1, cex = .8)
    mtext("Odds ratio", 2, line = +2, cex = .8)
    legend("topleft",c("Effectif", "Univarié", "Modèle", "Intervalle conf."),
           col=c("yellow", "blue", "#FF00FF", "darkorchid4"), lwd=c(10, 1, 5, 1), lty=c(1, 1, 1, 1), cex=.6)
}