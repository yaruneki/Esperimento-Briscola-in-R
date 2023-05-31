# Funzione per simulare una partita di briscola
simula_partita_briscola <- function() {
  # Inizializzazione del mazzo di carte
  mazzo <- c("Asso di Denari", "Due di Denari", "Tre di Denari", "Quattro di Denari",
             "Cinque di Denari", "Sei di Denari", "Sette di Denari", "Fante di Denari",
             "Cavallo di Denari", "Re di Denari", "Asso di Spade", "Due di Spade",
             "Tre di Spade", "Quattro di Spade", "Cinque di Spade", "Sei di Spade",
             "Sette di Spade", "Fante di Spade", "Cavallo di Spade", "Re di Spade",
             "Asso di Coppe", "Due di Coppe", "Tre di Coppe", "Quattro di Coppe",
             "Cinque di Coppe", "Sei di Coppe", "Sette di Coppe", "Fante di Coppe",
             "Cavallo di Coppe", "Re di Coppe", "Asso di Bastoni", "Due di Bastoni",
             "Tre di Bastoni", "Quattro di Bastoni", "Cinque di Bastoni", "Sei di Bastoni",
             "Sette di Bastoni", "Fante di Bastoni", "Cavallo di Bastoni", "Re di Bastoni")
  
  # Mischia il mazzo di carte
  mazzo_mischiato <- sample(mazzo)
  
  # Distribuzione delle carte ai giocatori
  carte_giocatore1 <- mazzo_mischiato[1:3]
  carte_giocatore2 <- mazzo_mischiato[4:6]
  
  # Determina la briscola
  briscola <- mazzo_mischiato[7]
  
  # Inizializzazione dei punti dei giocatori
  punti_giocatore1 <- 0
  punti_giocatore2 <- 0
  
  # Simulazione delle mani
  index <- 8
  vin <- 0
  while (index <=39){
    
    print(cat("indice: ",index))
    # Giocatore 1 gioca una carta
    carta <- carte_giocatore1[sample(1)]
    carte_giocatore1 <- carte_giocatore1[carte_giocatore1 != carta]
    punti_manouno <- punteggio_carta(carta, briscola)
    
    # Giocatore 2 gioca una carta
    carta <- carte_giocatore2[sample(1)]
    carte_giocatore2 <- carte_giocatore2[carte_giocatore2 != carta]
    punti_manodue <- punteggio_carta(carta, briscola)
    
    if (!is.na(punti_manouno) && !is.na(punti_manodue)) {
      if (punti_manouno >= punti_manodue) {
        punti_giocatore1 <- punti_giocatore1 + punti_manouno + punti_manodue
        carte_giocatore1 <- c(carte_giocatore1, mazzo_mischiato[index])
        index <- index+1
        carte_giocatore2 <- c(carte_giocatore2, mazzo_mischiato[index])
        index <- index+1
        if (index == 39){
          vin <- 1
        }
      } else if (punti_manouno < punti_manodue) {
        punti_giocatore2 <- punti_giocatore2 + punti_manouno + punti_manodue
        carte_giocatore2 <- c(carte_giocatore2, mazzo_mischiato[index])
        index <- index+1
        carte_giocatore1 <- c(carte_giocatore1, mazzo_mischiato[index])
        index <- index+1
        if (index == 39){
          vin <- 2
        }
      }
    }else if (!is.na(punti_manouno) && is.na(punti_manodue)) {
      print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      punti_giocatore1 <- punti_giocatore1 + punti_manouno + punti_manodue
      carte_giocatore1 <- c(carte_giocatore1, mazzo_mischiato[index])
      index <- index+1
      carte_giocatore2 <- c(carte_giocatore2, mazzo_mischiato[index])
      index <- index+1
      if (index == 39){
        vin <- 1
      }
    }else{
      punti_giocatore2 <- punti_giocatore2 + punti_manouno + punti_manodue
      carte_giocatore2 <- c(carte_giocatore2, mazzo_mischiato[index])
      index <- index+1
      carte_giocatore1 <- c(carte_giocatore1, mazzo_mischiato[index])
      index <- index+1
      if (index == 39){
        vin <- 2
      }
    }
  }
  
  if (vin == 1){
    carte_giocatore1 <- c(carte_giocatore1, mazzo_mischiato[length(mazzo_mischiato)])
    carte_giocatore2 <- c(carte_giocatore2, mazzo_mischiato[7])
  }
  if (vin == 2){
    carte_giocatore2 <- c(carte_giocatore2, mazzo_mischiato[7])
    carte_giocatore1 <- c(carte_giocatore1, mazzo_mischiato[length(mazzo_mischiato)])
  }
  for (i in 1:3) {
    # Giocatore 1 gioca una carta
    carta <- carte_giocatore1[sample(1)]
    carte_giocatore1 <- carte_giocatore1[carte_giocatore1 != carta]
    punti_manouno <- punteggio_carta(carta, briscola)
    
    # Giocatore 2 gioca una carta
    carta <- carte_giocatore2[sample(1)]
    carte_giocatore2 <- carte_giocatore2[carte_giocatore2 != carta]
    punti_manodue <- punteggio_carta(carta, briscola)
    
    if (!is.na(punti_manouno) && !is.na(punti_manodue)) {
      if (punti_manouno > punti_manodue) {
        punti_giocatore1 <- punti_giocatore1 + punti_manouno + punti_manodue
      } else if (punti_manouno < punti_manodue) {
        punti_giocatore2 <- punti_giocatore2 + punti_manouno + punti_manodue
      }
    }
  }
  vincitore <- -1
  if(!is.na(punti_giocatore1) && !is.na(punti_giocatore2)){
    # Determina il vincitore della partita
    if (punti_giocatore1 > punti_giocatore2) {
      vincitore <- 1
    } else if (punti_giocatore1 < punti_giocatore2) {
      vincitore <- 2
    } else {
      vincitore <- 0
    }
  }else if(!is.na(punti_giocatore1) && is.na(punti_giocatore2)){
    vincitore <- 2
  }else{
    vincitore <- 1
  }
  
  # Restituisce il vincitore della partita
  return(vincitore)
}

# Funzione per calcolare il punteggio di una carta in base alla briscola
punteggio_carta <- function(carta, briscola) {
  print(carta)
  print(briscola)
  # Imposta il punteggio di ogni carta
  punteggi <- c("Asso" = 11, "Tre" = 10, "Re" = 4, "Cavallo" = 3, "Fante" = 2,
                "Sette" = 0, "Sei" = 0, "Cinque" = 0, "Quattro" = 0, "Due" = 0)
  
  # Determina il seme della carta
  seme_carte <- c("Denari", "Spade", "Coppe", "Bastoni")
  seme <- substr(carta, nchar(carta) - 8, nchar(carta) - 6)
  
  # Calcola il punteggio della carta
  punteggio <- punteggi[substr(carta, 1, nchar(carta) - 10)]
  if(is.na(punteggio))
    punteggio <- 0
  # Applica il bonus per la briscola
  if (!is.na(substr(carta, nchar(carta) - 4, nchar(carta))) && !is.na(briscola)) {
    if (substr(carta, nchar(carta) - 4, nchar(carta)) == briscola) {
      punteggio <- punteggio + 10
    }
  }
  print(punteggio)
  return(punteggio)
}

#Simulazione dell'esperimento
esperimento <- function(n) {
  giocatore1 <- 0
  giocatore2 <- 0
  vittorie1 <- 0
  vittorie2 <- 0
  g1 <- 0
  g2 <- 0
  absv1 <- 0
  absv2 <- 0
  for (i in 1:n){
    # Simulazione di una partita di briscola
    risultato <- simula_partita_briscola()
    # Stampa il risultato
    if (risultato == 0) {
      cat("La partita è terminata in pareggio.\n")
    } else {
      cat("Il giocatore", risultato, "ha vinto la partita!\n")
      if(risultato == 1){
        g1 <- g1+1
        if(g1 == 2){
          g1 <- 0
          g2 <- 0
          absv1 <- absv1 + 1
        }
      }else if(risultato == 2){
        g2 <- g2+1
        if (g2 == 2){
          g1 <- 0
          g2 <- 0
          absv2 <- absv2 + 1
        }
      }
      if(((risultato == 1) && (n%%2 == 1)) || (risultato == 2 && (n%%2 == 0))){
        giocatore1 <- giocatore1+1
        if(giocatore1 == 2){
          giocatore1 <- 0
          giocatore2 <- 0
          vittorie1 <- vittorie1 + 1
        }
      }else if((risultato == 2 && (n%%2 == 1)) || (risultato == 1 && (n%%2 == 0))){
        giocatore2 <- giocatore2+1
        if(giocatore2 == 2){
          giocatore1 <- 0
          giocatore2 <- 0
          vittorie2 <- vittorie2 + 1
        }
      }
    }
  }
  par(mfrow = c(1, 2))
  val1 <- c((vittorie1/(vittorie1+vittorie2)),(vittorie2/(vittorie1+vittorie2)))
  lab1 <- c(paste("Giocatore 1",(vittorie1/(vittorie1+vittorie2))), paste("Giocatore 2",(vittorie2/(vittorie1+vittorie2))))
  pie(val1, labels = lab1, main = "giocatore")
  val2 <- c((absv1/(absv1+absv2)),(absv2/(absv1+absv2)))
  lab2 <- c(paste("Turno 1",(absv1/(absv1+absv2))), paste("Turno 2",(absv2/(absv1+absv2))))
  pie(val2, labels = lab2, main = "turno")
}

