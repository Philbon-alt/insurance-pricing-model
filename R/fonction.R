EQM<-function(pred, vrai) {
  return (mean((pred-vrai)^2))
}

graphPois<-function(lambda){
  xbetchkvkuvk<-seq(from=0,to=lambda+max(3,lambda),length.out=2000)
  bbbbbbbfhgugg<-dpois(floor(xbetchkvkuvk), lambda)
  plot(bbbbbbbfhgugg, type="l")
}

train_test_split <- function(prop,data) {
  id <- sample(nrow(data))
  nent <- floor(prop*nrow(data))
  ent_data <- data[id[1:nent],]
  tes_data <- data[id[(nent+1):nrow(data)],]
  
  return(list(ent_data,tes_data))
}

precision <- function(pred, vrai){
  return(sum(pred==vrai)/length(pred))
}

GenGroup <- function(n,prob,centres){
  
  #Dimension des données  
  d <- ncol(centres)
  
  #nombres de groupes
  c <- nrow(centres)
  data <- matrix(data=rep(0,n*d),nrow=n)
  
  #number of each class
  nc <- rmultinom(1, n, prob)
  start=0
  assign <- rep(0,n)
  
  for (j in 1:c){
    assign[(start+1):(start+nc[j])] <-j
    start=start+nc[j]
  }
  
  start=0
  #En i on fait le tour de nos groupes et en j le tours de nos dimensions
  for (i in 1:c){
    for (j in 1:d){
      data[(start+1):(start+nc[i]),j] <- rnorm(nc[i],mean=centres[i,j])
    }
    start=start+nc[i]
  }
  
  return(list(data,assign))
}

kmean <- function(data,k,iter) {
  
  #assignement aléatoire
  assignements <- sample(1:k,nrow(data),replace=TRUE)
  
  for (j in 1:iter){
    centres <- Calcul_Centre(data,k,assignements)
    assignements <- Calcul_Assign(data,k,centres)
  }
  
  return(list(centres,assignements))
}

Calcul_Centre <- function(data,k,assignements){
  
  d <- ncol(data)
  centres <- matrix(data=rep(0,d*k),nrow=k)
  
  for (j in 1:k){
    centres[j,] <- colSums(data[assignements==j,])/sum(assignements==j)
  }
  
  
  return(centres)
}

Calcul_Assign <- function(data,k,centres){
  
  assignements <- rep(0,nrow(data))
  distance <- matrix(rep(0,nrow(data)*k),nrow=nrow(data))
  
  for (i in 1:nrow(data)){
    for (j in 1:k){
      distance[i,j] <- sqrt(sum((data[i,]-centres[j,])^2))
    }
    assignements[i] <- which.min(distance[i,])
  }
  return(assignements)
}

Calcul_Assign2 <- function(data,k,centres){
  
  assignements <- rep(1,nrow(data))
  
  for (i in 1:nrow(data)){
    distance <- sqrt(sum((data[i,]-centres[1,])^2))
    for (j in 2:k){
      if (sqrt(sum((data[i,]-centres[j,])^2))<distance){
        distance <- sqrt(sum((data[i,]-centres[j,])^2))
        assignements[i] <- j
      }
      
    }
  }
  return(assignements)
}

kmeans_custom <- function(data, k, max_iter = 100) {
  # Randomly initialize cluster centroids
  set.seed(123)  # Set a seed for reproducibility
  centroids <- data[sample(1:nrow(data), k), ]
  
  for (iter in 1:max_iter) {
    # Assign each data point to the nearest centroid
    distances <- sapply(1:k, function(i) {
      rowSums((data - centroids[i, ])^2)
    })
    cluster_assignments <- apply(distances, 1, which.min)
    
    # Update centroids
    new_centroids <- t(sapply(1:k, function(i) {
      if (sum(cluster_assignments == i) > 0) {
        colMeans(data[cluster_assignments == i, ])
      } else {
        centroids[i, ]  # Keep the centroid unchanged if no points assigned
      }
    }))
    
    # Check for convergence
    if (identical(centroids, new_centroids)) {
      break
    }
    
    centroids <- new_centroids
  }
  
  # Return the final cluster assignments and centroids
  result <- list(cluster_assignments = cluster_assignments, centroids = centroids)
  return(result)
}

Select_alpha <- function(alpha,x_train, y_train, x_val,y_val){
  nalpha = length(alpha)
  MSE <- rep(0,nalpha)
  lambda <- rep(0,nalpha)
  for (i in 1:nalpha){
    
    
    cvfit <- cv.glmnet(x_train, y_train, alpha = alpha[i], lambda = grid)
    
    
    lambda[i] <- cvfit$lambda.min
    pred <- predict(cvfit,s=bestlam,newx=x_val)
    
    
    MSE[i] <- EQM(pred,y_val)
  }
  
  return(list(alpha[which.min(MSE)],lambda[which.min(MSE)],MSE[which.min(MSE)],MSE))
}

analise <- function(series, x) {
  par(mfrow = c(3, 1))
  
  main <- glue("Autocorrélogramme de {x}")
  acf(series, lag.max = 40, main = main)
  
  main <- glue("Autocorrélogramme partiel de {x}")
  pacf(series, lag.max = 40, main = main)
  
  main <- glue("Périodogramme de {x}")
  spectrum(series, log = "no", plot = T, main = main)
  
  Box.test(series, lag = 40)
}

TableauAIC <- function(series, p_max=6, q_max=6) {
  AIC_table <- matrix(NA, nrow = p_max + 1, ncol = q_max + 1)
  for (p in 0:p_max) {
    for (q in 0:q_max) {
      tryCatch({
        model <- arima(series, order = c(p, 0, q), method = "ML", optim.control = list(maxit = 1000))
        AIC_table[p + 1, q + 1] <- model$aic
      }, warning = function(w) {
        cat("Avertissement pour p =", p, "q =", q, ":", conditionMessage(w), "\n")
        AIC_table[p + 1, q + 1] <- NA
      }, error = function(e) {
        cat("Erreur pour p =", p, "q =", q, ":", conditionMessage(e), "\n")
        AIC_table[p + 1, q + 1] <- NA
      })
    }
  }
  return(AIC_table)
}