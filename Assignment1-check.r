pdf_function <- function(x) (1/(1*2*3*4*5))* x^5*exp(-x)

library(coda)
library(ggplot2)


Metro_hast <- function(steps ,starting_point, p ){
  #create our vectors 
  current_point <- starting_point
  
  for(i in 2:steps){
    currentX <- current_point[i-1]
    X_to_evaluate <- rlnorm(1, meanlog = log(currentX) , sdlog = p)
    Uniform_var <- runif(1)
    
    ratio_numerator <- (pdf_function(X_to_evaluate)* dlnorm(currentX, meanlog = log(X_to_evaluate), p) )
    denominator <- (pdf_function(currentX)*dlnorm(X_to_evaluate,meanlog=log(currentX), p))
    
    alpha <- min(1 , ratio_numerator / denominator)
    
    if(Uniform_var <= alpha)current_point[i] <- X_to_evaluate
    
    else current_point[i] <- currentX
  }
  
  (-1)*current_point
  
}



f<-Metro_hast(5000,1,1)
df_1 <- as.data.frame(f)
df_1$ID <- 1:5000

ggplot(df_1 , aes(x = ID , y = f))+geom_line()


########
#### 1.2 


Metro_hast_chi <- function(steps ,starting_point ){
  #create our vectors 
  current_point <- starting_point
  
  for(i in 2:steps) {
    currentX <- current_point[i-1]
    
    X_to_evaluate <- rchisq(1, df=floor(currentX +1))
    Uniform_var <- runif(1)
    
    ratio_numerator <- pdf_function(X_to_evaluate)*dchisq(1,df=floor(X_to_evaluate+1))
    denominator <- pdf_function(currentX)*dchisq(1, df=floor(X_to_evaluate+1))
    
    alpha <- min(1 , ratio_numerator / denominator)
    
    if(Uniform_var <= alpha)current_point[i] <- X_to_evaluate
    else current_point[i] <- currentX
    
    
    
  }
  current_point
  
  
}

sample2<-Metro_hast_chi(5000,rchisq(1,1))

df_2 <- as.data.frame(sample2)
df_2$ID <- 1:5000

ggplot(df_2 , aes(x = ID , y = sample2))+geom_line()




###############
#1.3

Matrix_1<-matrix(,10,2000)

for(i in 1:10){
  Matrix_1[one_to_ten,] <- Metro_hast_chi(2000, i )
}

Gelman_list <- list()

for (i in 1:10) {
  
  Gelman_list[[i]]<- as.mcmc(Matrix_1[i,])
  
}
gelman.diag(Gelman_list, confidence = 0.95, transform=FALSE, autoburnin=TRUE,
            multivariate=TRUE)



gelman.plot(Gelman_list, bin.width = 10, max.bins = 50,
            confidence = 0.95)



###############
##### 1.4 #####
###############

draw_1 <--Metro_hast(5000,rlnorm(1,0,1),1)
mean(draw_1)


draw_2 <- Metro_hast_chi(5000,rchisq(1,floor(1)))
mean(draw_2)