library(ggplot2)

set.seed(5)
rand.class <- function(matrix.count=2000, max=100) {
  a <- floor(runif(matrix.count, min=0, max=max))
  b <- floor(runif(matrix.count, min=0, max=max))
  c <- floor(runif(matrix.count, min=0, max=max))
  d <- floor(runif(matrix.count, min=0, max=max))
  matrix.data <- data.frame(a,b,c,d)
  colnames(matrix.data) <- c('a','b','c','d')
  return(matrix.data)
}

calc.f1 <- function(vector) {
  a <- vector[1]
  b <- vector[2]
  c <- vector[3]
  d <- vector[4]
  f1 <- (a^2+a*b+c*d+d^2)/(a+b+c+d)^2
  return(f1)
}

calc.comp1 <- function(vector) {
  a <- vector[1]
  b <- vector[2]
  c <- vector[3]
  d <- vector[4]
  comp <- (a*c+b*d+2*a*d)/(a+b+c+d)^2
  return(comp)
}

calc.acc <- function(vector) {
  a <- vector[1]
  b <- vector[2]
  c <- vector[3]
  d <- vector[4]
  acc <- (a+d)/(a+b+c+d)
  return(acc)
}

df <- rand.class()
df <- cbind(df,
            f1=apply(df, 1, calc.f1),
            accuracy=apply(df, 1, calc.acc),
            comp1=apply(df, 1, calc.comp1),
            se=(apply(df, 1, calc.acc)-apply(df, 1, calc.f1))^2)

ggplot(df, aes(x=accuracy,y=f1, color=comp1)) + 
  geom_point() +
  labs(x="accuracy",
       y="support-weighted mean F1",
       color="SWM F1 component") +
  scale_color_gradient(low="#13223a", high="#b3c2d8") +
  geom_abline(slope=1, intercept=0)

x<-df[(df$accuracy>.9),]
y<-x[x$se<.1,]
y
