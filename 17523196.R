#1.
TB<-c(50,51,52,53,54)
BB<-c(40,46,44,55,49)


plot(TB,BB)
scatter.smooth(TB,BB)
#bagaimana model liniernya?
lm
#membuat regresi linier model
reg<-lm(BB~TB)
reg

#2.
predict(reg, data.frame(TB= 55))

#3.
x<- c(0,1,2,3,4)
y<- c(1,2.25,3.75,4.25,5.65)

library(polynom)
f<-poly.calc(x, y)
f

#4.
#INI SOAL DI SLIDE INTERPOLASI- BAGIAN POLINOM LAGRANGE
n=5
x<- c(0,1,2,3,4)
y<- c(1,2.25,3.75,4.25,5.65)
#tentukan polinom pada titik x=3,5 atau f(3.5) atau p(3.5)
px = 2.75
#L=lagrange
L = 0.0

for(i in 1:n){
  pembilang = 1.0
  penyebut = 1.0
  for(j in 1:n){
    if(i != j){
      pembilang = pembilang*(px - x[j])
      penyebut = penyebut * (x[i] -x[j])
    }
    j <- j+1
  }
  
  L = L + (pembilang/penyebut)*y[i]
  i <- i+1
}
print(paste("answer is " ,L))

#5.
plot(x,y)
curve(f, add=TRUE)

#13
h <- 0.1
x <- seq(0.1, by=h)
f <- function(x){
  return(x^2)
}
f0<- f(x[1])
fi <- sapply(x[2:10], f)
fn <- f(x[length(x)])

trap <- function(f0,fi,fn,h){
  L< - h *(f0+2*sum(fi)-1+fn)/2
  return(L)
}

trap(f0,fi,fn,h)