library(testthat)
library(GWRLASSO)
test_that("these functions efficiently select relevant variables and accurately predict the value of response variable at unknown test locations", {
  set.seed(123)
  n<- 100
  p<- 7
  m<-sqrt(n)
  id<-seq(1:n)
  x<-matrix(runif(n*p), ncol=p)
  e<-rnorm(n, mean=0, sd=1)
  xy_grid<-expand.grid(c(1:m),c(1:m))
  Latitude<-xy_grid[,1]
  Longitude<-xy_grid[,2]
  B0<-(Latitude+Longitude)/6
  B1<-(Latitude/3)
  B2<-(Longitude/3)
  B3<-(2*Longitude)
  B4<-2*(Latitude+Longitude)/6
  B5<-(4*Longitude/3)
  B6<-2*(Latitude+Longitude)/18
  B7<-(4*Longitude/18)
  y<-B0+(B1*x[,1])+(B2*x[,2])+(B3*x[,3])+(B4*x[,4])+(B5*x[,5])+(B6*x[,6])+(B7*x[,7])+e
  data_spatial<-data.frame(y,x,Latitude,Longitude)
  result_exp<-GWRLASSO_exponential(data_spatial,0.8,0.7,exponential_kernel,10)
  result_gau<-GWRLASSO_gaussian(data_spatial,0.8,0.7,gaussian_kernel,10)
  expect_equal(round(result_exp$R_square,3),1)
  expect_equal(round(result_gau$R_square,3),1)
  })
