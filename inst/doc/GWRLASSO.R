## -----------------------------------------------------------------------------
# Examples: Variable selection and prediction at unknown test locations using GWRLASSO hybrid spatial model 

# Generation of response variable and predictor variables as well as the locational coordinates 

library(GWRLASSO)
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
data_sp<-data.frame(y,x,Latitude,Longitude)
head(data_sp)

# Application of the GWRLASSO model with the exponential kernel function

library(GWRLASSO)
GWRLASSO_exp<-GWRLASSO_exponential(data_sp,0.8,0.7,exponential_kernel,10)
GWRLASSO_exp

# Application of the GWRLASSO model with the gaussian kernel function

library(GWRLASSO)
GWRLASSO_gau<-GWRLASSO_gaussian(data_sp,0.8,0.7,gaussian_kernel,10)
GWRLASSO_gau

