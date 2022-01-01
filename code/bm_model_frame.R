mf <- model.frame(mpg ~ disp + log(hp), data = mtcars, subset = cyl == 6) 
terms(mf)

# BM model_frame -------------
# Uses https://cran.r-project.org/web/packages/glmnetUtils/vignettes/intro.html 
library(microbenchmark)
library(hardhat)
makeSampleData <- function(N, P)
{
  X <- matrix(rnorm(N*P), nrow=N)
  data.frame(y=rnorm(N), X)
}

df1 <- makeSampleData(N=10000, P=100)
df2 <- makeSampleData(N=100000, P=100)
df3 <- makeSampleData(N=1000000, P=100)
edwards::object_size_all()

res <- microbenchmark(
  model.frame(y ~ ., df1), 
  model_frame(y ~ ., df1), 
  model.frame(y ~ ., df2), 
  model_frame(y ~ ., df2),
  model.frame(y ~ ., df3), 
  model_frame(y ~ ., df3),
  times=5
)
print(res, unit="s", digits=2)
plot(res)


mf1 <- model_frame(y ~ ., df1)
mf2 <- model_frame(y ~ ., df2)
mf3 <- model_frame(y ~ ., df3)

res2 <- microbenchmark(
  model_matrix(mf1$terms, mf1$data), 
  model_matrix(mf2$terms, mf2$data),
  model_matrix(mf3$terms, mf3$data),
  model.matrix(y ~ ., df3),
  times=5
)

print(res2, unit="s", digits=2)
plot(res2)

10000 * 10000
10000^2
