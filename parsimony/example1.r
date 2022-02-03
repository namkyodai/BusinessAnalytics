## Example 1
## We specify a seed to make the results reproducible. Omitting the
## set.seed statement would lead to a different set of random numbers
## and the results would vary somewhat
set.seed(10)
alpha=0.10
m=100
p=dim(m)
index=dim(m)
for (i in 1:5) {
  x=rnorm(25,1,1)
  t=-abs(mean(x)/(sd(x)/sqrt(25)))
  p[i]=2*pt(t,24)
  index[i]=i
}
for (i in 6:m) {
  x=rnorm(25)
  t=-abs(mean(x)/(sd(x)/sqrt(25)))
  p[i]=2*pt(t,24)
  index[i]=i
}
count=p<=0.05
table(count)
ps=sort(p)
logps=log(ps)
logindex=log(index)
y=log(index*alpha/m)
plot(logps~logindex,xlab="log(j)",ylab="log(ProbValue(j))",main="False
Discovery Rate")
points(y~logindex,type="l")
ps
ps[6]

