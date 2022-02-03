## Example 2
set.seed(10)
alpha=0.20
m=500
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
ps[7]