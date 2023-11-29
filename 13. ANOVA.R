responce=c(2,3,4,4,5,6,6,7,8)
x=rep(c("model1","model2","model3"),c(3,3,3))
levels=factor(x)
ano.result=aov(responce~levels)
summary(ano.result)

# F분포표에서 드러난 것과 같이 유의수준 0.1보다 낮은 0.008값이 측정되었으므로 귀무가설을 기각한다.