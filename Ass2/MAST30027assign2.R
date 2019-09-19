dataset = read.delim("assign2.txt", header = TRUE, sep = " ")
dataset['prosocial_action'] = (dataset$prosoc_left == dataset$pulled_left)
data1 = dataset[dataset$actor == 1,]
data2 = dataset[dataset$actor == 2,]
data3 = dataset[dataset$actor == 3,]
data4 = dataset[dataset$actor == 4,]
data5 = dataset[dataset$actor == 5,]
data6 = dataset[dataset$actor == 6,]
data7 = dataset[dataset$actor == 7,]

x = xtabs( ~ prosoc_left + prosocial_action + condition, data = dataset)

data.frame(x)$Freq

##
model1 = glm(Freq~prosoc_left + prosocial_action + condition, family = poisson,data = x )
deviance(model1)
df.residual(model1)
pchisq(deviance(model1), df = df.residual(model1), lower.tail = FALSE)

##
model2 = glm(Freq~prosoc_left * prosocial_action + condition, family = poisson,data = x )
deviance(model2)
df.residual(model2)
pchisq(deviance(model2), df = df.residual(model2), lower.tail = FALSE)

#2-way
x = xtabs( ~  prosocial_action + condition, data = dataset)
x = data.frame(x)
(x)
model0 = glm(Freq~prosocial_action + condition, family = poisson,data = x )
deviance(model0)
df.residual(model0)
(pchisq(deviance(model0), df = df.residual(model0), lower.tail = FALSE))


prosocial_percentage = xtabs( ~ actor,prosocial_action, data = dataset)
prosocial_percentage = transform(prosocial, Freq=Freq/72)
ggplot(data=prosocial_percentage, aes(x=actor, y=Freq)) + geom_bar(stat="identity", color="blue", fill="blue",width = 0.5)+ geom_hline(yintercept=0.5, linetype=1, color = "red",size = 2)

#actor
x = xtabs( ~ prosoc_left + condition + pulled_left, data = dataset)
x = data.frame(x)
(x)
model0 = glm(Freq~prosoc_left + condition + pulled_left, family = poisson,data = x )
deviance(model0)
df.residual(model0)
(pchisq(deviance(model0), df = df.residual(model0), lower.tail = FALSE))



#actor1
x = xtabs( ~ prosoc_left + condition + pulled_left, data = data1)
x = data.frame(x)
(x)
model0 = glm(Freq~prosoc_left + condition + pulled_left, family = poisson,data = x )
deviance(model0)
df.residual(model0)
(pchisq(deviance(model0), df = df.residual(model0), lower.tail = FALSE))

#actor2
x = xtabs( ~ prosoc_left + condition + pulled_left, data = data2)
x = data.frame(x)
(x)
model0 = glm(Freq~prosoc_left + condition + pulled_left, family = poisson,data = x )
deviance(model0)
df.residual(model0)
(pchisq(deviance(model0), df = df.residual(model0), lower.tail = FALSE))

#actor3
x = xtabs( ~ prosoc_left + condition + pulled_left, data = data3)
x = data.frame(x)
(x)
model0 = glm(Freq~prosoc_left + condition + pulled_left, family = poisson,data = x )
deviance(model0)
df.residual(model0)
(pchisq(deviance(model0), df = df.residual(model0), lower.tail = FALSE))
#actor4
x = xtabs( ~ prosoc_left + condition + pulled_left, data = data4)
x = data.frame(x)
(x)
model0 = glm(Freq~prosoc_left + condition + pulled_left, family = poisson,data = x )
deviance(model0)
df.residual(model0)
(pchisq(deviance(model0), df = df.residual(model0), lower.tail = FALSE))

#actor5
x = xtabs( ~ prosoc_left + condition + pulled_left, data = data5)
x = data.frame(x)
(x)
model0 = glm(Freq~prosoc_left + condition + pulled_left, family = poisson,data = x )
deviance(model0)
df.residual(model0)
(pchisq(deviance(model0), df = df.residual(model0), lower.tail = FALSE))

#actor6
x = xtabs( ~ prosoc_left + condition + pulled_left, data = data6)
x = data.frame(x)
(x)
model0 = glm(Freq~prosoc_left + condition + pulled_left, family = poisson,data = x )
deviance(model0)
df.residual(model0)
(pchisq(deviance(model0), df = df.residual(model0), lower.tail = FALSE))

#actor7
x = xtabs( ~ prosoc_left + condition + pulled_left, data = data7)
x = data.frame(x)
(x)
model0 = glm(Freq~prosoc_left + condition + pulled_left, family = poisson,data = x )
deviance(model0)
df.residual(model0)
(pchisq(deviance(model0), df = df.residual(model0), lower.tail = FALSE))



#actor
x = xtabs( ~ actor + condition + prosocial_action, data = dataset)
x = data.frame(x)

model0 = glm(Freq~actor + condition + prosocial_action, family = poisson,data = x )
deviance(model0)
df.residual(model0)
pchisq(deviance(model0), df = df.residual(model0), lower.tail = FALSE)
summary(xtabs( ~ actor + condition + prosocial_action, data = dataset))




x = xtabs( ~ prosoc_left + condition + pulled_left, data = dataset)
x = data.frame(x)

#using poisson regression to build model
model0 = glm(Freq~prosoc_left + condition + pulled_left, family = poisson,data = x )
model1 = glm(Freq~prosoc_left + condition + pulled_left + pulled_left * prosoc_left, family = poisson,data = x )

deviance(model0)
df.residual(model0)
pchisq(deviance(model0), df = df.residual(model0), lower.tail = FALSE)

deviance(model1)
df.residual(model1)
pchisq(deviance(model1), df = df.residual(model1), lower.tail = FALSE)
anova(model0, model1, test = "Chisq")

x = xtabs( ~ prosoc_left + condition + pulled_left, data = dataset)












newdata = dataset[dataset$actor =! 2,]

# generate data for champanzee 1
x = data.frame(xtabs( ~ prosoc_left + condition + pulled_left, data = dataset))

#using poisson regression to build model
model = glm(Freq~prosoc_left + condition + pulled_left, family = poisson,data = x )
deviance(model)
df.residual(model)
pchisq(deviance(model), df = df.residual(model), lower.tail = FALSE)








#test all 3 factor are independent
x = xtabs( ~ prosocial_action + condition + actor, data = dataset)
model1 = glm(Freq~ prosocial_action + condition + actor, family = poisson,data = x )
deviance(model1)
pchisq(deviance(model1), df = df.residual(model1), lower.tail = FALSE)
#0.9598556, model not enrough

#test if actor is independent of prosocial_action and condition 
model2 = glm(Freq~ prosocial_action * condition + actor, family = poisson,data = x )
deviance(model2)
pchisq(deviance(model2), df = df.residual(model2), lower.tail = FALSE)

#test if prosocial_action and condition conditionally independent of actor.
model3 = glm(Freq~ prosocial_action * actor + condition * actor, family = poisson,data = x )
deviance(model3)
pchisq(deviance(model3), df = df.residual(model3), lower.tail = FALSE)

#test larger model
model4 = glm(Freq~ (prosocial_action + actor + condition)^2 , family = poisson,data = x )
deviance(model4)
pchisq(deviance(model4), df = df.residual(model4), lower.tail = FALSE)

anova(model3,model4,test = "Chisq")


library(ggplot2)
prosocial = xtabs( ~ actor,prosocial_action, data = dataset[dataset$condition==1,])
(prosocial_percentage = transform(prosocial, Freq=Freq/36))


# generate the bar plot
ggplot(data=prosocial_percentage, aes(x=actor, y=Freq,fill = condition))+
  geom_bar(aes(fill = condition),stat="identity", color="blue", fill="blue",width = 0.5)+
  geom_hline(yintercept=0.5, linetype=1, color = "red",size = 2)+
  ggtitle("Plot of the percentage of prosocial action for each actor")



library(ggplot2)
ggplot(data=prosocial_percentage,aes(x=actor,y=Freq,fill=condition))+
  geom_bar(stat="identity",position="dodge")+
  ggtitle("Plot of the percentage of prosocial action for each actor")+
  geom_hline(yintercept=0.5, linetype=2, color = "red",size = 1)


prosocial = xtabs(prosocial_action ~ condition, data = dataset)
(prosocial_percentage = transform(prosocial, Freq=Freq/252))

# generate the bar plot
library(ggplot2)
ggplot(data=prosocial_percentage,aes(x=condition,y=Freq))+
  geom_bar(stat="identity",position="dodge")+
  ggtitle("Plot of the percentage of prosocial action for chimpanzees")
  