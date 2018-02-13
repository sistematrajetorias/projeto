dados_alfredo_final=read.csv(file.choose(), header=T, sep = ',')
View(dados_alfredo_final)
base_final_cluster=dados_alfredo_final

base_final_cluster$cluster=as.factor(base_final_cluster$cluster)
base_final_cluster$EscPai=as.factor(base_final_cluster$EscPai)
base_final_cluster$Mora=as.factor(base_final_cluster$Mora)
base_final_cluster$TipRes=as.factor(base_final_cluster$TipRes)
base_final_cluster$AtivRemun=as.factor(base_final_cluster$AtivRemun)
base_final_cluster$ESCORE_BRUTO=as.numeric(base_final_cluster$ESCORE_BRUTO)
base_final_cluster$StatusFinal=as.factor(base_final_cluster$StatusFinal)

str(base_final_cluster$StatusFinal)


glm1=glm(StatusFinal ~ ESCORE_BRUTO + TipRes + AtivRemun, data=base_final_cluster,family= "binomial") 
summary(glm1)
confint(glm1) ## CIs using profiled log-likelihood
confint.default(glm1) ## CIs using standard errors
exp(coef(glm1)) ## odds ratios
exp(cbind(OR = coef(glm1), confint(glm1))) ## odds ratios and 95% CI

base_final_cluster$prob = predict(glm1, newdata = base_final_cluster, type = "response")
library(pROC)
area <- roc(StatusFinal ~ prob, data = base_final_cluster)
plot(area, main="Curva ROC do modelo logístico", xlab="Especificidade",ylab="Sensibilidade")

### UCLA
base_final_cluster2=base_final_cluster[!is.na(base_final_cluster$ESCORE_BRUTO),]
#View(base_final_cluster2)
min(base_final_cluster2$ESCORE_BRUTO)

glm2=glm(StatusFinal ~ ESCORE_BRUTO + AtivRemun, data=base_final_cluster2,family= "binomial") 

newdata1 <- with(base_final_cluster2,
                 data.frame(ESCORE_BRUTO = mean(ESCORE_BRUTO), AtivRemun = factor(1:4)))
newdata1$rankP <- predict(glm2, newdata = newdata1, type = "response")
newdata1
newdata2 <- with(base_final_cluster2,
                 data.frame(ESCORE_BRUTO = rep(seq(from = -6.2, to = 243, length.out = 50), 4),
                            AtivRemun = factor(rep(1:4, each = 50))))
newdata3 <- cbind(newdata2, predict(glm2, newdata = newdata2, type="link", se=TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
library(ggplot2)
library(plotly)
ggplot(newdata3, aes(x = ESCORE_BRUTO, y = PredictedProb)) +
  labs(x="Escore Bruto",y="Probabilidade")+
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = AtivRemun), alpha = .2) +
  geom_line(aes(colour = AtivRemun), size=1)

base_final_cluster_est=base_final_cluster[base_final_cluster$ABI=="Estatística",]
base_final_cluster_mat=base_final_cluster[base_final_cluster$ABI=="Matemática",]
base_final_cluster_cic=base_final_cluster[base_final_cluster$ABI=="Ciência da Computação",]

