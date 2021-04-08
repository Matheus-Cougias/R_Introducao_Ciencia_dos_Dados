## Ajuste da Base de dados do Call Center
## Marcelo A Costa - 26/08/2020

 rm(list=ls(all=TRUE))
 require(packHV)
 
 # Leitura do banco de dados
 dados <- read.delim2("bank-full.csv", sep=";")
 
 # Manipulacao dos dados - Realiza a redefinção da coluna da variável categórica, pois o 
 # R considerou as variáveis em ordem alfabetica, ao invés de utilizar a ordem
 # correta dos meses do ano.
 dados$month <- factor(dados$month,
                     levels = c("jan","feb","mar","apr","may",
                                "jun","jul","aug","sep","oct",
                                "nov","dec")) 
 # Tranformação das variáveis que devem ser consideradas como fator.
 dados$y         <- as.factor(dados$y)
 dados$job       <- as.factor(dados$job)
 dados$marital   <- as.factor(dados$marital)
 dados$education <- as.factor(dados$education)
 dados$default   <- as.factor(dados$default)
 dados$housing   <- as.factor(dados$housing) 
 dados$loan      <- as.factor(dados$loan) 
 dados$contact   <- as.factor(dados$contact) 
 dados$poutcome  <- as.factor(dados$poutcome) 
 
 ## Apenas uma constatacao...
 table(dados$y)/sum( table(dados$y) )
 
 # A partir da análise da relação entre a variável resposta (que é a aceitação ou não
 # de comprar o serviço), podemos obter algumas ideias iniciais do que poderá ocorrer
 # quando estudarmos nosso modelo de regressão.
 plot(age ~ y, data=dados, pch=19, col="light blue")
 plot(y ~ job, data=dados)
 plot(y ~ marital, data=dados)
 plot(y ~ education, data=dados)
 plot(y ~ default, data=dados)
 plot(balance ~ y, data=dados, pch=19, col="light blue", outline=TRUE)
 plot(y ~ housing, data=dados)
 plot(y ~ loan, data=dados)
 plot(y ~ contact, data=dados)
 plot(y ~ day, data=dados)
 plot(y ~ as.factor(day), data=dados)
 plot(y ~  month, data=dados)
 plot(duration ~ y, data=dados)
 plot(campaign ~ y, data=dados, pch=19, col="light blue", outline=TRUE)
 plot(pdays ~ y, data=dados, pch=19, col="light blue", outline=TRUE)
 plot(previous ~ y, data=dados, pch=19, col="light blue", outline=FALSE)
 plot(y ~ poutcome, data=dados)

 dados$day <- as.factor(dados$day)

 ## - - - - - - - - - - - - - - - - - - - - -
 ## lm(x ~ y + relevel(b, ref = "3"))
 ## Ajuste do Modelo de Regressao Logistico
 
 # Sabendo da correlação entre a duração da chamada e a probabilidade do cliente aceitar
 # a compra do serviço, podemos avaliar de maneira separada a variável duration
 modelo <- glm(y ~ duration, family=binomial, data=dados)
 summary(modelo)
 
 # Pode-se fazer o mesmo no caso dos meses do ano, para detectar qual desses melhor
 # apresenta resultados
 modelo <- glm(y ~ month, family=binomial, data=dados)
 summary(modelo)
 
 modelo <- glm(y ~ relevel(month, ref = "dec"), 
               family=binomial, data=dados)
 summary(modelo)
 
 modelo <- glm(y ~ ., family=binomial, data=dados)
 summary(modelo)
 
 ## Selecao de variaveis
 modelo <- step(modelo)
 summary(modelo)
 
 ## Resultado Final
 modelo <- glm(y ~ .-age-default-pdays-previous, 
               family=binomial, data=dados)
 summary(modelo)

 with( modelo, 1-pchisq(deviance, df.residual) ) 

 dados$fit <- predict(modelo, type="response")
 hist_boxplot(dados$fit)
 
 ## res  <- residuals(modelo, type="deviance")
 ## prob <- dados$fit
 ## plot(res ~ prob)
 
 plot(modelo, lty=0)
 

 ## Analise de Sensibilidade/Especificidade
 # O primeiro método para analisar se um modelo dado é melhor que o outro, é utilizando
 # a curva ROC. O melhor modelo é aquele que a curva mais se aproximar dos eixos em
 # direção aos pontos (1,0) e (0,1). 
 
 require(pROC)

 curva.roc <- roc(modelo$y, predict(modelo, type="response") )
 plot(curva.roc, col="red")
 
 # Ao gerar a curva, para determinar o quão abrangente é o modelo gerado, pode-se
 # tomar o parâmetro auc, que determina a área sobre a curva, que no caso foi de
 # 0.9079. 
 curva.roc$auc

 # Passos para gerar os gráficos de sensitividade e especificidade
 with(curva.roc, plot(sensitivities ~ thresholds, 
                      ylab="prob. classificacao", 
                      type="l", col="red", lwd=2) )
 grid()
 with(curva.roc, lines(specificities ~ thresholds, 
                       col="blue", lty=2, lwd=2) )
 legend("topright", legend=c("no","yes"), lty=c(2,1), lwd=2,
        col=c("blue","red"), bg="white", cex=1.5, bty="n")
 
 # Modo de encontrar a interseção das curvas, retornando a proporção de acerto nesse ponto
 # e a limiar para que o aceto seja igual nessas classes
 pos       <- which.min( with(curva.roc, sqrt( (specificities - sensitivities)^2 ) )  ) 
 curva.roc$sensitivities[pos] ## Sensitivity == Specificity
 curva.roc$thresholds[pos]
 table(dados$y)/sum(table(dados$y))
 
 
 ## - - - - - - - - - - - - - - - - - - - 
 ## Validacao Cruzada
 ## Create 10 equally size folds...
 ## folds must be created to guarantee "cases" in the training fold...
 
 # Ainda é muito comum realizar uma validação cruzada, utilizando a curva ROC de uma
 # amostra dos dados originais, Primeiramente o banco de dados é dividido aleatoriamente
 # em 10 pedaços.
 
 # Para cada uma das amostras o programa realizará uma estimativa
 folds  <- cut( seq(1,nrow(dados)), breaks=10, labels=FALSE)
 folds  <- sample(folds, size=length(folds)) ## Randomly shuffle the folds
 Y      <- modelo$y
 Yfolds <- rep(NA, nrow(dados))
 for(fld in 1:10){
         #Segment your data by "fold" using the which() function 
         testIndexes <- which(folds==fld, arr.ind=TRUE)
         
         train <- dados[-testIndexes,]; 
         test  <- dados[testIndexes,];  
         
         model.glm <- glm(y ~ .-age-default-pdays-previous, 
                          family=binomial, data=train)

         out <- predict(model.glm, newdata=test, type="response")
         Yfolds[testIndexes] <- out
 }
 
 # Assim, podemos calcular o valor da curva ROC para o modelo de validação cruzada,
 # dando um valor bem próximo do modelo inicial.
 sum(!is.na(Yfolds))
 curve.roc <- roc(Y, Yfolds)
 plot(curve.roc, col="dark green")
 pos       <- which.min( with(curve.roc, sqrt( (specificities - sensitivities)^2 ) )  ) 
 curve.roc$sensitivities[pos] ## Sensitivity == Specificity
 curva.roc$thresholds[pos]

 
 
 ## - - - - - - - - - - - - - - - - - - - 
 ## Comparacao com "randomForest" (Modelo de Machine Learning)
 require(randomForest)
 set.seed(1234)
 modFit      <- randomForest(y ~ . -age-default-pdays-previous, data=dados)
 curva.rocRF <- roc(dados$y, predict(modFit, type="prob")[,2] )
 plot(curva.rocRF, col="dark green")


 ## Compara as duas curvas ROC
 plot(curva.roc, col="red")
 plot(curva.rocRF, col="blue", add=T, lty=2)
 legend("bottomright", legend=c("Logistico", "RandomForests"),
        lwd=2, lty=c(1,2), col=c("red", "blue"), cex=1.4, bty="n")
