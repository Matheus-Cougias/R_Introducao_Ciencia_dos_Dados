## Ajuste da Base de dados do Call Center
## Marcelo A Costa - 26/08/2020

 
rm(list=ls(all=TRUE))
 
 # Leitura do banco de dados
 dados <- read.csv2("bank-full.csv")
 
 # Manipulacao dos dados - Realiza a redefinção da coluna da variável categórica, pois o 
 # R considerou as variáveis em ordem alfabetica, ao invés de utilizar a ordem
 # correta dos meses do ano.
 dados$month <- factor(dados$month,
                     levels = c("jan","feb","mar","apr","may",
                                "jun","jul","aug","sep","oct",
                                "nov","dec")) 
 
 
 # Identifica os primeiros dados da tabela
 head(dados)
 
 # Imprime o sumário do banco de dados, para compreender melhor as variáveis
 summary(dados)
 
 # O comando table(y) me diz qual o total dos clientes que aceitaram o serviço
 # e qual o total dos clientes não aceitaram. O resultado mostra que em torno de
 # 12% dos clientes aceitaram o serviço. Isso faz com que a base seja desbalanceada,
 # pois a proporção de um lado é muito maior que do outro. Como solução para esse
 # problema, podemos tanto inflar a base de dados para igualar as proporções,
 # ou então retirar uma quantidade de dados para igualar os lados.
 table(dados$y)/sum( table(dados$y) )
 dados$y <- as.factor(dados$y)
 dados$job <- as.factor(dados$job)
 dados$marital <- as.factor(dados$marital)
 dados$education <- as.factor(dados$education)
 dados$default <- as.factor(dados$default)
 dados$housing <- as.factor(dados$housing)
 dados$loan <- as.factor(dados$loan)
 dados$contact <- as.factor(dados$contact)
 dados$day <- as.factor(dados$day)
 dados$month <- as.factor(dados$month)
 dados$poutcome <- as.factor(dados$poutcome)
 
 
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
 plot(previous ~ y, data=dados, pch=19, col="light blue", outline=TRUE)
 plot(y ~ poutcome, data=dados)


 ## - - - - - - - - - - - - - - - - - - - - -
 ## Ajuste do Modelo de Regressao Logistico
 
 # Como exemplo, podemos pegar a regressão entre os meses e a aceitação do
 # produto oferecido. Calculamos o valor da probabilidade de alguém comprar
 # em determinado mês através da fórmula: exp(valor)/(1+exp(valor)).
 # Nesse caso, utilizamos:
 # valor = Estimate(Intercept) + Estimate(mês)
 # Outra conclusão que podemos retirar é de que o comportamento dos meses
 # onde o p-valor é extremamente alto, seu comportamento será similar ao
 # do mês retirado como Intercept, que no caso do teste é janeiro.
 modelo <- glm(y ~ month, family=binomial, data=dados)
 summary(modelo)
 
 
 # Como o objetivo é estudar TODO o modelo, realizamos um ajuste do mesmo
 # utilizando TODAS as variáveis possíveis encontradas. Nas variáveis
 # contínuas, consideramos que quanto maior for o valor (por exemplo a 
 # idade), será acrescida da probabilidade aquele valor do Estimate.
 # Lembrando que o Intercept foi calculado através da combinação de diversas
 # variáveis, que forma um tipo determinado de indivíduo.
 # Para montar o melhor possível modelo, no caso das categóricas, utilizamos
 # a configuração que gerará a melhor probabilidade de compra do serviço.
 modelo <- glm(y ~ ., family=binomial, data=dados)
 summary(modelo)
 
 
 # Uma possibilidade é aplicar o step para que as variáveis pouco representativas
 # sejam retiradas do modelo, para que a análise seja facilitada.
 modelo <- step(modelo)
 summary(modelo)
 
 
 # Outra possibilidade é montar o próprio modelo, selecionando a partir da
 # leitura dos dados quais variáveis devem ser retiradas.
 modelo <- glm(y ~ .-age-default-pdays-previous, 
               family=binomial, data=dados)
 summary(modelo)

 with( modelo, 1-pchisq(deviance, df.residual) ) 

 dados$fit <- predict(modelo, type="response")
 plot(fit ~ y, data=dados)

 ## Analise de Sensibilidade/Especificidade
 require(pROC)

 curva.roc <- roc(dados$y, predict(modelo, type="response") )
 plot(curva.roc, col="red")
 curva.roc$auc

 with(curva.roc, plot(sensitivities ~ thresholds, 
                      ylab="prob. classificacao", 
                      type="l", col="red", lwd=2) )
 grid()
 with(curva.roc, lines(specificities ~ thresholds, 
                       col="blue", lty=2, lwd=2) )
 legend("topright", legend=c("no","yes"), lty=c(2,1), lwd=2,
        col=c("blue","red"), bg="white", cex=1.5, bty="n")
 

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
