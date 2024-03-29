## Ajuste da Base de dados do Call Center
## Marcelo A Costa - 26/08/2020

 
rm(list=ls(all=TRUE))
 
 # Leitura do banco de dados
 dados <- read.csv2("bank-full.csv")
 
 # Manipulacao dos dados - Realiza a redefin��o da coluna da vari�vel categ�rica, pois o 
 # R considerou as vari�veis em ordem alfabetica, ao inv�s de utilizar a ordem
 # correta dos meses do ano.
 dados$month <- factor(dados$month,
                     levels = c("jan","feb","mar","apr","may",
                                "jun","jul","aug","sep","oct",
                                "nov","dec")) 
 
 
 # Identifica os primeiros dados da tabela
 head(dados)
 
 # Imprime o sum�rio do banco de dados, para compreender melhor as vari�veis
 summary(dados)
 
 # O comando table(y) me diz qual o total dos clientes que aceitaram o servi�o
 # e qual o total dos clientes n�o aceitaram. O resultado mostra que em torno de
 # 12% dos clientes aceitaram o servi�o. Isso faz com que a base seja desbalanceada,
 # pois a propor��o de um lado � muito maior que do outro. Como solu��o para esse
 # problema, podemos tanto inflar a base de dados para igualar as propor��es,
 # ou ent�o retirar uma quantidade de dados para igualar os lados.
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
 
 
 # A partir da an�lise da rela��o entre a vari�vel resposta (que � a aceita��o ou n�o
 # de comprar o servi�o), podemos obter algumas ideias iniciais do que poder� ocorrer
 # quando estudarmos nosso modelo de regress�o.
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
 
 # Como exemplo, podemos pegar a regress�o entre os meses e a aceita��o do
 # produto oferecido. Calculamos o valor da probabilidade de algu�m comprar
 # em determinado m�s atrav�s da f�rmula: exp(valor)/(1+exp(valor)).
 # Nesse caso, utilizamos:
 # valor = Estimate(Intercept) + Estimate(m�s)
 # Outra conclus�o que podemos retirar � de que o comportamento dos meses
 # onde o p-valor � extremamente alto, seu comportamento ser� similar ao
 # do m�s retirado como Intercept, que no caso do teste � janeiro.
 modelo <- glm(y ~ month, family=binomial, data=dados)
 summary(modelo)
 
 
 # Como o objetivo � estudar TODO o modelo, realizamos um ajuste do mesmo
 # utilizando TODAS as vari�veis poss�veis encontradas. Nas vari�veis
 # cont�nuas, consideramos que quanto maior for o valor (por exemplo a 
 # idade), ser� acrescida da probabilidade aquele valor do Estimate.
 # Lembrando que o Intercept foi calculado atrav�s da combina��o de diversas
 # vari�veis, que forma um tipo determinado de indiv�duo.
 # Para montar o melhor poss�vel modelo, no caso das categ�ricas, utilizamos
 # a configura��o que gerar� a melhor probabilidade de compra do servi�o.
 modelo <- glm(y ~ ., family=binomial, data=dados)
 summary(modelo)
 
 
 # Uma possibilidade � aplicar o step para que as vari�veis pouco representativas
 # sejam retiradas do modelo, para que a an�lise seja facilitada.
 modelo <- step(modelo)
 summary(modelo)
 
 
 # Outra possibilidade � montar o pr�prio modelo, selecionando a partir da
 # leitura dos dados quais vari�veis devem ser retiradas.
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
