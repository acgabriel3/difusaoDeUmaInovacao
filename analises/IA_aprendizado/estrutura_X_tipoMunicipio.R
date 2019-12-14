#***
#CAMINHO
#

library(caret)
#source()

#***
#Aqui iniciarei a tentativa de construcao de um modelo de aprendizado de maquinas, com a tabela gerada
#pela fucao geraMedidas(). 

#***
#uma observacao importante, eh que toda execucao deve ser organizada de acordo com a floresta, ou seja,
#o diretorio raiz

cidades <- cidades[,-1]

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(cidades$classificacao, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- cidades[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- cidades[validation_index,]

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# a) linear algorithms
set.seed(7)
fit.lda <- train(classificacao~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(classificacao~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(classificacao~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(classificacao~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(classificacao~., data=dataset, method="rf", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
dotplot(results)

print(fit.svm)

# estimate skill of LDA on the validation dataset
predictions <- predict(fit.svm, validation)
confusionMatrix(predictions, validation$classificacao)

sum(predictions == "Centro Local", na.rm = TRUE)

#Medidas possiveis:
#normalizacao dos valores
#diminuir quantidades de fatores
#Equilibrar observacoes para cada tipo de fator

#Lembrando que: Os resultados de um aprendizado de maquina dependem em muito dos dados de entrada, e seu formato


#para o kmens: 

cidades_sem_classificacao <- cidades[,c(-1, -6)]

set.seed(20)

modelo <- kmeans(cidades_sem_classificacao, 17 , nstart = 20) #Entao pode-se comparar com as classificacoes reais

#Fazendo uma comparacao visual:

library(ggplot2)

ggplot(cidades, aes(grau, classificacao, color = cidades$classificacao)) + geom_point()
ggplot(cidades, aes(betweenness, grau, color = as.factor(modelo$cluster))) + geom_point()
















