library(specmine)
load('.RData')

### An�lise preditiva com machine learning

# Para a previs�o de dados usando machine learning, decidimos fazer tr�s an�lises diferentes: prever, para cada amostra, se a fermenta��o foi realizada com ou sem *jeotgal*, os diferentes dias de fermenta��o, e se est� no in�cio ou no final da fermenta��o.
# Come��mos por fazer um pequeno tratamento dos metadados do dataset, acrescentando uma coluna que informa se uma amostra tem ou n�o *jeotgal* (tomando os valores 'COM' e 'SEM').
c_s_jeotgal=c()
for (i in MTBLS654$metadata$Sample.Name){
  if (grepl( 'CK',i, fixed=TRUE) | grepl( 'NK',i, fixed=TRUE)){
    c_s_jeotgal = c(c_s_jeotgal, "SEM")
  }
  else {c_s_jeotgal = c(c_s_jeotgal, "COM")}
}

MTBLS654$metadata['Jeotgal'] = c_s_jeotgal


# Fun��o que calcula a precis�o para os modelos de classifica��o:
pecc = function(obs,pred)
  sum(obs==pred)/length(obs)

# Fun��es que calculam medidas de erro (RSME e MAD) para os modelos de regress�o:
rmse = function(previstos, reais) sqrt(mean((previstos-reais)^2))
mad = function(previstos, reais) mean(abs(previstos-reais))


# Para a primeira an�lise, foi criado um subset com amostras apenas de 12, 16 e 20 dias de fermenta��o, sendo este ainda separado em duas partes consoante as r�plicas (A ou B).

MTBLS654_12_16_20_ML = subset_samples_by_metadata_values(MTBLS654,"Fermentation_days",c(12,16,20))
MTBLS654_12_16_20_ML_A = subset_samples_by_metadata_values(MTBLS654_12_16_20_ML,"Factor.Value.Replicate.",c("A"))
MTBLS654_12_16_20_ML_B = subset_samples_by_metadata_values(MTBLS654_12_16_20_ML,"Factor.Value.Replicate.",c("B"))

# Com estes subsets foi criado um modelo de PLS, usando valida��o cruzada com repeti��es.
# O subset contendo apenas r�plicas A foi usado no treino do modelo, enquanto que o subset contendo r�plicas B foi usado para a previs�o de valores.
# Sempre que poss�vel, foi usada valida��o cruzada leave-one-out na cria��o de modelos, dado o n�mero de amostras ser pequeno no nosso dataset. No entanto, em alguns casos surgiu v�rios warnings quando se usava leave-one-out, pelo que sempre que isso aconteceu opt�mos por usar valida��o cruzada com repeti��es.

res_train_concentrations = train_models_performance(MTBLS654_12_16_20_ML_A, c("pls"), "Jeotgal", "repeatedcv")
result = predict_samples(res_train_concentrations$final.models$pls, MTBLS654_12_16_20_ML_B$data)

# Analisando a matriz de confus�o dos valores reais com os previstos, bem como o PECC, � poss�vel verificar que, no geral, o modelo previu bem o tipo de amostra relativamente � fermenta��o com e sem *jeotgal*, sendo que h� apenas um falso positivo em 'com jeotgal'.

table(result[,2], MTBLS654_12_16_20_ML_B$metadata$Jeotgal)
#      COM SEM
#COM   6   1
#SEM   0   5

pecc(MTBLS654_12_16_20_ML_B$metadata$Jeotgal,result[,2])
#0.9166667

# O mesmo procedimento foi aplicado desta vez para 25, 30 e 40 dias de fermenta��o, usando valida��o cruzada leave-one-out.

MTBLS654_25_30_40_ML = subset_samples_by_metadata_values(MTBLS654,"Fermentation_days",c(25,30,40))
MTBLS654_25_30_40_ML_A = subset_samples_by_metadata_values(MTBLS654_25_30_40_ML,"Factor.Value.Replicate.",c("A"))
MTBLS654_25_30_40_ML_B = subset_samples_by_metadata_values(MTBLS654_25_30_40_ML,"Factor.Value.Replicate.",c("B"))

res_train_concentrations = train_models_performance(MTBLS654_25_30_40_ML_A, "pls", "Jeotgal", "loocv", metric="Accuracy")
res_train_concentrations$performance

result = predict_samples(res_train_concentrations$final.models$pls, MTBLS654_25_30_40_ML_B$data)

# Da an�lise da matriz de confus�o e do PECC, verifica-se que o modelo previu todos os valores correctamente.

table(result[,2], MTBLS654_25_30_40_ML_B$metadata$Jeotgal)
#  COM SEM
#COM   6   0
#SEM   0   6

pecc(MTBLS654_25_30_40_ML_B$metadata$Jeotgal,result[,2])
#1


# Para a an�lise de previs�o dos dias de fermenta��o, o dataset com todos os dados foi dividido consoante o valor para as r�plicas.
# Mais uma vez, o subset com r�plicas A foi usado no treino do modelo e o subset com r�plicas B usado para previs�o.

MTBLS654_A = subset_samples_by_metadata_values(MTBLS654,"Factor.Value.Replicate.","A")
MTBLS654_B  = subset_samples_by_metadata_values(MTBLS654,"Factor.Value.Replicate.","B")

# O modelo usado foi Random Forests com valida��o cruzada de 5 folds e 20 repeti��es.

res = train_and_predict(MTBLS654_A, MTBLS654_B$data, num.folds = 5, num.repeats = 20,'Fermentation_days', "rf", "repeatedcv")

# Os valores de RMSE e MAD resultantes s�o relativamente baixos (??), pelo que se conclui que o modelo prev� bem os dias de fermenta��o.

#(PEDRO MOREIRA) os valores de RMSE e MAD s�o relativamente altos, mas � um dos melhores modelos de regress�o que temos, dada a falta de
#dados que temos.  

rmse(res$predictions.result[,2], MTBLS654_B$metadata$Fermentation_days)
#3.170678
mad (res$predictions.result[,2], MTBLS654_B$metadata$Fermentation_days)
#2.483101

# De seguida, procedeu-se � mesma an�lise, desta vez usando apenas os dados relativos � fermenta��o sem jeotgal.

MTBLS654_A_SEM = subset_samples_by_metadata_values(MTBLS654_A,"Jeotgal","SEM")
MTBLS654_B_SEM = subset_samples_by_metadata_values(MTBLS654_B,"Jeotgal","SEM")

# A valida��o cruzada foi repetida 50 vezes, dado estarmos a usar metade dos dados relativamente � an�lise anterior.

res = train_and_predict(MTBLS654_A_SEM, MTBLS654_B_SEM$data, num.folds = 5, num.repeats = 50,'Fermentation_days', "knn", "repeatedcv")

# Como se pode verificar pelas medidas de erro, o erro resultante � muito mais elevado, o que indica que o modelo n�o prev� t�o bem os dias de fermenta��o quando se usa apenas os dados 'sem jeotgal' para treino.

rmse(res$predictions.result[,2], MTBLS654_B$metadata$Fermentation_days)
#[1] 11.73286
mad (res$predictions.result[,2], MTBLS654_B$metadata$Fermentation_days)
#[1] 10.4


# Fazendo a mesma an�lise para os dados 'com jeotgal', verifica-se que a previs�o dos valores � melhor, com valores de RMSE e MAD que se assemelham mais aos do primeiro modelo.

MTBLS654_A_COM = subset_samples_by_metadata_values(MTBLS654_A,"Jeotgal","COM")
MTBLS654_B_COM = subset_samples_by_metadata_values(MTBLS654_B,"Jeotgal","COM")
res = train_and_predict(MTBLS654_A_COM, MTBLS654_B_COM$data, num.folds = 5, num.repeats = 50,'Fermentation_days', "rf", "loocv")

rmse(res$predictions.result[,2], MTBLS654_B_COM$metadata$Fermentation_days)
#[1] 3.8704
mad (res$predictions.result[,2], MTBLS654_B_COM$metadata$Fermentation_days)
#[1] 3.08

# Conclui-se que o primeiro modelo � o que aparenta ter melhor performance dos tr�s modelos criados, talvez por ser o modelo para o qual se usa todos as amostras para treino.


# Para a �ltima an�lise, criou-se datasets de treino e teste com os dados relativos apenas ao in�cio e ao final da fermenta��o (0 e 40 dias).

MTBLS654_0_40 = subset_samples_by_metadata_values(MTBLS654,"Fermentation_days",c(0,40))
MTBLS654_0_40_A = subset_samples_by_metadata_values(MTBLS654_0_40,"Factor.Value.Replicate.","A")
MTBLS654_0_40_B = subset_samples_by_metadata_values(MTBLS654_0_40,"Factor.Value.Replicate.","B")

# Usou-se um modelo de redes neuronais com valida��o cruzada de 5 folds e 20 repeti��es.

res=train_and_predict(MTBLS654_0_40_A, MTBLS654_0_40_B$data, num.folds = 5, num.repeats = 20,'Fermentation_days', "nnet", "repeatedcv")

# Como os valores de previs�o do modelo s�o probabildades, � necess�rio convert�-los para os valores reais (assumindo 0 se a probabilidade for menor do que 0.5 e 40 se a probabilidade for maior do que 0.5).
# Para isso, usou-se a seguinte fun��o:
results_convert= c()
for (i in 1:length(res$predictions.result[,2])){
  if (res$predictions.result[,2][i]<0.5){results_convert = c(results_convert,0)}
  else {results_convert = c(results_convert,40)}
}

# Analisando a matriz de confus�o e o PECC resultantes, verifica-se que modelo � capaz de prever todos os valores de in�cio ou final de fermenta��o correctamente.


table(results_convert, MTBLS654_0_40_B$metadata$Fermentation_days)
#results_convert 0 40
#0    4  0
#40   0  4

pecc(MTBLS654_0_40_B$metadata$Fermentation_days, results_convert)
#1


# (PEDRO MOREIRA) Tal como todos os modelos anteriores, � question�vel a qualidade destes modelos, pois como existe poucos dados para treino, significa que o treino 
# podem ter gerados modelos com complexidades insuficientes. 

#########################
#########################
### Feature selection

# Numa fase seguinte, procedeu-se � selec��o de atributos (metabolitos) para a cria��o de novos modelos de regress�o para a previs�o de dias de fermenta��o.
# A selecc��o de atributos foi feita com valida��o cruzada de 5 folds e 10 repeti��es.

vals=feature_selection(MTBLS654, "Fermentation_days", method = "rfe", functions=caret::rfFuncs, validation = "repeatedcv", repeats = 5, number = 10, subsets = 2^(2:4))
#glycine, lactate, dimethylamine, sucrose, fructose

# Usando o modelo Random Forests com valida��o cruzada de 5 folds e 50 repeti��es, os metabolitos seleccionados s�o glicina, lactato, dimetilamina, sacarose e frutose.
# Os datasets de treino e teste s�o criados usando apenas os valores para esses metabolitos.

MTBLS654_A_rf = subset_x_values(MTBLS654_A, c("glycine", "lactate", "dimethylamine", "sucrose", "fructose"))
MTBLS654_B_rf = subset_x_values(MTBLS654_B, c("glycine", "lactate", "dimethylamine", "sucrose", "fructose"))

res = train_and_predict(MTBLS654_A_rf, MTBLS654_B_rf$data, num.folds = 5, num.repeats = 50,'Fermentation_days', "rf", "repeatedcv")

res$predictions.result

# As medidas de erro resultantes deste modelo s�o ligeiramente mais baixas do que as do modelo criado anteriormente para a mesma previs�o, pelo que � poss�vel concluir que a selec��o de atributos melhora, embora pouco, a performance do modelo neste caso.

rmse(res$predictions.result[,2], MTBLS654_B_rf$metadata$Fermentation_days)
#3.088342
mad (res$predictions.result[,2], MTBLS654_B_rf$metadata$Fermentation_days)
#2.283631


# Repetiu-se o procedimento anterior, desta vez usando um modelo de regress�o linear.
# Os metabolitos seleccionados foram inosina, tirosina, fenilalanina, valina e trimetilamina.
vals=feature_selection(MTBLS654, "Fermentation_days", method = "rfe", functions=caret::lmFuncs, validation = "repeatedcv", repeats = 5, number = 10, subsets = 2^(2:4))
#inosine, tyrosine, phenylalanine, valine, trimethylamine

vals$optVariables

MTBLS654_A_lm = subset_x_values(MTBLS654_A, c("inosine", "tyrosine", "phenylalanine", "valine", "trimethylamine"))
MTBLS654_B_lm = subset_x_values(MTBLS654_B, c("inosine", "tyrosine", "phenylalanine", "valine", "trimethylamine"))

res = train_and_predict(MTBLS654_A_lm, MTBLS654_B_lm$data, num.folds = 5, num.repeats = 50,'Fermentation_days', "lm", "repeatedcv")

res$predictions.result

# Pela an�lise das medidas de erro, conclui-se que este modelo tem pior performance do que o Random Forests.

rmse(res$predictions.result[,2], MTBLS654_B_lm$metadata$Fermentation_days)
#7.398949
mad (res$predictions.result[,2], MTBLS654_B_lm$metadata$Fermentation_days)
#5.739444


# Para o modelo Bagged Trees, os metabolitos seleccionados foram glicina, frutose, lactato e manitol.
# � excep��o do manitol, todos os mesmos metabolitos seleccionados s�o os mesmos para o Random Forests.

vals=feature_selection(MTBLS654, "Fermentation_days", method = "rfe", functions=caret::treebagFuncs, validation = "repeatedcv", repeats = 5, number = 10, subsets = 2^(2:4))
#glycine, fructose, sucrose, lactate, mannitol

MTBLS654_A_bg = subset_x_values(MTBLS654_A, c("glycine", "fructose", "sucrose", "lactate", "mannitol"))
MTBLS654_B_bg = subset_x_values(MTBLS654_B, c("glycine", "fructose", "sucrose", "lactate", "mannitol"))

res = train_and_predict(MTBLS654_A_bg, MTBLS654_B_bg$data, num.folds = 5, num.repeats = 50,'Fermentation_days', "treebag", "repeatedcv")

res$predictions.result

rmse(res$predictions.result[,2], MTBLS654_A_bg$metadata$Fermentation_days)
# [1] 4.381092
mad (res$predictions.result[,2], MTBLS654_A_bg$metadata$Fermentation_days)
# [1] 3.268282

# A performance deste modelo � ligeiramente pior que a do modelo Random Forests mas melhor do que a de regress�o linear.
# A selec��o de atributos poder� ter influ�ncia nos erros produzidos por um determinado modelo (??).


####### S� DEPOIS DE FAZER ESTA DISCUSS�O � Q REPAREI QUE NO C�LCULO DE RMSE E MAD EST�-SE A USAR
# MTBLS654_B_lm$metadata$Fermentation_days
# EM VEZ DE
# MTBLS654_B_bg$metadata$Fermentation_days
# MAS USANDO ESTE �LTIMO D� O MESMO ERRO QUE D� NOS DOIS MODELOS A SEGUIR (LDA e Naive-Bayes)
rmse(res$predictions.result[,2], MTBLS654_B_bg$metadata$Fermentation_days)
mad(res$predictions.result[,2], MTBLS654_B_bg$metadata$Fermentation_days)
# [1] NA
#Warning message:
#  In Ops.factor(previstos, reais) : '-' not meaningful for factors


#LDA
#vals=feature_selection(MTBLS654, "Fermentation_days", method = "rfe",  functions=caret::ldaFuncs, validation = "repeatedcv", repeats = 5, number = 10, subsets = 2^(2:4))
#so fiz feature pois modelo dava erro

#Naive-Bayes
#vals=feature_selection(MTBLS654, "Fermentation_days", method = "rfe",  functions=caret::nbFuncs, validation = "repeatedcv", repeats = 5, number = 10, subsets = 2^(2:4))
#so fiz feature pois modelo dava erro



##############

#regressao linear

linregression_onevar(MTBLS654, "acetate", c("Fermentation_days","Jeotgal"), "Fermentation_days+Jeotgal")
#este e um exemplo de metabolito; os c�digos seguintes faz para todos


reg_days_Jeot=linreg_all_vars(MTBLS654, c("Fermentation_days","Jeotgal"), "Fermentation_days+Jeotgal")

linreg_pvalue_table(reg_days_Jeot)

linreg_rsquared(reg_days_Jeot)

plot_regression_coefs_pvalues(reg_days_Jeot[1:6], bar.col=c("blue", "cyan", "red", "green", "yellow", "black"))
