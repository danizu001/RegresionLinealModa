install.packages("dplyr")
library(dplyr)
install.packages("fastDummies")
library(fastDummies)

train_data<-read.csv("C:\\Users\\danyg\\OneDrive\\Documents\\Javeriana Maestria\\Primer Semestre\\Metodo Analítica 1\\Taller 3\\train.csv", header=TRUE, sep=",", dec=".")
test_data<-read.csv("C:\\Users\\danyg\\OneDrive\\Documents\\Javeriana Maestria\\Primer Semestre\\Metodo Analítica 1\\Taller 3\\test.csv", header=TRUE, sep=",", dec=".")

train_data$tamamer <- factor(train_data$tamamer, levels = c("Pequeño", "Median", "Grande"))
train_data$tamamer <- as.numeric(train_data$tamamer) - 1
test_data$tamamer <- factor(test_data$tamamer, levels = c("Pequeño", "Median", "Grande"))
test_data$tamamer <- as.numeric(test_data$tamamer) - 1

idloc<-train_data$idloc
input_data <- within(train_data,rm(idloc))
test_data <- within(test_data,rm(ropamujer))

input_data <- dummy_cols(input_data, select_columns = c("idmercado"), remove_selected_columns = TRUE)
test_data <- dummy_cols(test_data, select_columns = c("idmercado"), remove_selected_columns = TRUE)

idloc_g <- test_data$idloc[test_data$promo == "1"]
idloc_m <- test_data$idloc[test_data$promo == "2"]
idloc_p <- test_data$idloc[test_data$promo == "3"]

idloc_test<-test_data$idloc
predict_data <- within(test_data,rm(idloc))


input_data_no_at<-input_data[-c(263, 549,674,974, 1425, 1024,845,1347), ]
input_data_no_at$serv_per_tel <- input_data_no_at$telefono/input_data_no_at$servicio
predict_data$serv_per_tel <- predict_data$telefono/predict_data$servicio
input_data_no_at$asinserv_per_tel <- log(input_data_no_at$telefono/input_data_no_at$servicio)
predict_data$asinserv_per_tel <- log(predict_data$telefono/predict_data$servicio)
input_data_no_at$asinedad <- log(input_data_no_at$edadloc)
predict_data$asinedad <- log(predict_data$edadloc)
input_data_no_at$asinimpresa <- log(input_data_no_at$impresa)
predict_data$asinimpresa <- log(predict_data$impresa)
input_data_no_at$asincorreo <- log(input_data_no_at$correo)
predict_data$asincorreo <- log(predict_data$correo)
input_data_no_at$asinnomina <- log(input_data_no_at$nomina)
predict_data$asinnomina <- log(predict_data$nomina)

input_data_no_at$inserv_per_tel <- 1/(input_data_no_at$telefono/input_data_no_at$servicio)
predict_data$inserv_per_tel <- 1/(predict_data$telefono/predict_data$servicio)
input_data_no_at$inedad <- 1/(input_data_no_at$edadloc)
predict_data$inedad <- 1/(predict_data$edadloc)
input_data_no_at$inimpresa <- 1/(input_data_no_at$impresa)
predict_data$inimpresa <- 1/(predict_data$impresa)
input_data_no_at$incorreo <- 1/(input_data_no_at$correo)
predict_data$incorreo <- 1/(predict_data$correo)
input_data_no_at$innomina <- 1/(input_data_no_at$nomina)
predict_data$innomina <- 1/(predict_data$nomina)


input_data_no_at$sqrserv_per_tel <- sqrt(input_data_no_at$telefono/input_data_no_at$servicio)
predict_data$sqrserv_per_tel <- sqrt(predict_data$telefono/predict_data$servicio)
input_data_no_at$sqredad <- sqrt(input_data_no_at$edadloc)
predict_data$sqredad <- sqrt(predict_data$edadloc)
input_data_no_at$sqrimpresa <- sqrt(input_data_no_at$impresa)
predict_data$sqrimpresa <- sqrt(predict_data$impresa)
input_data_no_at$sqrcorreo <- sqrt(input_data_no_at$correo)
predict_data$sqrcorreo <- sqrt(predict_data$correo)
input_data_no_at$sqrnomina <- sqrt(input_data_no_at$nomina)
predict_data$sqrnomina <- sqrt(predict_data$nomina)

lista_de_trains <- split(input_data_no_at, input_data_no_at$promo)
lista_de_tests <- split(predict_data, predict_data$promo)


for (i in seq_along(lista_de_trains)) {
  # Eliminar la columna 'categoria' de cada DataFrame
  lista_de_trains[[i]] <- subset(lista_de_trains[[i]], select = -promo)
}

for (i in seq_along(lista_de_tests)) {
  # Eliminar la columna 'categoria' de cada DataFrame
  lista_de_tests[[i]] <- subset(lista_de_tests[[i]], select = -promo)
}


modelo_grande <- lm(ropamujer ~.,data=lista_de_trains[[1]])
modelo_mediano <- lm(ropamujer ~.,data=lista_de_trains[[2]])
modelo_peque <- lm(ropamujer ~.,data=lista_de_trains[[3]])


modelostep_g<- step(modelo_grande,direction="both")
modelostep_m<- step(modelo_mediano,direction="both")
modelostep_p<- step(modelo_peque,direction="both")


#####################

set.seed(49584) 
sample_g <- sample.int(nrow(lista_de_trains[[1]]), floor(.75*nrow(lista_de_trains[[1]])))
input_train_g <- lista_de_trains[[1]][sample_g, ]
input_test_g <- lista_de_trains[[1]][-sample_g, ]

predic.train_g<-cbind(as.matrix(input_train_g[,1:8]),as.matrix(input_train_g[,10:34]))
precio.train_g<-as.matrix(input_train_g[,9])

predic.test_g<-cbind(as.matrix(input_test_g[,1:8]),as.matrix(input_test_g[,10:34]))
precio.test_g<-as.matrix(input_test_g[,9])

sample_m <- sample.int(nrow(lista_de_trains[[2]]), floor(.72*nrow(lista_de_trains[[2]])))
input_train_m <- lista_de_trains[[2]][sample_m, ]
input_test_m <- lista_de_trains[[2]][-sample_m, ]

predic.train_m<-cbind(as.matrix(input_train_m[,1:8]),as.matrix(input_train_m[,10:34]))
precio.train_m<-as.matrix(input_train_m[,9])

predic.test_m<-cbind(as.matrix(input_test_m[,1:8]),as.matrix(input_test_m[,10:34]))
precio.test_m<-as.matrix(input_test_m[,9])

sample_p <- sample.int(nrow(lista_de_trains[[3]]), floor(.8*nrow(lista_de_trains[[3]])))
input_train_p <- lista_de_trains[[3]][sample_p, ]
input_test_p <- lista_de_trains[[3]][-sample_p, ]

predic.train_p<-cbind(as.matrix(input_train_p[,1:8]),as.matrix(input_train_p[,10:34]))
precio.train_p<-as.matrix(input_train_p[,9])

predic.test_p<-cbind(as.matrix(input_test_p[,1:8]),as.matrix(input_test_p[,10:34]))
precio.test_p<-as.matrix(input_test_p[,9])

library(glmnet)
fitlasso_g<-glmnet(predic.train_g,precio.train_g,alpha = 1)
fitridge_g<-glmnet(predic.train_g,precio.train_g,alpha = 0)

foundridge_g<-cv.glmnet(predic.train_g, precio.train_g,alpha=0,nfolds=5)
foundlasso_g<-cv.glmnet(predic.train_g, precio.train_g,alpha=1,nfolds=5)

fitlasso_m<-glmnet(predic.train_m,precio.train_m,alpha = 1)
fitridge_m<-glmnet(predic.train_m,precio.train_m,alpha = 0)

foundridge_m<-cv.glmnet(predic.train_m, precio.train_m,alpha=0,nfolds=5)
foundlasso_m<-cv.glmnet(predic.train_m, precio.train_m,alpha=1,nfolds=5)

fitlasso_p<-glmnet(predic.train_p,precio.train_p,alpha = 1)
fitridge_p<-glmnet(predic.train_p,precio.train_p,alpha = 0)

foundridge_p<-cv.glmnet(predic.train_p, precio.train_p,alpha=0,nfolds=5)
foundlasso_p<-cv.glmnet(predic.train_p, precio.train_p,alpha=1,nfolds=5)

for (i in 1:10){ 
  assign(paste("found_g", i, sep=""), cv.glmnet(predic.train_g, precio.train_g, nfolds=4, 
                                              alpha=i/10,))
}
#obtengo los valores del minimo mse
min(foundridge_g$cvm)
min(found_g1$cvm)
min(found_g2$cvm)
min(found_g3$cvm)
min(found_g4$cvm)
min(found_g5$cvm)
min(found_g6$cvm)
min(found_g7$cvm)
min(found_g8$cvm)
min(found_g9$cvm)
min(foundlasso_g$cvm)

for (i in 1:10){ 
  assign(paste("found_m", i, sep=""), cv.glmnet(predic.train_m, precio.train_m, nfolds=4, 
                                                alpha=i/10,))
}
#obtengo los valores del minimo mse
min(foundridge_m$cvm)
min(found_m1$cvm)
min(found_m2$cvm)
min(found_m3$cvm)
min(found_m4$cvm)
min(found_m5$cvm)
min(found_m6$cvm)
min(found_m7$cvm)
min(found_m8$cvm)
min(found_m9$cvm)
min(foundlasso_m$cvm)

for (i in 1:10){ 
  assign(paste("found_p", i, sep=""), cv.glmnet(predic.train_p, precio.train_p, nfolds=4, 
                                                alpha=i/10,))
}
#obtengo los valores del minimo mse
min(foundridge_p$cvm)
min(found_p1$cvm)
min(found_p2$cvm)
min(found_p3$cvm)
min(found_p4$cvm)
min(found_p5$cvm)
min(found_p6$cvm)
min(found_p7$cvm)
min(found_p8$cvm)
min(found_p9$cvm)
min(foundlasso_p$cvm)

elastic5_g<-glmnet(predic.train_g,precio.train_g,alpha = 0.5)
elastic5_m<-glmnet(predic.train_m,precio.train_m,alpha = 0.4)
elastic5_p<-glmnet(predic.train_p,precio.train_p,alpha = 0.8)

predicciones1_g<-predict.glmnet(elastic5_g, predic.test_g, s=elastic5_g$lambda.min)

#ridge
predicciones2_g<-predict.glmnet(fitridge_g, predic.test_g, s=foundridge_g$lambda.min)

#lasso
predicciones3_g<-predict.glmnet(fitlasso_g, predic.test_g, s=foundlasso_g$lambda.min)

predicciones1_m<-predict.glmnet(elastic5_m, predic.test_m, s=elastic5_m$lambda.min)

#ridge
predicciones2_m<-predict.glmnet(fitridge_m, predic.test_m, s=foundridge_m$lambda.min)

#lasso
predicciones3_m<-predict.glmnet(fitlasso_m, predic.test_m, s=foundlasso_m$lambda.min)

predicciones1_p<-predict.glmnet(elastic5_p, predic.test_p, s=elastic5_p$lambda.min)

#ridge
predicciones2_p<-predict.glmnet(fitridge_p, predic.test_p, s=foundridge_p$lambda.min)

#lasso
predicciones3_p<-predict.glmnet(fitlasso_p, predic.test_p, s=foundlasso_p$lambda.min)

erroreselastic_g=sqrt(mean((predicciones1_g-precio.test_g[,1])^2))
erroresridge_g=sqrt(mean((predicciones2_g-precio.test_g[,1])^2))
erroreslasso_g=sqrt(mean((predicciones3_g-precio.test_g[,1])^2))

erroreselastic_m=sqrt(mean((predicciones1_m-precio.test_m[,1])^2))
erroresridge_m=sqrt(mean((predicciones2_m-precio.test_m[,1])^2))
erroreslasso_m=sqrt(mean((predicciones3_m-precio.test_m[,1])^2))

erroreselastic_p=sqrt(mean((predicciones1_p-precio.test_p[,1])^2))
erroresridge_p=sqrt(mean((predicciones2_p-precio.test_p[,1])^2))
erroreslasso_p=sqrt(mean((predicciones3_p-precio.test_p[,1])^2))

predicciones_real_p<-predict.glmnet(fitlasso_p, as.matrix(lista_de_tests[[3]]), s=foundlasso_p$lambda.min)
resultados_prediccion2_p <- data.frame(idloc = idloc_p, ropamujer = array(predicciones_real_p))

#######################
predmod_step_g <- predict(modelostep_g, lista_de_tests[[1]], se.fit=TRUE)
predmod_step_m <- predict(modelostep_m, lista_de_tests[[2]], se.fit=TRUE)
predmod_step_p <- predict(modelostep_p, lista_de_tests[[3]], se.fit=TRUE)


rmse_calc_g <- predict(modelostep_g, lista_de_trains[[1]], se.fit=TRUE)
rmse_calc_m <- predict(modelostep_m, lista_de_trains[[2]], se.fit=TRUE)
rmse_calc_p <- predict(modelostep_p, lista_de_trains[[3]], se.fit=TRUE)


RMSEmod_step_g<-sqrt(mean((rmse_calc_g$fit-lista_de_trains[[1]]$ropamujer)^2))
RMSEmod_step_m<-sqrt(mean((rmse_calc_m$fit-lista_de_trains[[2]]$ropamujer)^2))
RMSEmod_step_p<-sqrt(mean((rmse_calc_p$fit-lista_de_trains[[3]]$ropamujer)^2))



resultados_prediccion_g <- data.frame(idloc = idloc_g, ropamujer = predmod_step_g$fit)
resultados_prediccion_m <- data.frame(idloc = idloc_m, ropamujer = predmod_step_m$fit)
resultados_prediccion_p <- data.frame(idloc = idloc_p, ropamujer = predmod_step_p$fit)

predict_total <- rbind(resultados_prediccion_g, resultados_prediccion_m, resultados_prediccion2_p)

indices_ordenados <- match(predict_total$idloc, idloc_test)

# Ordenar el DataFrame utilizando los índices obtenidos
resultado_ordenado <- predict_total[order(indices_ordenados), ]

write.csv(resultado_ordenado, "C:\\Users\\danyg\\OneDrive\\Documents\\Javeriana Maestria\\Primer Semestre\\Metodo Analítica 1\\Taller 3\\modelo_step_div.csv", row.names = FALSE)




