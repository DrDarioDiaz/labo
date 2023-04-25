# Rager Balanceado
# En honor a la estadistica clasica
# El balancedo segun la proporcion de la clase, y no del negocio

#Este script esta pensado para correr en la PC local

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("ranger")
require("randomForest")  #solo se usa para imputar nulos

#Aqui se debe poner la carpeta de la computadora local
setwd("X:\\gdrive\\austral2023v\\")  #Establezco el Working Directory

#cargo los datos donde entreno
dataset  <- fread("./datasets/dataset_pequeno.csv")


dtrain  <- dataset[ foto_mes == 202107 ]
dapply  <- dataset[ foto_mes == 202109 ]


#genero el modelo de Random Forest con la libreria ranger
#notar como la suma de muchos arboles contrarresta el efecto de min.node.size=1
param  <- list( "num.trees"=       300,  #cantidad de arboles
                "mtry"=             30,  #cantidad de variables que evalua para hacer un split  sqrt(ncol(dtrain))
                "min.node.size"=  1500,  #tamaño minimo de las hojas
                "max.depth"=        12   # 0 significa profundidad infinita
              )

set.seed(102191) #Establezco la semilla aleatoria

factorizado  <- as.factor( dtrain$clase_ternaria )
dtrain[ , clase_ternaria := factorizado ]

#imputo los nulos, ya que ranger no acepta nulos
#Leo Breiman, ¿por que le temias a los nulos?
dtrain  <- na.roughfix( dtrain )

setorder( dtrain, clase_ternaria )  #primero quedan los BAJA+1, BAJA+2, CONTINUA

# caculo los pesos de las clases
# de forma que queden totalmente balanceadas
peso <- list()
peso$baja1  <- dtrain[ , .N ] / dtrain[ clase_ternaria=="BAJA+1", .N ]
peso$baja2  <- dtrain[ , .N ] / dtrain[ clase_ternaria=="BAJA+2", .N ]
peso$continua  <- dtrain[ , .N ] / dtrain[ clase_ternaria=="CONTINUA", .N ]

#genero el modelo de Random Forest llamando a ranger()
modelo  <- ranger( formula= "clase_ternaria ~ .",
                   data=  dtrain, 
                   probability=   TRUE,  #para que devuelva las probabilidades
                   num.trees=     param$num.trees,
                   mtry=          param$mtry,
                   min.node.size= param$min.node.size,
                   max.depth=     param$max.depth,
                   class.weights= c( peso$baja1, peso$baja2, peso$continua )  # BAJA+1, BAJA+2, CONTINUA
                 )


#Carpinteria necesaria sobre  dapply
# como quiere la Estadistica Clasica, imputar nulos por separado
# ( aunque en este caso ya tengo los datos del futuro de anteman
#  pero bueno, sigamos el librito de estos fundamentalistas a rajatabla ...
dapply[ , clase_ternaria := NULL ]
dapply  <- na.roughfix( dapply )


#aplico el modelo recien creado a los datos del futuro
prediccion  <- predict( modelo, dapply )

#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.numeric(prediccion$predictions[ ,"BAJA+2" ] > 1/40) ) ) #genero la salida

#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/KA3370/", showWarnings = FALSE )
archivo_salida  <- "./exp/KA3370/KA3370_001.csv"

#genero el archivo para Kaggle
fwrite( entrega, 
        file= archivo_salida, 
        sep="," )
