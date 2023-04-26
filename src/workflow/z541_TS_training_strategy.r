# Workflow  Training Strategy

#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
#   8 vCPU


#limpio la memoria
rm( list= ls(all.names= TRUE) )  #remove all objects
gc( full= TRUE )                 #garbage collection

require("data.table")
require("yaml")


#Parametros del script
PARAM  <- list()
PARAM$experimento <- "TS5410"

PARAM$exp_input  <- "FE5310"

PARAM$future       <- c( 202109 )
PARAM$final_train  <- c( 202107, 202106, 202105, 202104, 202103, 202102, 202101, 202012, 202011 )

PARAM$train$training     <- c( 202105, 202104, 202103, 202102, 202101, 202012, 202011, 202010, 202009 )
PARAM$train$validation   <- c( 202106 )
PARAM$train$testing      <- c( 202107 )

PARAM$train$undersampling  <- 1.0   # 1.0 significa NO undersampling ,  0.1  es quedarse con el 10% de los CONTINUA

PARAM$train$semilla  <- 102191
# FIN Parametros del script


# si training se establece identico a validation, entonces aguas abajo se hara Cross-Validation
# si training = validation = testing   tambien se hara  Cross-Validation
#------------------------------------------------------------------------------

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
PARAM$stat$time_start  <- format(Sys.time(), "%Y%m%d %H%M%S")

setwd( "~/buckets/b1/" )

#cargo el dataset donde voy a entrenar
#esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
dataset_input  <- paste0( "./exp/", PARAM$exp_input, "/dataset.csv.gz" )
dataset  <- fread( dataset_input )


#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

write_yaml( PARAM, file= "parametros.yml" )   #escribo parametros utilizados

setorder( dataset, foto_mes, numero_de_cliente )

#grabo los datos del futuro
fwrite( dataset[ foto_mes %in% PARAM$future, ],
        file= "dataset_future.csv.gz",
        logical01= TRUE,
        sep= "," )

#grabo los datos donde voy a entrenar los Final Models
fwrite( dataset[ foto_mes %in% PARAM$final_train, ],
        file= "dataset_train_final.csv.gz",
        logical01= TRUE,
        sep= "," )



#grabo los datos donde voy a hacer la optimizacion de hiperparametros
set.seed( PARAM$train$semilla )
dataset[ foto_mes %in% PARAM$train$training , azar := runif( nrow(dataset[foto_mes %in% PARAM$train$training ]) ) ]

dataset[  , fold_train := 0L ]
dataset[ foto_mes %in% PARAM$train$training & 
         ( azar <= PARAM$train$undersampling  | clase_ternaria %in% c( "BAJA+1", "BAJA+2" ) )
         , fold_train := 1L ]

dataset[  , fold_validate := 0L ]
dataset[ foto_mes %in% PARAM$train$validation, fold_validate := 1L ]

dataset[  , fold_test := 0L ]
dataset[ foto_mes %in% PARAM$train$testing, fold_test := 1L ]


fwrite( dataset[ fold_train + fold_validate + fold_test >= 1 , ],
        file= "dataset_training.csv.gz",
        logical01= TRUE,
        sep= "," )

#------------------------------------------------------------------------------
PARAM$stat$time_end  <- format(Sys.time(), "%Y%m%d %H%M%S")
write_yaml( PARAM, file= "parametros.yml" )   #escribo parametros utilizados

#dejo la marca final
cat( format(Sys.time(), "%Y%m%d %H%M%S"),"\n",
     file= "zRend.txt",
     append= TRUE  )
