# Experimentos Colaborativos Default
# ZZ con semillerio


#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
#   8 vCPU


#limpio la memoria
rm( list= ls(all.names= TRUE) )  #remove all objects
gc( full= TRUE )                 #garbage collection

require("data.table")
require("primes")
require("yaml")

require("lightgbm")


#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "ZZ7990"
PARAM$exp_input  <- "HT7510"

#Atencion, que cada modelos se procesa con 5 semillas, ajuste a SUS necesidades
PARAM$modelos_rank  <- c( 1 )  #Que modelos quiero, segun su posicion en el ranking e la Bayesian Optimizacion, ordenado por ganancia descendente
PARAM$semillerio  <- 20   # cantidad de semillas a utilizar en el semillerio, podrian ser 50 o 100 para mayor estabilidad
PARAM$semilla_primos  <- 102191  # se utiliza para generar el vector de  PARAM$semillerio  semillas

PARAM$kaggle$envios_desde  <-  8000L
PARAM$kaggle$envios_hasta  <- 13500L
PARAM$kaggle$envios_salto  <-   500L

PARAM$graficar$envios_hasta  <- 20000L  #para el caso que deba graficar
PARAM$graficar$ventana_suavizado  <- 2001L

PARAM$home  <- "~/buckets/b1/"
# FIN Parametros del script

OUTPUT  <- list()

#------------------------------------------------------------------------------
options(error= function() {
  traceback(20);
  options(error= NULL);
  stop("exiting after script error")
})
#------------------------------------------------------------------------------

GrabarOutput  <- function()
{
  write_yaml( OUTPUT, file= "output.yml" )   # grabo OUTPUT
}
#------------------------------------------------------------------------------
# impresion de los graficos cuando conozco la clase, cuando future tiene clase

ImprimirGraficos  <- function( tb_ganancias, modelo_rank, iteracion_bayesiana, qsemillas )
{
  # calculo la mayor ganancia  SUAVIZADA
  tb_ganancias[ , gan_suavizada := frollmean( x= ganancia_acum,
                                              n= PARAM$graficar$ventana_suavizado,
                                              align= "center",
                                              na.rm= TRUE,
                                              hasNA= TRUE )]

  ganancia_suavizada_max  <- tb_ganancias[ , max( gan_suavizada, na.rm= TRUE ) ]

  ymax  <- max( tb_ganancias, na.rm= TRUE ) * 1.05

  arch_grafico  <- paste0( "modelo_",
                           sprintf( "%02d", modelo_rank ),
                           "_",
                           sprintf( "%03d", iteracion_bayesiana ),
                           ".pdf" )

  pdf( arch_grafico )

 # primera curva
  plot( x= tb_ganancias[ , envios],
        y= tb_ganancias[ , g1],
        type= "l",
        col= "gray",
        xlim= c( 0, PARAM$graficar$envios_hasta ),
        ylim= c( 0, ymax ),
        main= paste0("sem: ", qsemillas, " Mejor gan prom= ", as.integer(ganancia_suavizada_max) ),
        xlab= "Envios",
        ylab= "Ganancia",
        panel.first= grid()
     )

  #las siguientes curvas
  if( qsemillas > 1 )
  {
    for( s in 2:qsemillas )
    {
      lines( x= tb_ganancias[ , envios],
             y= tb_ganancias[ , get(paste0("g", s)) ],
             col= "gray" )
    }
  }

  # finalmente la curva promedio
  lines( x= tb_ganancias[ , envios ],
         y= tb_ganancias[ , ganancia_acum ],
         col= "red" )

  dev.off()


  #grabo las ganancias, para poderlas comparar con OTROS modelos
  arch_ganancias  <- paste0( "ganancias_",
                             sprintf( "%02d", modelo_rank ),
                             "_",
                             sprintf( "%03d", iteracion_bayesiana ),
                             ".txt" )

  fwrite( tb_ganancias,
          file= arch_ganancias,
          sep= "\t",
        )
}
#------------------------------------------------------------------------------
# generacion de archivos para Kaggle, cuando future no tiene clase

GenerarKaggle  <- function( tb_prediccion, modelo_rank, iteracion_bayesiana )
{
  # genero el vector de cortes
  cortes  <- seq( from=  PARAM$kaggle$envios_desde,
                  to=    PARAM$kaggle$envios_hasta,
                  by=    PARAM$kaggle$envios_salto )


  # genero los archivos por probabilidad
  for( campo  in  c( "prob" ) )
  {
    setorderv( tb_prediccion, c(campo), c(-1) )
    # grabo los archivos para cada corte
    for( corte in cortes )
    {
      tb_prediccion[ , Predicted := 0L ]
      tb_prediccion[ 1:corte, Predicted := 1L ]

      nom_submit  <- paste0( PARAM$experimento,
                             "_",
                             sprintf( "%02d", modelo_rank ),
                             "_",
                             sprintf( "%03d", iteracion_bayesiana ),
                             "_",
                             sprintf( "%05d", corte ),
                             substr( campo, 1, 1),
                             ".csv" )

      fwrite( tb_prediccion[ , list( numero_de_cliente, Predicted ) ],
              file= nom_submit,
              sep= "," )
    }
  }

}
#------------------------------------------------------------------------------
# grabo la importancia de variables

grabar_importancia  <- function( modelo_final, modelo_rank, iteracion_bayesiana )
{
  tb_importancia  <- as.data.table( lgb.importance( modelo_final ) )
  fwrite( tb_importancia,
          file= paste0( "impo_",
                         sprintf( "%02d", modelo_rank ),
                         "_",
                         sprintf( "%03d", iteracion_bayesiana ),
                         ".txt" ),
          sep= "\t" )

  rm( tb_importancia )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
OUTPUT$PARAM  <- PARAM
OUTPUT$time$start  <- format(Sys.time(), "%Y%m%d %H%M%S")

#genero un vector de una cantidad de PARAM$semillerio  de semillas,  buscando numeros primos al azar
primos  <- generate_primes(min=100000, max=1000000)  #genero TODOS los numeros primos entre 100k y 1M
set.seed( PARAM$semilla_primos ) #seteo la semilla que controla al sample de los primos
ksemillas  <- sample(primos)[ 1:PARAM$semillerio ]   #me quedo con PARAM$semillerio  primos al azar


#creo la carpeta donde va el experimento
dir.create( paste0( PARAM$home, "exp/", PARAM$experimento, "/"), showWarnings= FALSE )
setwd(paste0( PARAM$home, "exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

GrabarOutput()
write_yaml( PARAM, file= "parametros.yml" )   #escribo parametros utilizados

#leo la salida de la optimizaciob bayesiana
arch_log  <- paste0( PARAM$home, "exp/", PARAM$exp_input, "/BO_log.txt" )
tb_log  <- fread( arch_log )
setorder( tb_log, -ganancia )

#leo el nombre del expermento de la Training Strategy
arch_TS  <- paste0( PARAM$home, "exp/", PARAM$exp_input, "/TrainingStrategy.txt" )
TS  <- readLines( arch_TS, warn= FALSE )

#leo el dataset donde voy a entrenar el modelo final
arch_dataset  <- paste0( PARAM$home, "exp/", TS, "/dataset_train_final.csv.gz" )
dataset  <- fread( arch_dataset )

#leo el dataset donde voy a aplicar el modelo final
arch_future  <- paste0( PARAM$home, "exp/", TS, "/dataset_future.csv.gz" )
dfuture  <- fread( arch_future )

# logical que me indica si los dtos de future tienen la clase con valores, y NO va para Kaggle
future_con_clase  <- dfuture[ clase_ternaria== "" | is.na(clase_ternaria), .N ] == 0

#defino la clase binaria
dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 ) ]

campos_buenos  <- setdiff( colnames(dataset), c( "clase_ternaria", "clase01") )


#genero un modelo para cada uno de las modelos_qty MEJORES iteraciones de la Bayesian Optimization
vganancias_suavizadas  <- c()

imodelo  <- 0L
for( modelo_rank in  PARAM$modelos_rank )
{
  imodelo  <- imodelo + 1L
  cat( "\nmodelo_rank: ", modelo_rank, ", semillas: " )
  OUTPUT$status$modelo_rank  <- modelo_rank

  parametros  <- as.list( copy( tb_log[ modelo_rank ] ) )
  iteracion_bayesiana  <- parametros$iteracion_bayesiana


  #creo CADA VEZ el dataset de lightgbm
  dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ , campos_buenos, with= FALSE] ),
                          label=   dataset[ , clase01],
                          weight=  dataset[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                          free_raw_data= FALSE
                        )

  ganancia  <- parametros$ganancia

  #elimino los parametros que no son de lightgbm
  parametros$experimento  <- NULL
  parametros$cols         <- NULL
  parametros$rows         <- NULL
  parametros$fecha        <- NULL
  parametros$estimulos    <- NULL
  parametros$ganancia     <- NULL
  parametros$iteracion_bayesiana  <- NULL

  #  parametros$num_iterations  <- 10  # esta linea es solo para pruebas en desarrollo

  if( future_con_clase )
  {
     tb_ganancias  <- as.data.table( list( "envios"= 1:1:PARAM$graficar$envios_hasta ) )
     tb_ganancias[ , gan_sum := 0.0 ]
  }

  # inicializo  tb_prediccion
  tb_prediccion  <- dfuture[ , list( numero_de_cliente, foto_mes, clase_ternaria ) ]
  tb_prediccion[ , pos := .I ]
  tb_prediccion[ , semillas := 0L ]
  tb_prediccion[ , sum_prob_acumulada := 0 ]  # aqui voy a acumular la probabildad del semillerio
  tb_prediccion[ , sum_rank_acumulado := 0 ]  # aqui voy a acumular la probabildad del semillerio

  sem  <- 0L

  for( vsemilla  in ksemillas ) #recorro las semillas del Semillerio
  {
    sem  <- sem + 1L
    cat( sem, " " )
    OUTPUT$status$sem  <- sem
    GrabarOutput()

    setorder( tb_prediccion, pos )  #ordeno por el original

    #Utilizo la semilla definida en este script
    parametros$seed  <- vsemilla

    nombre_raiz  <- paste0( sprintf( "%02d", modelo_rank ),
                            "_",
                            sprintf( "%03d", iteracion_bayesiana ) )

    arch_modelo  <- paste0( "modelo_" ,
                            nombre_raiz,
                            ".model" )

    #genero el modelo entrenando en los datos finales
    set.seed( parametros$seed, kind= "L'Ecuyer-CMRG")
    modelo_final  <- lightgbm( data= dtrain,
                               param=  parametros,
                               verbose= -100 )

    #grabo el modelo, achivo .model
    lgb.save( modelo_final,
              file= arch_modelo )

    # creo y grabo la importancia de variables, solo para la primer semilla
    if( sem == 1 ) grabar_importancia( modelo_final, modelo_rank, iteracion_bayesiana )

    #genero la prediccion, Scoring
    prediccion  <- predict( modelo_final,
                            data.matrix( dfuture[ , campos_buenos, with= FALSE ] ) )

    tb_prediccion[ , prob_semilla := prediccion ]
    tb_prediccion[ , rank_semilla := frank( prediccion, ties.method= "random" ) ]
    tb_prediccion[ , sum_prob_acumulada := sum_prob_acumulada + prediccion ]
    tb_prediccion[ , sum_rank_acumulado := sum_rank_acumulado + frank( prediccion, ties.method= "random" ) ]

    tb_prediccion[ , semillas := sem ]
    tb_prediccion[ , prob := sum_prob_acumulada/semillas ]
    tb_prediccion[ , ranking := sum_rank_acumulado/semillas ]

    fwrite( tb_prediccion[ , list(numero_de_cliente, foto_mes, semillas, prob, ranking, clase_ternaria) ],
            file= paste0( "pred_", nombre_raiz, ".csv"  ),
            sep= "\t" )

    if( ! future_con_clase )  GenerarKaggle( tb_prediccion, modelo_rank, iteracion_bayesiana )


    if( future_con_clase )
    {
      setorder( tb_prediccion, -prob_semilla )
      tb_ganancias[ , paste0( "g", sem )  := tb_prediccion[ 1:PARAM$graficar$envios_hasta, cumsum( ifelse(clase_ternaria== "BAJA+2", 117000, -3000)) ] ]

      setorder( tb_prediccion, -prob )
      tb_ganancias[ , ganancia_acum  := tb_prediccion[ 1:PARAM$graficar$envios_hasta, cumsum( ifelse(clase_ternaria== "BAJA+2", 117000, -3000)) ] ]

      ImprimirGraficos( tb_ganancias, modelo_rank, iteracion_bayesiana, sem )
    }

    #borro y limpio la memoria para la vuelta siguiente del for
    rm( modelo_final )
    gc()

  } # end del for semillas

  rm( tb_ganancias )
  #impresion ganancias
  rm( dtrain )
  rm( parametros )
  gc()

} # end  for  modelo_rank

#------------------------------------------------------------------------------
if( future_con_clase )
{
  OUTPUT$ganancias_suavizadas  <- vganancias_suavizadas
}

OUTPUT$time$end  <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#dejo la marca final
cat( format(Sys.time(), "%Y%m%d %H%M%S"),"\n",
     file= "zRend.txt",
     append= TRUE )

#------------------------------------------------------------------------------
#suicidio,  elimina la maquina virtual directamente
# para no tener que esperar a que termine una Bayesian Optimization
# sino Google me sigue facturando a pesar de no estar procesando nada
# Give them nothing, but take from them everything.

system( "sleep 10  &&
        export NAME=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/name -H 'Metadata-Flavor: Google') &&
        export ZONE=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/zone -H 'Metadata-Flavor: Google') &&
        gcloud --quiet compute instances delete $NAME --zone=$ZONE",
        wait= FALSE )
