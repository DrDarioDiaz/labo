# Hibridacion de semillerios
# Corre en apenas segundos
# El metodo simple  es el promedio simple de las probabilidades de los semmillerios
# El metodo ponder  pondera (multiplica) por la cantidad de semillas de ese semillerio
# ambos metodos dan MUY parecido

#Necesita para correr en Google Cloud
#  32 GB de memoria RAM
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
PARAM$experimento  <- "HB8010"

PARAM$exp_input  <- c( "ZZ7990-01", "ZZ7990-66", "ZZ7990-53", "ZZ7990-52" )  # el inpput deben ser semillerios

PARAM$kaggle$envios_desde  <-  9500L
PARAM$kaggle$envios_hasta  <- 11500L
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
# verifico que existan los semillerios

verificacion_rapida  <- function( home_dir, exp_input )
{
  # Primera comprobacion rapida, veo que sean por lo menos dos semillerios
  if( length(exp_input) < 2 )  stop( "Fatal Error. PARAM$exp_input   debe tener por lo menos dos elementos" )

  # Segunda comprobacion rapida, veo que existan los experimentos
  i  <- 1L
  while( dir.exists(file.path(home_dir, "exp", exp_input[i] )) )  i  <- i + 1L
  if( i <= length( exp_input ) )   stop( "Fatal Error. No existe el experimento ", exp_input[i], "\n" )

  # Tercera comprobacion rapida, veo que existan archivos csv para todos los experimentos
  i  <- 1L
  while( length( list.files( file.path(home_dir, "exp", exp_input[i] ),
                             pattern= "^pred_*" ) ) > 0L  )    { i  <- i + 1L }

  if( i <= length( exp_input ) )   stop( "Fatal Error. En la carpeta ", exp_input[i], " No existe archivo pred \n" )
}
#------------------------------------------------------------------------------
# generacion de archivos para Kaggle, cuando future no tiene clase

GenerarKaggle  <- function( tb_prediccion, infijo )
{
  # genero el vector de cortes
  cortes  <- seq( from=  PARAM$kaggle$envios_desde,
                  to=    PARAM$kaggle$envios_hasta,
                  by=    PARAM$kaggle$envios_salto )

  # grabo los archivos para cada corte
  for( corte in cortes )
  {
    tb_prediccion[ , Predicted  := 0L ]
    tb_prediccion[ 1:corte, Predicted  := 1L ]

    nom_submit  <- paste0( PARAM$experimento,
                           "_", infijo, "_",
                           sprintf( "%05d", corte ),
                             ".csv" )

    fwrite( tb_prediccion[ , list( numero_de_cliente, Predicted ) ],
            file= nom_submit,
            sep= "," )

    tb_prediccion[ , Predicted  := NULL ]
  }
}
#------------------------------------------------------------------------------
# impresion de los graficos cuando conozco la clase, cuando future tiene clase

ImprimirGraficos  <- function( tb_ganancias, semillerios_qty )
{
  # calculo la mayor ganancia  SUAVIZADA
  tb_ganancias[ , gan_simple_suavizada := frollmean( x= ganancia_acum_simple,
                                                     n= PARAM$graficar$ventana_suavizado,
                                                     align= "center",
                                                     na.rm= TRUE,
                                                     hasNA= TRUE )]

  tb_ganancias[ , gan_ponder_suavizada := frollmean( x= ganancia_acum_ponder,
                                                     n= PARAM$graficar$ventana_suavizado,
                                                     align= "center",
                                                     na.rm= TRUE,
                                                     hasNA= TRUE )]

  ganancia_simple_suavizada_max  <- tb_ganancias[ , max( gan_simple_suavizada, na.rm= TRUE ) ]
  ganancia_ponder_suavizada_max  <- tb_ganancias[ , max( gan_ponder_suavizada, na.rm= TRUE ) ]

  ymax  <- max( tb_ganancias, na.rm= TRUE ) * 1.05

  arch_grafico  <- paste0( "modelo_hibridacion.pdf" )

  pdf( arch_grafico )

 # primera curva
  plot( x= tb_ganancias[ , envios],
        y= tb_ganancias[ , g1],
        type= "l",
        col= "gray",
        xlim= c( 0, PARAM$graficar$envios_hasta ),
        ylim= c( 0, ymax ),
        main= paste("semillerios: ",
                    semillerios_qty,
                    "simple", as.integer(ganancia_simple_suavizada_max),
                    "pond", as.integer(ganancia_ponder_suavizada_max) ),
        xlab= "Envios",
        ylab= "Ganancia",
        panel.first= grid()
     )

  #las siguientes curvas
  if( semillerios_qty > 1 )
  {
    for( s in 2:semillerios_qty )
    {
      lines( x= tb_ganancias[ , envios],
             y= tb_ganancias[ , get(paste0("g", s)) ],
             col= "gray" )
    }
  }

  # finalmente las curva de Hibridacion
  # la AZUL son las probs  acumuladas simples
  lines( x= tb_ganancias[ , envios ],
         y= tb_ganancias[ , ganancia_acum_simple ],
         col= "blue" )

  # la ROJA son las probs  acumuladas simples
  lines( x= tb_ganancias[ , envios ],
         y= tb_ganancias[ , ganancia_acum_ponder ],
         col= "red" )

  dev.off()


  #grabo las ganancias, para poderlas comparar con OTROS modelos
  arch_ganancias  <- paste0( "ganancias.txt" )

  fwrite( tb_ganancias,
          file= arch_ganancias,
          sep= "\t",
        )

  # devuelvo las ganancias suavizadas
  return( list( "simple"= ganancia_simple_suavizada_max,
                "ponder"= ganancia_ponder_suavizada_max ) )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

OUTPUT$PARAM  <- PARAM
OUTPUT$time$start  <- format(Sys.time(), "%Y%m%d %H%M%S")

# Compruebo que al menos existe lo necesario para empezar a trabajar
verificacion_rapida( PARAM$home, PARAM$exp_input )

#creo la carpeta donde va el experimento
dir.create( paste0( PARAM$home, "exp/", PARAM$experimento, "/"), showWarnings= FALSE )
setwd(paste0( PARAM$home, "exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

GrabarOutput()
write_yaml( PARAM, file= "parametros.yml" )   #escribo parametros utilizados


# inicializo
semillerios_qty  <- 0
global_periodos <- c()
primera_vez  <- TRUE

for( exp_input  in  PARAM$exp_input )
{
  cat( "\nExperimento : ", exp_input, "  " )
  predicciones  <- list.files( file.path( PARAM$home, "exp", exp_input),
                               pattern= "^pred_*",
                               full.names= TRUE )

  # recorro todas las predicciones de ese experimento,  todos los  PARAM$modelos_rank
  for( arch_prediccion  in  predicciones )
  {
    cat( arch_prediccion, " " )

    dataset  <- fread( arch_prediccion )
    if(  !( "semillas" %in%  colnames(dataset)) )
      stop( "En el experimento ", exp_input, " el archivo ", arch_prediccion, " no es de un semillerio.\n" )

    semillerios_qty  <- semillerios_qty + 1

    if( primera_vez )  #inicializo  tb_hibridacion
    {
      primera_vez  <- FALSE

      future_con_clase  <- dataset[ clase_ternaria== "" | is.na(clase_ternaria), .N ] == 0
      if( future_con_clase )
      {
        tb_ganancias  <- as.data.table( list( "envios"= 1:1:PARAM$graficar$envios_hasta ) )
        tb_ganancias[ , gan_sum := 0.0 ]
      }

      global_periodos  <- dataset[ , unique( foto_mes ) ]
      tb_hibridacion  <- copy(dataset[ , list( numero_de_cliente, foto_mes, clase_ternaria ) ])

      # iniciliazo los campos de la tabla tb_hibridacion
      tb_hibridacion[ , prob_hibridacion_simple := 0 ]   #acumulo la probabilidad simple
      tb_hibridacion[ , prob_hibridacion_ponder := 0 ]   #acumulo la probabilidad ponderada por la cantidad de semillas
      tb_hibridacion[ , semillerios := 0L ]              #cuento cuantos semillerios voy acumulando
      tb_hibridacion[ , semillas := 0L ]                 #sumo la cantidad de semillas de TODOS los semillerios
    }

    # verifico que no intentar joinear periodos distintos
    periodos  <- dataset[ , unique( foto_mes ) ]
    if( ! setequal( periodos, global_periodos ) )
     stop( "En el experimento ", exp_input, " el archivo ", arch_prediccion, " hay periodos extraños.\n" )

    # Hago el join y acumulo
    tb_hibridacion[ dataset,
                    on= c("numero_de_cliente", "foto_mes"),
                    c( "prob_actual", "prob_hibridacion_simple", "prob_hibridacion_ponder", "semillas", "semillerios") :=
                    list( i.prob,
                          prob_hibridacion_simple  + i.prob,
                          prob_hibridacion_ponder  + i.prob * i.semillas,   #probabilidad ponderada por la cantidad de semillas del semillerio
                          semillas  + i.semillas ,   # acumula la cantidad de semillas
                          semillerios  + 1 ) ]       # acumula la cantidad de semillerios, es decir, modelos

    # si la clase no es vacia
    if( future_con_clase )  #almaceno las ganancias de cada semillerio
    {
      setorder( tb_hibridacion, -prob_actual )
      tb_ganancias[ , paste0( "g", semillerios_qty )  := tb_hibridacion[ 1:PARAM$graficar$envios_hasta, cumsum( ifelse(clase_ternaria== "BAJA+2", 117000, -3000)) ] ]
    }

  }
}
cat("\n")


# si la clase tiene valores, imprimo los graficos de los semillerios
if( future_con_clase )
{
  setorder( tb_hibridacion, -prob_hibridacion_simple )
  tb_ganancias[ , ganancia_acum_simple  := tb_hibridacion[ 1:PARAM$graficar$envios_hasta, cumsum( ifelse(clase_ternaria== "BAJA+2", 117000, -3000)) ] ]

  setorder( tb_hibridacion, -prob_hibridacion_ponder )
  tb_ganancias[ , ganancia_acum_ponder  := tb_hibridacion[ 1:PARAM$graficar$envios_hasta, cumsum( ifelse(clase_ternaria== "BAJA+2", 117000, -3000)) ] ]

  gan_suavizadas  <- ImprimirGraficos( tb_ganancias, semillerios_qty )
}


# si la clase es vacia, genero las salidas para Kaggle
if( ! future_con_clase )
{
  setorder( tb_hibridacion, - prob_hibridacion_simple )
  GenerarKaggle( tb_hibridacion, "hibrid_simple" )

  Sys.sleep( 2 )

  setorder( tb_hibridacion, - prob_hibridacion_ponder )
  GenerarKaggle( tb_hibridacion, "hibrid_ponder" )
}


# grabo la tabla de hibridacion
tb_hibridacion[ , prob_actual := NULL ]  # ya no necesito el campo prob_actual
tb_hibridacion[ , prob_hibridacion_simple :=  prob_hibridacion_simple/semillerios ]
tb_hibridacion[ , prob_hibridacion_ponder :=  prob_hibridacion_ponder/semillas ]

fwrite( tb_hibridacion,
        file= "pred_hibridacion.csv",
        sep= "\t" )

#------------------------------------------------------------------------------
if( future_con_clase )
{
  OUTPUT$ganancias_suavizadas$simple   <- as.integer( gan_suavizadas$simple )
  OUTPUT$ganancias_suavizadas$ponder   <- as.integer( gan_suavizadas$ponder )
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
 