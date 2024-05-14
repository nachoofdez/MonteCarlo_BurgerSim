#Cargar bibliotecas
library(dplyr); 
library(tidyr);

#Semilla aleatoria
set.seed(12316) 

#PARÁMETROS SIMULACIÓN:
n_simulaciones <- 1; #Numero días laborables en un mes
n_servidores <- 2; #Numero de servidores/chefs
Hora = 720; # Tiempo de apertura hamburgueseria: 9:00 - 21:00 

lambdaL1 = 1/7.; #Tiempo Prod pan cada 7 minutos 
lambdaL2 = 1/7.; #Tiempo Prod carne
lambdaL<-c(lambdaL1,lambdaL2);
lambdaS <- 1/8.; # Tasa servicio chefs: cada 8 minutos (montaje hamburguesa)

lambdaLC <- 1/7. # Tasa Llegada cliente
lambdaSC <- 1/1. # Tasa servicio de pago para adquirir la hamburguesa

#Tiempos maximo en cola para las hamburguesas y clientes->
TmaxH = 40; #Tiempo maximo hamburguesas en cola antes de desecharla

#El tiempo abandono clientes cola va a seguir una Gamma de media 25:
MediaC <- 25;
formaC<- 5; 
escalaC<- MediaC / formaC;

#LISTAS PARA ALMACENAR LOS RESULTADOS POR SIMULACIÓN:
beneficios_por_dia = c()
gastos_por_dia = c()
ingresos_por_dia = c()
Tiempo_medio_clientes_cola_por_dia = c()
Tiempo_medio_hamburguesas_cola_por_dia = c()
Tiempo_medio_panes_cola_por_dia = c()
Tiempo_medio_carnes_cola_por_dia = c()
Numero_medio_clientes_cola_por_dia = c()
Numero_medio_hamburguesas_cola_por_dia = c()
Numero_medio_panes_cola_por_dia = c()
Numero_medio_carnes_cola_por_dia = c()
Tiempo_medio_clientes_sistema_por_dia = c()
Numero_medio_clientes_sistema_por_dia = c()
Numero_medio_chefs_ocupados_por_dia = c()
Tiempo_medio_hamburguesas_sistema_por_dia = c()
Numero_medio_hamburguesas_sistema_por_dia = c()
Porcentaje_abandono_clientes_por_dia = c()
Porcentaje_abandono_hamburguesas_por_dia = c()

#FUNCIONES AUXILIARES SIMULACIÓN DETECCIÓN SIGUIENTE EVENTO
es_llegada_pan_carne <-function(nextS, nextL, n_servidores, nextSC, nextLC,
                                nextSColaC,nextSColaH){
  return( ((sum(is.nan(nextS)) == n_servidores) || 
             (min(nextL) <= min(nextS, na.rm = TRUE))) &&
            (min(nextL) <= min(nextLC,nextSC,nextSColaC,
                               nextSColaH, na.rm = TRUE)) )
}

es_montaje <-function(nextS, nextL, n_servidores, nextSC, nextLC, nextSColaC,
                      nextSColaH){
  return( (sum(is.nan(nextS)) != n_servidores) && 
            min(nextS, na.rm = TRUE) < min(nextL, nextLC,nextSColaH,nextSColaC, 
                                           nextSC, na.rm = TRUE) )
}

es_llegada_cliente <- function(nextS, nextL, nextSC, nextLC, 
                               nextSColaC, nextSColaH){
  return( nextLC <= min(nextL, nextS, nextSC,
                        nextSColaH,nextSColaC, na.rm = TRUE) )
}

es_venta_hamburguesa <- function(nextS, nextL, nextSC, nextLC, nextSColaC,
                                 nextSColaH){
  return((!is.nan(nextSC)) && (nextSC <= min(nextL, nextS, nextLC,nextSColaH,
                                              nextSColaC, na.rm = TRUE)))
}

es_abandono_cliente <- function(nextS, nextL, nextSC, nextLC, nextSColaC,
                                nextSColaH){
  return( min(nextSColaC,na.rm=T) <= min(nextL, nextS, nextLC,nextSColaH,
                                         nextSC, na.rm = TRUE) )
}

#ITERACIÓN SOBRE EL NÚMERO DE DIAS SIMULADOS:
for (i in 1:n_simulaciones){
  
  #INICIALIZACIÓN VARIABLES:
  t = 0; 
  # Colas y servicios para crear las hamburguesas->
  Cola1 = 0; #Numero panes en cola
  Cola2 = 0; #Numero carnes en cola
  Servicio = 0;  #Numero de hamburguesas haciendose por el chef
  Llegadas = c(0, 0); #Número de panes y carnes producidas.
  Cola<-c(Cola1,Cola2);
  nextL1 = rexp(1, rate = lambdaL1);  #Próximo pan hecho
  nextL2=rexp(1,rate=lambdaL2);     #Próxima carne hecha
  nextL <- c(nextL1, nextL2);
  nextS = rep(NaN, n_servidores); #Proxima salida hamburguesa hecha
  Hamburguesas_vendidas <- 0;
  
  #Cola y servicio para la compra de Hamburguesas->
  ColaH = 0; # Hamburguesas en cola
  ColaC = 0; # Clientes en cola
  ServicioC = 0;  #Clientes en servicio
  LlegadasC = 0;  #Numero llegadas Clientes
  LlegadasH = 0;  #Numero de Hamburguesas hechas
  nextLC = rexp(1, rate = lambdaLC);  #Proxima llegada cliente
  nextSC = NaN;  #proxima salida cliente
  nextSColaH <- NaN  #Proximos tiempos de abandono cola hamburguesas
  nextSColaC <- NaN  #Proximos tiempos de abandono cola clientes
  AbandonoC <- 0;  #Numero de clientes que han abandonado la cola
  AbandonoH <- 0;  #Numero de hamburguesas desechadas
  
  stay = min(nextL, nextLC); #Estado inicial
  
  #Creamos tibble para almacenar datos:
  data_Hamburgueseria <- tibble(
    t, ColaH, ColaC, ServicioC, LlegadasC, LlegadasH, nextLC,
    nextSC, Cola=list(Cola), Servicio, Llegadas = list(Llegadas), 
    stay, nextL=list(nextL), nextS = list(nextS),
    nextSColaC = list(nextSColaC),nextSColaH=list(nextSColaH),
    AbandonoC, AbandonoH, Hamburguesas_vendidas)%>%
    unnest_wider(nextS, names_sep = '_')  %>%
    unnest_wider(nextL, names_sep = '_') %>%
    unnest_wider(Cola, names_sep = '_') %>%
    unnest_wider(Llegadas, names_sep = '_')
  
  #### SIMULACIÓN EVENTO####
  
  while (min(c(nextL, nextS, nextLC, nextSC, nextSColaC, 
               nextSColaH), na.rm = TRUE) <= Hora) {
    #Evento: Llegada de pan y carne
    if (es_llegada_pan_carne(nextS, nextL, n_servidores, nextSC, 
                             nextLC, nextSColaC,nextSColaH)) {
      # Identificar la llegada más rápida
      i <- which.min(nextL)
      t <- nextL[i]
      Llegadas[i] <- Llegadas[i] + 1
      
      #Calcular nuevo instante de llegadas
      nextL[i] <- t + rexp(1, rate = lambdaL[i])
      
      # Determinar el índice del otro producto (1 si i=2, 2 si i=1)
      j <- 3-i 
      
      if (Servicio >= n_servidores || Cola[j] == 0) { 
        # Si no esta el otro producto disponible o el servicio completo
        Cola[i] <- Cola[i] + 1
        
      } else { 
        # Si ambos productos están disponibles, pasar a servicio
        Servicio <- Servicio + 1
        Cola[j] <- Cola[j] - 1
        
        if (sum(is.nan(nextS)) == n_servidores) {
          # Si todos los servidores están vacíos, asignar al primer servidor
          nextS[1] <- t + rexp(1, rate = lambdaS)
          
        } else {
          # Si algún servidor está libre, asignar al servidor libre
          i_serv <- min(which(is.nan(nextS)))
          nextS[i_serv] <- t + rexp(1, rate = lambdaS)
        }
      }
      
    }
    #Evento: Montaje de Hamburguesa
    else if (es_montaje(nextS, nextL, n_servidores, nextSC, nextLC, 
                        nextSColaC, nextSColaH)){
      # Identificar el instante en el que se produce la salida
      t = min(nextS, na.rm = T);
      i_serv <- min(which(t == nextS)) #Servidor en el que se produce la salida
      
      if (Cola[1] > 0 && Cola[2] > 0){ # Si hay pan y carne en cola
        # Utilizar el pan y la carne, quitarlos de la cola actualizar 
        #el tiempo salida de servicio 
        Cola[1] = Cola[1] - 1;
        Cola[2]=Cola[2] - 1;
        nextS[i_serv] <- t + rexp(1, rate = lambdaS);
        
      } else { # Si no hay pan y carne
        # Decrementar las hamburguesas en servicio y 
        #marcar el servidor como disponible 
        Servicio = Servicio - 1;
        nextS[i_serv] <- NaN;
      }
      
      LlegadasH = LlegadasH+1;
      
      # Determinar si colocar la hamburguesa en cola o venderla directamente
      if (ColaC == 0 || ServicioC >= 1){
        # Colocar la hamburguesa en cola
        ColaH = ColaH+1;
        TFria <- t + TmaxH;
        nextSColaH <- c(nextSColaH,TFria);
        
      } else{
        # Vender la hamburguesa directamente
        ColaC = ColaC-1;
        TminAbandono <- min(nextSColaC, na.rm=T);
        nextSColaC <- nextSColaC[nextSColaC != TminAbandono];
        ServicioC = ServicioC+1;
        nextSC <- t + rexp(1, rate = lambdaSC);
      }
      
    } 
    #Evento: Llegada cliente
    else if (es_llegada_cliente(nextS, nextL, nextSC, nextLC, nextSColaC,
                                nextSColaH)){
      # Actualizar el tiempo actual y calcular el próximo 
      #instante de llegada de cliente
      t = nextLC;
      nextLC = t + rexp(1, rate = lambdaLC);
      
      LlegadasC = LlegadasC+1;
      
      if (ColaH == 0 || ServicioC >= 1){
        # Si no hay hamburguesas disponibles o hay al menos una en servicio, 
        #el cliente se une a la cola de clientes
        ColaC = ColaC+1;
        TAbandonoC <- t+  rgamma(n =1 , shape = formaC, scale = escalaC);
        nextSColaC <- c(nextSColaC, TAbandonoC)
        rm(TAbandonoC)
        
      } else {
        # Si hay hamburguesas disponibles, se procede a la venta directa
        ColaH = ColaH-1;
        TminFria <- min(nextSColaH, na.rm=T);
        nextSColaH <- nextSColaH[nextSColaH != TminFria];
        ServicioC = ServicioC+1;
        nextSC <- t + rexp(1, rate = lambdaSC);
      }
    }
    #Evento: Servicio venta de hamburguesas
    else if(es_venta_hamburguesa(nextS, nextL, nextSC, nextLC, nextSColaC,
                                 nextSColaH)){ 
      # Actualizar el tiempo actual y registrar la venta de una hamburguesa
      t = nextSC;
      Hamburguesas_vendidas<-Hamburguesas_vendidas+1;
      
      if (ColaC > 0 && ColaH > 0){
        # Si hay clientes y hamburguesas en cola, entra otro a servicio de caja
        ColaC = ColaC-1;
        TminAbandono <- min(nextSColaC, na.rm=T);
        nextSColaC <- nextSColaC[nextSColaC != TminAbandono];
        ColaH = ColaH-1;
        TminFria <- min(nextSColaH, na.rm=T);
        nextSColaH <- nextSColaH[nextSColaH != TminFria];
        # Establecer el próximo instante de servicio
        nextSC = t + rexp(1, rate = lambdaSC);
        
      } else {
        #Si no hay clientes o hamburguesas hechas, no se entra a servicio de caja
        ServicioC = 0;
        nextSC = NaN;
      }
    }
    #Evento: Abandono de cliente
    else if(es_abandono_cliente(nextS, nextL, nextSC, nextLC, nextSColaC,
                                nextSColaH)){
      # Reducir la cola de clientes y registrar el abandono de un cliente
      ColaC=ColaC-1;
      AbandonoC <- AbandonoC+1;
      
      # Identificar el próximo instante de abandono y 
      #actualizar el tiempo actual
      TminAbandono <- min(nextSColaC, na.rm=T);
      t <- TminAbandono;
      nextSColaC <- nextSColaC[nextSColaC != TminAbandono];
    }
    #Evento: Hamburguesa fría y abandono de la cola
    else{ 
      # Reducir la cola de hamburguesas y registrar 
      # el abandono de una hamburguesa
      ColaH=ColaH-1;
      AbandonoH <- AbandonoH +1;
      
      # Identificar el próximo instante de abandono y 
      #actualizar el tiempo actual
      TminFria <- min(nextSColaH, na.rm=T);
      t <- TminFria;
      nextSColaH <- nextSColaH[nextSColaH != TminFria];
    }
    
    stay = min(nextL,nextS,nextSC,nextLC,nextSColaC,nextSColaH,Hora, na.rm=T)-t;
    
    #Actualizamos el tibble tras la simulación del evento.
    data_Hamburgueseria <- rbind(
      data_Hamburgueseria, 
      tibble(
        t, ColaH, ColaC, ServicioC, LlegadasC, LlegadasH, nextLC, nextSC, 
        Cola=list(Cola), Servicio, Llegadas=list(Llegadas), stay, 
        nextL=list(nextL), nextS = list(nextS),nextSColaC = list(nextSColaC),
        nextSColaH = list(nextSColaH), AbandonoC, AbandonoH, 
        Hamburguesas_vendidas) 
      %>%
        unnest_wider(nextS, names_sep = '_') %>%
        unnest_wider(nextL, names_sep = '_') %>%
        unnest_wider(Cola, names_sep = '_') %>%
        unnest_wider(Llegadas, names_sep = '_')
    )
  }
  
  #ULTIMA ITERACIÓN CIERRE:
  
  #Si hay justo un cliente comprando al cerrar, le damos la hamburguesa
  #y cerramos. Los clientes y hamburguesas que se quedan en cola los 
  #consideramos como que abandonan.
  stay = 0
  if (ServicioC>0){
    Hamburguesas_vendidas<-Hamburguesas_vendidas+1;
    stay = nextSC - Hora;
  }
  
  if (ColaC>0){
    AbandonoC<-AbandonoC+ColaC;
    ColaC<-0;
    nextSColaC<-NaN;
  }
  
  if (ColaH>0){
    AbandonoH<-AbandonoH+ColaH;
    ColaH<-0;
    nextSColaH<- NaN;
  }
  t = Hora;
  #Actualización final tibble:
  data_Hamburgueseria <- rbind(
    data_Hamburgueseria, 
    tibble(
      t, ColaH, ColaC, ServicioC, LlegadasC, LlegadasH, nextLC, nextSC, 
      Cola=list(Cola), Servicio, Llegadas=list(Llegadas), stay, 
      nextL=list(nextL), nextS = list(nextS),nextSColaC = list(nextSColaC),
      nextSColaH = list(nextSColaH), AbandonoC, AbandonoH, 
      Hamburguesas_vendidas) 
    %>%
      unnest_wider(nextS, names_sep = '_') %>%
      unnest_wider(nextL, names_sep = '_') %>%
      unnest_wider(Cola, names_sep = '_') %>%
      unnest_wider(Llegadas, names_sep = '_')
  )
  
  #CÁLCULO Y ACUMULACIÓN ESTADÍSTICAS SIMULACIÓN UN DÍA
  n_eventos <- length(data_Hamburgueseria$t)
  n_clientes <- data_Hamburgueseria$LlegadasC[n_eventos]
  n_hamburguesas <- data_Hamburgueseria$LlegadasH[n_eventos]
  n_panes <- data_Hamburgueseria$Llegadas_1[n_eventos]
  n_carnes <- data_Hamburgueseria$Llegadas_2[n_eventos]
  tiempo_total <- data_Hamburgueseria$t[n_eventos]
  
  # 1) Tiempo medio clientes en cola
  WqC <- sum(data_Hamburgueseria$ColaC * (data_Hamburgueseria$stay/n_clientes))
  Tiempo_medio_clientes_cola_por_dia<-c(Tiempo_medio_clientes_cola_por_dia,WqC)
  
  # 2) Tiempo medio hamburguesas en cola
  WqH <- sum(data_Hamburgueseria$ColaH * 
               (data_Hamburgueseria$stay/n_hamburguesas))
  Tiempo_medio_hamburguesas_cola_por_dia<-
    c(Tiempo_medio_hamburguesas_cola_por_dia,WqH)
  
  # 3) Tiempo medio panes en cola
  WqP <- sum(data_Hamburgueseria$Cola_1 * (data_Hamburgueseria$stay/n_panes))
  Tiempo_medio_panes_cola_por_dia<-c(Tiempo_medio_panes_cola_por_dia,WqP)
  
  # 4) Tiempo medio carnes en cola
  WqCarn <- sum(data_Hamburgueseria$Cola_2 * 
                  (data_Hamburgueseria$stay/n_carnes))
  Tiempo_medio_carnes_cola_por_dia<-c(Tiempo_medio_carnes_cola_por_dia,WqCarn)
  
  # 5) Porcentaje abandono de clientes
  n_abandonos_clientes <- data_Hamburgueseria$AbandonoC[n_eventos]
  porcentaje_abandono_clientes <- n_abandonos_clientes/n_clientes
  Porcentaje_abandono_clientes_por_dia<-c(Porcentaje_abandono_clientes_por_dia,
                                          porcentaje_abandono_clientes)
  
  # 6) Porcentaje hamburguesas perdidas
  n_abandonos_hamburguesas <- data_Hamburgueseria$AbandonoH[n_eventos]
  porcentaje_abandono_hamburguesas <- n_abandonos_hamburguesas/n_hamburguesas
  Porcentaje_abandono_hamburguesas_por_dia<-
    c(Porcentaje_abandono_hamburguesas_por_dia,
      porcentaje_abandono_hamburguesas)
  
  # 7) Ingresos totales
  precio_ham <- 9.90
  n_hamburguesas_vendidas <- 
    data_Hamburgueseria$Hamburguesas_vendidas[n_eventos]
  ingresos <- n_hamburguesas_vendidas*precio_ham
  ingresos_por_dia <- c(ingresos_por_dia,ingresos)
  
  # 8) Gastos totales
  sueldo_chef <- 50
  coste_pan <- 1
  coste_carne <- 3
  gastos <- sueldo_chef*n_servidores + coste_pan*n_panes + coste_carne*n_carnes
  gastos_por_dia <- c(gastos_por_dia, gastos)
  
  # 9) Beneficios totales
  beneficios <- ingresos - gastos
  beneficios_por_dia <- c(beneficios_por_dia, beneficios)
  
  # 10) Media clientes en cola
  LqC <- sum(data_Hamburgueseria$ColaC*(data_Hamburgueseria$stay/tiempo_total))
  Numero_medio_clientes_cola_por_dia<-c(Numero_medio_clientes_cola_por_dia,LqC)
  
  # 11) Media hamburguesas en cola
  LqH <- sum(data_Hamburgueseria$ColaH*(data_Hamburgueseria$stay/tiempo_total))
  Numero_medio_hamburguesas_cola_por_dia<-
    c(Numero_medio_hamburguesas_cola_por_dia,LqH)
  
  # 12) Media panes en cola
  LqP <- sum(data_Hamburgueseria$Cola_1*(data_Hamburgueseria$stay/tiempo_total))
  Numero_medio_panes_cola_por_dia<-c(Numero_medio_panes_cola_por_dia,LqP)
  
  # 13) Media carnes en cola
  LqCarn <- sum(data_Hamburgueseria$Cola_2*
                  (data_Hamburgueseria$stay/tiempo_total))
  Numero_medio_carnes_cola_por_dia<-c(Numero_medio_carnes_cola_por_dia,LqCarn)
  
  # 14) Tiempo medio clientes en el sistema
  WC <- sum((data_Hamburgueseria$ColaC + data_Hamburgueseria$ServicioC)
            *data_Hamburgueseria$stay/n_clientes)
  Tiempo_medio_clientes_sistema_por_dia<-
    c(Tiempo_medio_clientes_sistema_por_dia,WC)
  
  # 15) Numero medio clientes en el sistema
  LC <- sum((data_Hamburgueseria$ColaC + data_Hamburgueseria$ServicioC)
            *data_Hamburgueseria$stay/tiempo_total)
  Numero_medio_clientes_sistema_por_dia<-
    c(Numero_medio_clientes_sistema_por_dia,LC)
  
  # 16) Tiempo medio de hamburguesas en el sistema
  WH <- sum((data_Hamburgueseria$ColaH + data_Hamburgueseria$ServicioC)
            *data_Hamburgueseria$stay/n_hamburguesas)
  Tiempo_medio_hamburguesas_sistema_por_dia<-
    c(Tiempo_medio_hamburguesas_sistema_por_dia,WH)
  
  # 17) Numero medio de hamburguesas en el sistema
  LH <- sum((data_Hamburgueseria$ColaH + data_Hamburgueseria$ServicioC)*
              data_Hamburgueseria$stay/tiempo_total)
  Numero_medio_hamburguesas_sistema_por_dia<-
    c(Numero_medio_hamburguesas_sistema_por_dia,LH)
  
  #18) Numero medio de servidores ocupados en el sistema
  Media_chefs_ocupados <- sum(data_Hamburgueseria$Servicio*
                                (data_Hamburgueseria$stay/tiempo_total))
  Numero_medio_chefs_ocupados_por_dia<-
    c(Numero_medio_chefs_ocupados_por_dia,Media_chefs_ocupados)
}

