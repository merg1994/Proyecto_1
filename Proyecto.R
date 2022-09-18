tiempo_entrada<-function(n){
  dist_prob <- matrix(data = c(0.1,0.15,0.1,0.35,0.25,0.05,0,
                               0.1,0.1,0.15,0.2,0.35,0.1,0,
                               0,0.1,0.1,0.2,0.1,0.25,0.25,
                               0,0.25,0.2,0.2,0.15,0.15,0.15,
                               0.15,0.15,0.2,0.2,0.1,0.1,0.1,
                               0.2,0.15,0.1,0.5,0.05,0,0,
                               0.35,0.25,0.2,0.1,0.1,0,0),
                            nrow = 7,ncol = 7,byrow = TRUE)
  p_lun <- dist_prob[1,1:7]
  p_mar <- dist_prob[2,1:7]
  p_mier <-dist_prob[3,1:7]
  p_jue <- dist_prob[4,1:7]
  p_vie <-dist_prob[5,1:7]
  p_sab <- dist_prob[6,1:7]
  p_dom <- dist_prob[7,1:7]
  tiempo<- c(0:6)
  return(sample(tiempo, 1,replace = TRUE, prob = switch(n ,"Lunes"=p_lun,"Martes"=p_mar,"Miercoles"=p_mier,
                                             "Jueves"=p_jue,"Viernes"=p_vie,"Sabado"=p_sab,
                                             "Domingo"=p_dom)))
} 



cliente <- function(day){
  i <- 1
  cliente <- c()
  while(i <= 480){
    client[i] <- tiempo_entrada(day)
    i <- i +1
  }
  return(client)
}

banco_cliente<- function(queue, clientes){
  if(clientes >= queue){
    clientes <- queue
    t<- clientes
  }else{
    t <- clientes
  }
  
  return(t)
  
}

agentes <- function(queue, agente, cliente){
  if(cliente >= 1){
    cliente <- cliente - 1
    banco_cliente(queue, cliente)
    agente <- 1
  }else{
    agente <- 0 
  }
  return(agente)
}

time_service <- function(agent, agente){
  if(agente == 1){
    tiempo <- round(abs(rnorm(agent,8,5)),2)
  }else{
    tiempo <- 0
  }
  return(tiempo)
}

main <- function(weeks, time, queue, agent){#aca es donde recibe cuantos semanas, el tiempo de espera que espera la persona, y la cantidad maxima que el banco recibe.
  tiempo_servicio <- c()
  cola <-queue
  banco <- list()
  hora<- c()
  minuto <- c()
  t <- c()
  dia <- c()
  agente <- rep(0,agent)
  df_banco <- data.frame()
  for(week in 1:weeks){#defino el tamaño de las semanas
    for(day in c("Lunes","Martes","Miercoles","Jueves","Viernes","Sabado","Domingo")){#el dia de hay Lunes, Martes, Miercoles, Jueves, Viernes, Sabado, Domingo)
      i <- 1
      n <- 10
      j <- 0
      k<- 1
      z <- 1
      y <- 1
      cont <- 0
      clientes <- 0
      tiempo <- cliente(day)
      while(i <= 480){
        dia[i]<- day
        timer <- tiempo[k]
        if(j<= 59){
          hora[i]<- n
          minuto[i]<- j
          if(timer == cont){
            clientes <- clientes +1
            t[i]<- banco_cliente(queue, clientes)
            agente[z]<- agentes(queue, agente, t[i])
            tiempo_servicio[i] <- time_service(agent,agente[z])
            z <- z +1 
            cont <- 0
            k <- k +1
          }else{
            t[i]<- banco_cliente(queue, clientes)
            cont <- cont +1
            agente[z]<- agentes(queue, agente, t[i])
            tiempo_servicio[i] <- time_service(agent,agente[z])
            z <- z +1 
          }
          j <- j+1
        }else{
          j<-1
          n<- n+1
          hora[i]<- n
          minuto[i]<- 0
          if(timer == cont){
            clientes <- clientes +1
            t[i]<- banco_cliente(queue, clientes)
            agente[z]<- agentes(queue, agente, t[i])
            tiempo_servicio[i] <- time_service(agent,agente[z])
            z <- z +1 
            k <- k +1
          }else{
            t[i] <- banco_cliente(queue, clientes)
            agente[z]<- agentes(queue, agente, t[i])
            tiempo_servicio[i] <- time_service(agent,agente[z])
            z <- z +1 
            cont <- cont +1
          }
        }
        i<- i+1
        length(t) <- length(hora)
        length(tiempo_servicio) <- length(hora)
        length(agente) <- length(hora)
      }
      banco[[day]] <- list(Dia = as.character(dia), Hora =as.numeric(hora),
                           Minuto = as.numeric(minuto),Cliente = as.numeric(t),
                           Agente = as.numeric(agente),
                           Tiempo_Servicio = as.numeric(tiempo_servicio)
                           )
      df_banco<- rbind(df_banco,as.data.frame(banco[[day]]))
    }
  }
  
  return(View(df_banco))
}

