# Proyecto_1

Para la ejecucion del proyecto se tiene en cuenta las siguientes opciones que se hace
llamando al Main(weeks, time, queue,agent)
donde:
> Weeks son cuantas semanas de trabajo.
> Time tiempo de espera.
> Queue, cantidad maxima que recibira el banco.
> Agent, cuantos agentes estarán atendiendo.

Por lo que tiene otras funciones que hace parte al main que son las siguientes:
- Tiempo_entrada(n), donde esta la probabilidad de entrada de cada cliente.
- cliente(day), la generacion de entrada de clientes.
- banco_cliente(queue, Clientes), el limite que puede haber de clientes en el banco.
- Agentes(queue,agente, cliente), hace que en verificar si el agente esta desocupado.
- time_service(agent, agente), el tiempo de servicio que hace el agente.
