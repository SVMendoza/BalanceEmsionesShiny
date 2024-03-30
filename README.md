# BalancesEmisionesShiny

Repositorio para una aplicación Shiny que permite generar reportes de balances de emisiones de carbono. La aplicación Shiny, simplifica los procesos de estimación de Reducciones de Emisiones de la Deforestación y Degradación, enfocado a datos de actividad, incorporando la propagación del error en todos los pasos mediante el Método de Monte Carlo.

<p> R </p>


--------------------------------------------------------

source(paste0(Dir, '\\Ejecutar.r'))

-------------------------------------------------------

Agregar el directorio de la aplicación

-------------------------------------------------------

Dir<-'' 

-------------------------------------------------------

# Entrada por componente de carbono

![img](https://github.com/SVMendoza/BalanceEmsionesShiny/blob/main/Entrada.png)

# Resultado de la Simulación. Suma de los distintos componentes y propagación de la incertidumbre.

![img](https://github.com/SVMendoza/BalanceEmsionesShiny/blob/main/ResultadoSimulacion.png)