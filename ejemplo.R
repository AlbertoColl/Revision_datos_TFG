### Ejemplo ordenar datos + ANOVA
### Alberto Coll Fernandez - 03/05/2023
### Esto es el encabezado, se pone con # y se usa para poner comentarios

# Todas lo que empiece por # no se ejecuta
# A continuacion hago un apartado, terminando el comentario con cuatro rayitas
# Lo puedes abrir y cerrar con esa flechita de la izquierda

### Importar los datos ----

# Primer paso y muy importante, elegir directorio de trabajo con la funcion setwd()
# En mi caso es este pero tienes que cambiarlo tu en tu ordenador
# Ojo con las barras que a veces da error por usar \ en vez de /

setwd("D:/collf/Documents/GitHub/TFG-Alberto-Coll")

# Ahora vamos a cargar el paquetes que vamos a usar, tidyverse.
#La primera vez necesitas instalarlo:
install.packages("tidyverse")

# El resto de veces que uses R, ya estara instalado y solo necesitas cargarlo asi:
library(tidyverse)

# Y ahora queda importar tus datos. Guardalos como csv desde Excel porque es muchisimo mas sencillo

datos <-  read.csv2("./datos/ejemplo_pajaros.csv", encoding = "latin1")

# No solo lo hemos cargado, sino que lo hemos guardado como un objeto llamado datos
# Puedes ver tus datos si le das en el "environment" o si usas la funcion view()
view(datos)

### Ordenar los datos ----

#Ahora ya tenemos los datos cargados, y mas o menos ordenados, pero faltan cosas

summary(datos)
# Si usas esto ves un resumen de tus variables. Vemos que tramo estan guardadas como numero, pero realmente no son variables continuas, sino que son factores (solo puede tomar los valores 1, 2, 3 y 4 porque son categorias). Eso hay que arreglarlo:

datos$tramo <- as.factor(datos$tramo)
# La funcion as.factor() devuelve lo que le das pero convertido en factores. Luego se la hemos vuelto a asignar a los datos para sustituirlos.

summary(datos) # Si vuelves a ver esto, se ha arreglado, pero tenemos una situacion similar con "mes", asi que repetimos la operacion:
datos$mes <- as.factor(datos$mes)


