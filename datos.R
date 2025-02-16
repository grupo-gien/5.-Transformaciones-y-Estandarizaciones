# Scripts del capitulo de transformaciones y estandarizaciones


#-----------
# Ejemplo 1.Histogramas de frecuencia
# Lectura de la base de datos.
aves<-read.csv2("Aves.csv",row.names=1)
# Paquete requerido
library(lattice)
# Realizaci�n de los histogramas
histogram(~Longitud.total, data=aves,nint= 7, 
          xlab = "Longitud Total de Aves (mm)", ylab = "Frecuencias")
histogram(~log10(Longitud.total), data=aves, nint= 7,  
          xlab=expression(log[10]*(Longitud_Total)),ylab="Frecuencias")
histogram(~log(Longitud.total), data=aves, nint= 7, 
          xlab = expression(log[10]*(Longitud_Total)), ylab = "Frecuencias")          
histogram(~sqrt(Longitud.total), data=aves, nint= 7, 
          xlab = "(Longitud Total)^1/2", ylab = "Frecuencias")
histogram(~sqrt(sqrt(Longitud.total)), data=aves, nint= 7, 
          xlab = "Ra�z Cuarta (Longitud Total de Aves)", ylab = "Frecuencias")



#----------------
# Ejemplo 2. Transformaciones y estandarizaciones generales

# Lectura de la base de datos
datos<-read.csv2("Insectos.csv",row.names=1) 
datos<-datos[,2:6]
head(datos)
# Paquete requerido
library (vegan) 

# 1.) Transformaciones monot�nicas
# Transformaci�n ra�z cuadrada
datos.r= sqrt(datos)
head(datos.r)

# Transformaci�n logar�tmica
datos.log= log10(datos)
head(datos.r)

# Frecuencia relativa, por especies
datos.fr= prop.table(datos, margin=NULL)

# Frecuencia relativa de sitios
datos.s.fr <- decostand(datos, "total", MARGIN = 2) 

# Transformaci�n arcoseno
daros.arc1= asin(sqrt(datos.fr))*180/pi
head(daros.arc1)

# Proporciones para transformaci�n arcoseno
datos.prop <- datos / apply(datos,1,sum) 

# Transformaci�n arcoseno
daros.arc2= asin(sqrt(datos.fr))*180/pi
head(daros.arc2)


# 2.) Suavisamiento (Beals)
# Lectura de datos 
datos.belt <- beals(datos)
head(datos.belt)


# 3.) Estandarizaciones 
# 3.1 M�ximo por columnas (variables) 
# M�ximo para especies
datos.c <- decostand(datos, "max",1)
head(datos.c)

# Relaci�n media y desviaci�n - Puntaje Z
datos.s <- scale(datos)
head(datos.s)

# 3.2 Estandarizaci�n para filas (sitios) 
# M�ximo para sitios
datos.f =decostand(datos, "max", 2) 
head(datos.f) 

# Normalizaci�n
datos.norm <- decostand(datos, "normalize")
head(datos.norm) 

# Hellinger 
datos.hell <- sqrt(datos / apply(datos,1,sum))
head(datos.hell) 

# 3.2 Doble estandarizaci�n (filas y columnas)
# Chi Cuadrado
datos.chi <- decostand(datos, "chi.square")
head(datos.chi) 

# Wisconsin
datos.wis <- wisconsin(datos)
head(datos.wis)



# Figuras de cajas con algunas transformaciones para los cole�pteros

par(mfrow = c(2,2))
boxplot(datos$Coleop, sqrt(datos$Coleop), log1p(datos$Coleop), las = 1,cex=5, 
        main = "Transformaciones simples", names = c("datos crudos", "ra�z", "log"), col = "bisque")

boxplot(datos.c$Coleop, datos.fr$Coleop, las = 1, 
        main = "Estandarizaciones para especies",names = c("max", "total"), col = "lightgreen")

boxplot(datos.hell$Coleop, datos.s.fr$Coleop, datos.norm$Coleop, las = 1, 
        main = "Estandarizaciones para sitios",names = c("Hellinger", "total", "norm"), col = "lightblue")

boxplot(datos.chi$Coleop, datos.wis$Coleop, las = 1, 
        main = "Doble estandarizaci�n", names = c("Chi-cuadrado", "Wisconsin"), col = "orange")

# Se cierra el panel dise�ado 
par(mfrow = c(1,1))




