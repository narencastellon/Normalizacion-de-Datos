# Normalizacion-de-Datos

---
title: "Normalización de Datos"
author: "Naren Castellón"
date: "01/26/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Los métodos de normalización de datos se utilizan para hacer que las variables, medidas en diferentes escalas, tengan valores comparables. Estos pasos de preprocesamiento son importantes para la visualización de clústeres y mapas de calor, el análisis de componentes principales y otros algoritmos de aprendizaje automático basados en medidas de distancia.

**Métodos de normalización de datos**

1. Normalización Z-Score (estandarización)
2. Escalar robusto
3. Normalización Min-Max
4. Normalización media
5. Unidad de longitud


**¿Qué es la normalización de datos?**

La normalización de datos es un paso de procesamiento previo de datos en el que ajustamos las escalas de las características para tener una escala de medida estándar. En Machine Learning, también se conoce como escalamiento de características.

**¿Por qué necesitamos la normalización de datos?**
Los algoritmos de aprendizaje automático, como los algoritmos basados en la distancia, los algoritmos basados en el descenso de gradientes, esperan que las funciones se escalen.

¿Por qué estos algoritmos necesitan escalar las características? Para responder a esto, miramos un ejemplo. Tenemos datos de 35 trabajadores con dos variables años de experiencia que van desde 1.1 años - 10.5 años y sus correspondientes salarios que van desde 37k a $122k.

```{r message=FALSE}
library(readr)
salary <- read_csv("C:/Users/naren/OneDrive/Documentos/Proyectos R Markdown/Base de datos/Salary.csv")
head(salary,13)
```

```{r}
summary(salary)
cor(salary)
```

**Diagrama de dispersión**

Ahora trazamos un diagrama de dispersión para ver la distribución de los datos originales


```{r message=FALSE}
library(heatmaply)
heatmaply(salary, 
  xlab = "Salarios",
  ylab = "Años de experiencia", 
  main = "Datos originales")
```

**Los algoritmos basados** en la distancia , como **SVM, K-Means y KNN**, clasifican los objetos al encontrar similitudes entre ellos mediante funciones de distancia. Estos algoritmos son receptivos al tamaño de las variables. Si no escalamos, la característica con una magnitud mayor (Salario), tendrá más influencia que la característica con una magnitud menor (años de experiencia), y esto conduce a sesgos en los datos y también afecta la precisión.

También tenemos **Análisis de componentes principales (PCA)** que requiere escalado ya que intenta capturar la varianza máxima. Si los datos no se escalan, esto podría dar lugar a sesgos, ya que la característica con una escala mayor podría influir en las características de menor escala.

**Los modelos basados en árboles** siguen algunas reglas de clasificación y regresión. Por lo tanto, no se requiere escalado para modelos basados en árboles.

### 1. Normalización Z-Score (estandarización)
La normalización de la puntuación Z transforma xa x 'restando cada valor de las características por la media de la muestra y luego dividiendo por la desviación estándar de la muestra . La media y la desviación estándar resultantes de los valores estandarizados son 0 y 1, respectivamente.

La fórmula de estandarización es:

![](imagenes/z_score1.png)

```{r}
stanardise_salary <-  as.data.frame(scale(salary, center = TRUE, scale = TRUE))
head(stanardise_salary)
```

Calculando algunos estadísticos para años de experiencia

```{r message=FALSE}
library(dplyr)
stanardise_salary %>% summarise(Min = min(YearsExperience,na.rm = TRUE),
                              Q1 = quantile(YearsExperience,probs = .25,na.rm = TRUE),
                              Median = median(YearsExperience, na.rm = TRUE),
                              Q3 = quantile(YearsExperience,probs = .75,na.rm = TRUE),
                              Max = max(YearsExperience,na.rm = TRUE),
                              Mean = mean(YearsExperience, na.rm = TRUE),
                              SD = sd(YearsExperience, na.rm = TRUE),
                              n = n(),
                              Missing = sum(is.na(YearsExperience)))
```

Calculando algunos estadísticos para años de salarios
```{r}
stanardise_salary %>% summarise(Min = min(Salary,na.rm = TRUE),
                               Q1 = quantile(Salary,probs = .25,na.rm = TRUE),
                               Median = median(Salary, na.rm = TRUE),
                               Q3 = quantile(Salary,probs = .75,na.rm = TRUE),
                               Max = max(Salary,na.rm = TRUE),
                               Mean = mean(Salary, na.rm = TRUE),
                               SD = sd(Salary, na.rm = TRUE),
                               n = n(),
                               Missing = sum(is.na(Salary)))
```

Como podemos ver en las tablas de estadísticas descriptivas, la media muestral ~ 0 y la desviación estándar media es 1.

También podemos ver que el valor mínimo y el valor máximo de la estandarización del puntaje z no están delimitados por límites. Aunque la estandarización no está limitada, la media / varianza de la muestra aún se ve afectada por los valores atípicos. Cuando tenemos valores atípicos en los datos, es mejor usar Robust Scalar.

### 2. Escalar robusto (Robust Scalar)
Como se mencionó anteriormente, se puede usar un escalar robusto cuando tenemos valores atípicos en los datos, ya que es robusto de valores atípicos.

El escalar robusto transforma xa x 'restando cada valor de las características por la mediana y dividiéndolo por el rango intercuartílico entre el primer cuartil (cuantil 25) y el tercer cuartil (cuantil 75).

La fórmula de Robust Scalar es:
![](imagenes/robust_scalar1.png)

Creamos nuestra función
```{r}
robust_scalar<- function(x){(x- median(x)) /(quantile(x,probs = .75)-quantile(x,probs = .25))}
robust_scalar_salary <- as.data.frame(lapply(salary, robust_scalar))

head(robust_scalar_salary)
```

Buscamos los estadísticos para los años de experiencia
```{r}
robust_scalar_salary %>% summarise(Min = min(YearsExperience,na.rm = TRUE),
                              Q1 = quantile(YearsExperience,probs = .25,na.rm = TRUE),
                              Median = median(YearsExperience, na.rm = TRUE),
                              Q3 = quantile(YearsExperience,probs = .75,na.rm = TRUE),
                              Max = max(YearsExperience,na.rm = TRUE),
                              Mean = mean(YearsExperience, na.rm = TRUE),
                              SD = sd(YearsExperience, na.rm = TRUE),
                              n = n(),
                              Missing = sum(is.na(YearsExperience)))
```

y para los salarios
```{r}
robust_scalar_salary %>% summarise(Min = min(Salary,na.rm = TRUE),
                               Q1 = quantile(Salary,probs = .25,na.rm = TRUE),
                               Median = median(Salary, na.rm = TRUE),
                               Q3 = quantile(Salary,probs = .75,na.rm = TRUE),
                               Max = max(Salary,na.rm = TRUE),
                               Mean = mean(Salary, na.rm = TRUE),
                               SD = sd(Salary, na.rm = TRUE),
                               n = n(),
                               Missing = sum(is.na(Salary)))
```
Como podemos ver en las tablas de estadísticas descriptivas, la mediana es 0. También podemos ver que la media muestral y la DE no son 0 y 1, respectivamente. Y los valores mínimo y máximo no están limitados.

### 3. Normalización Min-Max
La normalización Min-Max transforma xa x 'al convertir cada valor de las características en un rango entre 0 y 1 , y esto también se conoce como (0–1) Normalización. Si los datos tienen valores negativos, el rango habría estado entre -1 y 1 .

La fórmula para la normalización Mín-Máx es:

![](imagenes/normal_min-max1.png)

Si queremos escalar entre algún conjunto arbitrario acotado de valores [a, b] . Por ejemplo, cuando se trata de procesamiento de imágenes, los píxeles deben normalizarse para estar entre 0 y 255. Entonces podríamos usar la siguiente fórmula:

![](imagenes/normal_min-max a,b1.png)

Creamos nuestra función
```{r}
norm_minmax <- function(x){
                           (x- min(x)) /(max(x)-min(x))
                          }
normalise_salary <- as.data.frame(lapply(salary, norm_minmax))

head(normalise_salary)
```

Encontramos los estadísticos para los años de salarios
```{r}
normalise_salary %>% summarise(Min = min(YearsExperience,na.rm = TRUE),
                              Q1 = quantile(YearsExperience,probs = .25,na.rm = TRUE),
                              Median = median(YearsExperience, na.rm = TRUE),
                              Q3 = quantile(YearsExperience,probs = .75,na.rm = TRUE),
                              Max = max(YearsExperience,na.rm = TRUE),
                              Mean = mean(YearsExperience, na.rm = TRUE),
                              SD = sd(YearsExperience, na.rm = TRUE),
                              n = n(),
                              Missing = sum(is.na(YearsExperience)))
```

y para los salarios
```{r}
normalise_salary %>% summarise(Min = min(Salary,na.rm = TRUE),
                               Q1 = quantile(Salary,probs = .25,na.rm = TRUE),
                               Median = median(Salary, na.rm = TRUE),
                               Q3 = quantile(Salary,probs = .75,na.rm = TRUE),
                               Max = max(Salary,na.rm = TRUE),
                               Mean = mean(Salary, na.rm = TRUE),
                               SD = sd(Salary, na.rm = TRUE),
                               n = n(),
                               Missing = sum(is.na(Salary)))
```

Como podemos ver en la tabla de estadísticas descriptivas, el valor mínimo de la característica se normaliza a 0, el valor máximo se normaliza a 1 y los valores restantes están entre 0 y 1.


### 4. Normalización media

La normalización media transforma x a x de la misma forma que la normalización mínima-máxima; una cosa diferente es que cada valor de la característica se resta primero por la media de la muestra. 

La fórmula para la normalización media es:

![](imagenes/normal_media1.png)

Creamos nuestra función
```{r}
mean_norm_minmax <- function(x){
                                (x- mean(x)) /(max(x)-min(x))
                               }

mean_normalise_salary <- as.data.frame(lapply(salary, mean_norm_minmax))


head(mean_normalise_salary)
```

Encontramos algunos estadísticos
```{r}
mean_normalise_salary%>% summarise(Min = min(YearsExperience,na.rm = TRUE),
                              Q1 = quantile(YearsExperience,probs = .25,na.rm = TRUE),
                              Median = median(YearsExperience, na.rm = TRUE),
                              Q3 = quantile(YearsExperience,probs = .75,na.rm = TRUE),
                              Max = max(YearsExperience,na.rm = TRUE),
                              Mean = mean(YearsExperience, na.rm = TRUE),
                              SD = sd(YearsExperience, na.rm = TRUE),
                              n = n(),
                              Missing = sum(is.na(YearsExperience)))
```

y para los salarios
```{r}
mean_normalise_salary%>% summarise(Min = min(Salary,na.rm = TRUE),
                               Q1 = quantile(Salary,probs = .25,na.rm = TRUE),
                               Median = median(Salary, na.rm = TRUE),
                               Q3 = quantile(Salary,probs = .75,na.rm = TRUE),
                               Max = max(Salary,na.rm = TRUE),
                               Mean = mean(Salary, na.rm = TRUE),
                               SD = sd(Salary, na.rm = TRUE),
                               n = n(),
                               Missing = sum(is.na(Salary)))
```

### 5. Unidad de longitud

La normalización de la longitud unitaria transforma x a x dividiendo cada valor del vector de características por la longitud euclidiana del vector.

Su fórmula esta dada por:

![](imagenes/normal_euclidea1.png)

Creamos nuestra función
```{r}
unit_length <- function(x) {x / sqrt(sum(x^2))
                            }
                            
unit_length_salary <- as.data.frame(lapply(salary, unit_length))

head(unit_length_salary)
```

La normalización mínima y máxima y la longitud de la unidad están limitadas por valores [0,1]. Esta es una desventaja cuando tenemos valores atípicos en los datos. Si los datos tienen valores atípicos, es mejor utilizar Robust Scalar.

**Diagrama de dispersión**

Ahora trazamos un diagrama de dispersión para ver la distribución de los datos originales, normalizados mínimo-máximo, estandarizados, promedio normalizado y datos de longitud unitaria.
trazando los datos originales:

```{r}
ggplot() + 
geom_point(salary, mapping = aes(x=YearsExperience, y=Salary), color='darkgreen')
```

Gráficamos los datos normalizados para cada uno de los métodos
```{r}
ggplot() + 
geom_point(normalise_salary, mapping = aes(x=YearsExperience, y=Salary, color="Min-Max Normalisation"))+
geom_point(stanardise_salary, mapping = aes(x=YearsExperience, y=Salary, color="Z-score Standardisation")) +
geom_point(robust_scalar_salary, mapping = aes(x=YearsExperience, y=Salary, color="Robust Scalar"))+ 
geom_point(mean_normalise_salary, mapping = aes(x=YearsExperience, y=Salary, color="Mean Normalisation")) + geom_point(unit_length_salary, mapping = aes(x=YearsExperience, y=Salary, color="Unit Length Normalisation")) + 
scale_color_manual(name = "Diferentes técnica de Normalizado",
values = c( "Min-Max Normalisation" = "blue", "Z-score Standardisation" = "red", "Robust Scalar" = "darkgrey", "Mean Normalisation" = "orange", "Unit Length Normalisation" = "black"))
```

Como podemos ver al comparar los datos originales y los datos escalados, el escalado no afectó la distribución o relación entre el salario y los años de experiencia.

**Normalización vs estandarización**

Los métodos de escalado / normalización más utilizados son la normalización y la estandarización mínima-máxima. Veamos la diferencia de cómo se dispersan los datos de normalización y estandarización.
```{r}
ggplot() + 
geom_point(normalise_salary, mapping = aes(x=YearsExperience, y=Salary, color="Min-Max Normalisation"))+
geom_point(stanardise_salary, mapping = aes(x=YearsExperience, y=Salary, color="Z-score Standardisation"))+xlim(-3,3)+ylim(-3,3)+ scale_color_manual(name = "Standardisation vs Normalisation",
values = c( "Min-Max Normalisation" = "darkblue", "Z-score Standardisation" = "red"))
```

Como podemos ver, los datos de normalización están delimitados entre 0 y 1, y la estandarización no tiene límites.

**Diagrama de colores**
También podemos usar los mapas de colores para hacer las visualizaciones

```{r}
library(heatmaply)
par(mfrow = c(1, 2))

heatmaply(salary, 
  xlab = "Salarios",
  ylab = "Años de experiencia", 
  main = "Datos original")

heatmaply(normalise_salary, 
  xlab = "Salarios",
  ylab = "Años de experiencia", 
  main = "Datos normalizados")

```
