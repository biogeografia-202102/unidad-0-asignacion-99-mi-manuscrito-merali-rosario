# '---
# 'title: "Técnicas de ordenación. <br> Parte 1: Ordenación no restringida. <br> PCA, CA y PCoA"
# 'autor: "JR"
# 'fecha: "21 de noviembre de 2020"
# 'salida: github_document
# ' 
# '---

knitr :: opts_chunk $ set (fig.width = 12, fig.height = 8)

# '## Preámbulo
# ' 
# '### Cargar paquetes
# ' 
biblioteca (vegano)
biblioteca (tidyverse)
biblioteca (sf)
biblioteca (mapview)
fuente ('biodata / funciones.R')
# ' 
# '### Cargar datos
# ' 
load ('biodata / Apocynaceae-Meliaceae-Sapotaceae.Rdata')
load ('biodata / matriz_ambiental.Rdata')
mi_fam <- mc_apcyn_melic_saptc
(colnames (mi_fam) <- make.cepnames (colnames (mi_fam)))
(df_equivalencias <- data.frame (
  nombre_original = colnames (mc_apcyn_melic_saptc),
  colnames (mi_fam)))
bci_env_grid%>% tibble
grupos_upgma_k2 <- readRDS ('grupos_upgma_k2.RDS')
mesa (grupos_upgma_k2)
grupos_ward_k3 <- readRDS ('grupos_ward_k3.RDS')
tabla (grupos_ward_k3)
# ' 
# '## Ordenación
# ' 
# 'La ordenación se basa en los mismos principios que la medición de asociación (similaridad) y el agrupamiento: un objeto se caracteriza por sus propiedades en un espacio n-dimensional, donde cada dimensión es una variable, un descriptor. Un simple diagrama de dispersion nos serviría para representar el caso especial de objetos descritos por sólo dos variables, pero no es lo común. Sin embargo, no podremos encontrar patrones consistentes analizando nuestros datos por pares de variables (eg paneles de correlación).
# ' 
# 'A diferencia del análisis de agrupamiento, o como complemento de éste, el análisis de ordenación abarca un conjunto de técnicas que procuran reducir la dimensionalidad de los datos. Se intenta representar, en ejes ortogonales exclusivamente (dos), la complejidad de todo un conjunto. Todas las técnicas de ordenación representan las principales tendencias de variación de los datos en un espacio de dimensiones reducidas, ordenando de manera convencional los ejes con grado descendente de varianza explicada en cada eje sucesivo (eg en n dimensiones, el eje 1 explica la mayor varianza , el eje n explica la mínima).
# ' 
# 'El análisis de ordenación puede ser no restringido (o simple) y restringido (o' canónico '). En el primer caso, las tendencias detectadas en el conjunto de datos de interés, no están restringidas por otro conjunto, por ejemplo, si buscamos tendencias en una matriz de comunidad, y sólo en ella. En el segundo caso, las tendencias detectadas en un conjunto de datos se asocian a otro conjunto, por ejemplo, si buscamos tendencias en una matriz de comunidad pero restringiéndolas a una matriz ambiental. En este script me concentraré en la ordenación no restringida o simple.
# ' 
# 'Las principales técnicas de ordenación no restringida son análisis de componentes principales o PCA (siglas de * análisis de componentes principales *), análisis de correspondencia o CA (* análisis de correspondencia *), análisis de correspondencia múltiple o MCA (* análisis de correspondencia múltiple * ), análisis de coordenadas principales o PCoA (* análisis de coordenadas principales *) y escalamiento multidimensional no métrico o NMDS (* escalado multidimensional no métrico *). Salvo el NMDS, todas estas técnicas se basan en la extracción de los vectores propios (* eigenvectors *) de una matriz de asociación. Explicaré PCA, CA y PCoA a continuación.
# ' 
# '### Análisis de componentes principales (PCA)
# ' 
# 'Es el método tradicional basado en vectores propios que exclusivamente se aplica a datos cuantitativos no procesados, que preserva la distancia euclídea; también se aplica a datos de especies, previa transformación de los datos. Por esta razón, es más común aplicarlo a variables ambientales (matriz ambiental), pero se aplica a datos transformados de composición de composición (matriz de comunidad). Un requisito fundamental para garantizar la eficiencia de este método, es que las variables deben tener algún grado de correlación entre sí, un supuesto a veces imposible de lograr con datos no procesados de matrices de comunidad, pero que sí es bastante común en matrices ambientales. Primero explicaré su uso con un subconjunto de variables ambientales (suelo), para luego aplicarlo a una matriz de comunidad.
# ' 
# '#### PCA aplicado a datos ambientales
# ' 
# 'Para aplicar PCA a datos ambientales, es necesario que todas las variables sean numéricas y "comparables" en cuanto a escalas de medición. Esto se consigue "escalándolas" (convirtiéndolas en puntuaciones z). A partir de la matriz escalada, se generará una matriz de correlaciones.
# ' 
# 'Dado que se requiere que las variables de entrada sean exclusivamente numéricas, el primer paso que realizará será obtener un conjunto de columnas numéricas y, de ellos, seleccionaré sólo las de suelo.
# ' 
# '¡IMPORTANTE! Haré esta demostración sólo con las variables de suelo, ** pero puedes (y debes) ordenar los sitios en función de otras variables, por ejemplo, las geomorfológicas combinadas con el hábitat y la heterogeneidad ambiental **. Por tal razón, haz también el PCA para las demás variables de la matriz ambiental.
# ' 
# 'A partir de los datos de suelo, la función `rda`, de` vegan` ejecutar los siguientes pasos: escalar las variables originales, calcular matriz de correlaciones y obtener vectores propios para el PCA.
# ' 
env_suelo <- bci_env_grid%>%
  st_drop_geometry%>%
  dplyr :: select (coincide con ('^ [AT, Z] | ^ pH $', ignore.case = F))
env_suelo%>% tibble
env_suelo_pca <- rda (env_suelo, scale = TRUE)
env_suelo_pca
resumen (env_suelo_pca)
# ' 
# 'Para agilizar la producción de scripts analíticos de referencia, trasladaré las explicaciones de cada resultado a los vídeos regulares que alojo en el repositorio de la asignatura. En ellos explicaré cómo interpretar éste y otros resultados.
# ' 
# 'En el vídeo asociado, explico el significado de:
# ' 
# '- Inercia, * Inercia *
# '- Valores propios, autovalores, * Autovalores *
# '- Escalamiento, * Escalamiento *
# '- Puntuaciones de "especies", * Puntajes de especies *
# '- Puntuaciones de "sitios", * Puntajes del sitio *
# ' 
screeplot (env_suelo_pca, bstick = TRUE)
# ' 
# 'Usando la función `cleanplot.pca`
# ' 
par (mfrow = c (1, 2))
cleanplot.pca (env_suelo_pca, escala = 1, mar.percent = 0.08, cex.char1 = 1.5)
cleanplot.pca (env_suelo_pca, escala = 2, mar.percent = 0.04, cex.char1 = 1.5)
par (mfrow = c (1, 1))
# ' 
# 'Comparar distribución de los sitios en biplots con distribución real en el mapa:
# ' 
# '### Generar mapa de cuadros sin simbología
# ' 
mapa_cuadros <- mapView (
  bci_env_grid,
  col.regions = 'grey80',
  alpha.regions = 0.3,
  map.types = 'OpenTopoMap',
  leyenda = F, zoom = 14,
  zcol = 'id')%>% addStaticLabels ()%>%
  folleto :: setView (
    lng = -79,85136,
    lat = 9.15097,
    zoom = 15)
mapa_cuadros
# ' 
# 'Comparar con resultados de un análisis de agrupamiento del mismo conjunto de datos. Primero agrupo mis sitios basado en la misma matriz ambiental fuente del PCA (`env_suelo`), escalándola.
# ' 
(env_agrupamiento <- hclust (dist (escala (env_suelo)), 'ward.D'))
(env_grupos <- cutree (env_agrupamiento, k = 3))
(mi_cluster <- factor (env_grupos))
(mi_cluster_l <- niveles (mi_cluster))
(mi_cluster_l_seq <- 1: longitud (mi_cluster_l))
# ' 
# 'Observa que estoy generando un agrupamiento basado en los datos de suelo. No estoy comparando un agrupamiento externo o anterior (por ejemplo, como los creados en los scripts "aa_analisis_de_agrupamiento *"). Sin embargo, dicha comparación es deseable y posible.
# ' 
# 'Luego calculo las puntuaciones de los sitios para usarlas luego como coordenadas de los puntos que añadieron al gráfico:
# ' 
(puntuaciones <- puntuaciones (env_suelo_pca, display = 'wa', scaling = 1))
# '
# 'Luego creo el gráfico base, coloco los puntos sobre el gráfico usando las puntuaciones, les coloco rótulos y, finalmente, coloco leyenda:
# '
grafico_base <- trazar (
  env_suelo_pca,
  display = "wa",
  escala = 1,
  tipo = "n",
  main = "PCA y grupos"
)
abline (v = 0, lty = "punteado")
abline (h = 0, lty = "punteado")
para (yo en mi_cluster_l_seq) {
  puntos (puntuaciones [mi_cluster == i,],
          pch = (14 + i),
          cex = 2,
          col = i + 1)
}
texto (puntuaciones, fila.nombres (env_suelo), cex = 1, pos = 3)
leyenda(
  "topright", # Otras alternativas: "bottomleft", "bottomright" y "topleft"
  pegar ("Grupo", c (mi_cluster_l_seq)),
  pch = 14 + c (mi_cluster_l_seq),
  col = 1 + c (mi_cluster_l_seq),
  pt.cex = 2
)
# ' 
# 'Es razonable que el análisis cluster y el biplot muestren patrones consistentes, puesto que se basan en la misma matriz ambiental.
# ' 
# 'Si hago lo mismo, pero usando mi análisis de agrupamiento anterior (* scripts * "aa_analisis_de_agrupamiento_ *"), no obtengo resultados consistentes, al menos en mi caso.
# ' 
# (mi_cluster_anterior <- grupos_upgma_k2)
(mi_cluster_anterior <- grupos_ward_k3)
(mi_cluster_anterior_l <- niveles (mi_cluster_anterior))
(mi_cluster_anterior_l_seq <- 1: longitud (mi_cluster_anterior_l))
grafico_base <- trazar (
  env_suelo_pca,
  display = "wa",
  escala = 1,
  tipo = "n",
  main = "PCA y grupos"
)
abline (v = 0, lty = "punteado")
abline (h = 0, lty = "punteado")
para (yo en mi_cluster_anterior_l_seq) {
  puntos (puntuaciones [mi_cluster_anterior == i,],
          pch = (14 + i),
          cex = 2,
          col = i + 1)
}
texto (puntuaciones, fila.nombres (env_suelo), cex = 1, pos = 3)
leyenda(
  "topright", # Otras alternativas: "bottomleft", "bottomright" y "topleft"
  pegar ("Grupo", c (mi_cluster_anterior_l_seq)),
  pch = 14 + c (mi_cluster_anterior_l_seq),
  col = 1 + c (mi_cluster_anterior_l_seq),
  pt.cex = 2
)
# ' 
# 'Esto podría significar que las tendencias / patrones de mi matriz de comunidad (cuadros de 1 Ha de BCI según composición), no se asocian / no son consistentes con variables de suelo según el PCA. Es probable que, usando una combinación diferente de variables ambientales, se puedan extraer patrones. No recomiendo identificar variables ambientales de forma meramente heurística, porque sería equivalente a pescar; recomiendo construir una matriz ambiental de variables seleccionadas a partir de patrones de dependencia identificados en scripts anteriores. Concretamente, en el script [aa_analisis_de_agrupamiento_3_variables_ambientales_segun_grupos.R] (aa_analisis_de_agrupamiento_3_variables_ambientales_segun_grupos.R) identifiqué posibles variables asociadas según los distintos agrupamientos realizados. Si fueras tu caso,
# ' 
# '#### PCA aplicado a datos de comunidad transformados
# ' 
mi_fam_hel <- decostand (mi_fam, método = 'hellinger')
mi_fam_hel%>% tibble
mi_fam_hel_pca <- rda (mi_fam_hel)
resumen (mi_fam_hel_pca)
gráfico de sedimentación(
  mi_fam_hel_pca,
  bstick = VERDADERO,
  npcs = longitud (mi_fam_hel_pca $ CA $ eig)
)
mi_fam_hel_pca_sc1 <- puntuaciones (mi_fam_hel_pca,
                                    display = "especie", escala = 1)
mi_fam_hel_pca_sc2 <- puntuaciones (mi_fam_hel_pca,
                                    display = "especie", escala = 2)
par (mfrow = c (1, 2))
cleanplot.pca (mi_fam_hel_pca, escala = 1, mar.percent = 0.06, cex.char1 = 0.7)
cleanplot.pca (mi_fam_hel_pca, escala = 2, mar.percent = 0.06, cex.char1 = 0.7)
par (mfrow = c (1, 1))
# ' 
# 'Si intentáramos realizar el PCA a datos de comunidad no transformados, no recogeríamos apropiadamente las tendencias y patrones, debido a la presencia de doble-ceros y valores extremos.
# ' 
# 'Las especies que necesitan mucho a los ejes 1 y 2 del PCA (aquellas cuyos vectores sobresalen el círculo de contribución uniforme), podrían coincidir con las que podrían ser consideradas como especies indicadoras o que preferirían por hábitats determinados.
# ' 
# 'Evaluaré el ajuste del PCA de datos de comunidad a datos ambientales, mediante la función `envfit`
# ' 
biplot
mi_fam_hel_pca,
main = "PCA, escalamiento 2, ajuste a variables ambientales")
(mi_fam_hel_pca_envfit <- envfit (mi_fam_hel_pca, env_suelo, escala = 2))
gráfico (mi_fam_hel_pca_envfit, p.max = 0.05, col = 3)
# ' 
# 'Comento los resultados en el vídeo asociado. También probaré el ajuste con todas las numéricas de la matriz, excluyendo por supuesto la columna `id`:
# ' 
# 'NOTA: te recomiendo probar otros métodos de selección de variables, como por ejemplo, usando la función `paso` para seleccionar fórmulas de modelos basadas en AIC.
# ' 
env_num <- bci_env_grid%>%
  select_if (es.numeric)%>%
  seleccione (-id)%>%
  st_drop_geometry
(mi_fam_hel_pca_envfit_num <- envfit (mi_fam_hel_pca, env_num, escala = 2))
biplot
mi_fam_hel_pca,
main = "PCA, escalamiento 2, ajuste a variables ambientales")
plot (mi_fam_hel_pca_envfit_num, p.max = 0.05, col = 3)
biplot
mi_fam_hel_pca,
main = "PCA, escalamiento 2, ajuste a variables ambientales")
plot (mi_fam_hel_pca_envfit_num, p.max = 0.1, col = 3)
# ' 
# 'Comento los resultados en el vídeo asociado. 
# ' 
# '¿Cuándo oa qué datos aplicar PCA?
# ' 
# '- PCA no es especialmente sensible a datos muy desviados de la normalidad.
# '- Como toda técnica, PCA tiene limitaciones.
# '- Las variables deben ser dimensionalmente homogéneas (unidades comparables o adimensionales).
# '- No usar en matriz transpuestas (no hace sentido la covarianza entre objetos).
# '- Es posible usar PCA con dato de presencia / ausencia, en cuyo caso, la matriz de comunidad debe transformarse a Hellinger, cuerdas o log-chord.
# '- Las relaciones entre variables se miden por ángulos, no por proximidad de las puntas de los vectores.
# ' 
# '### Análisis de correspondencia (CA)
# ' 
mi_fam_ca <- cca (mi_fam)
resumen (mi_fam_ca)
resumen (mi_fam_ca, escala = 1)
# '
#' Gráfico de sedimentación
# ' 
screeplot (mi_fam_ca, bstick = TRUE, npcs = longitud (mi_fam_ca $ CA $ eig))
# '
# 'Biplots
# ' 
par (mfrow = c (1, 2))
parcela (mi_fam_ca,
         escala = 1,
         main = "Análisis de correspondencia, escalamiento 1"
)
parcela (mi_fam_ca,
         scaling = 2, # Por defecto scaling = 2, lo escribo sólo para fines didáticos
         main = "Análisis de correspondencia, escalamiento 2")
par (mfrow = c (1, 1))
# ' 
# 'Excluyendo especie * Thevetia ahouai *, abreviada como * Thevahou *.
# ' 
mi_fam_ca <- cca (mi_fam [, -grep ('Thevahou', colnames (mi_fam))])
resumen (mi_fam_ca)
resumen (mi_fam_ca, escala = 1)
screeplot (mi_fam_ca, bstick = TRUE, npcs = longitud (mi_fam_ca $ CA $ eig))
par (mfrow = c (1, 2))
parcela (mi_fam_ca,
         escala = 1,
         main = "CA, escalamiento 1, sin Thevetia ahouai"
)
parcela (mi_fam_ca,
         escala = 2,
         main = "CA, escalamiento 2, sin Thevetia ahouai")
par (mfrow = c (1, 1))
# ' 
# 'Análisis de coordenadas principales (PCoA)
# ' 
# 'Las técnicas de ordenación anteriores preservan la distancia euclídea entre los objetos. Si necesitaras ordenar objetos usando una métrica, por ejemplo, la de Gower para datos mixtos, entonces PCA y CA diferente inútiles y, en su lugar, podrías usar PCoA. Paso el resto de la explicación al vídeo asociado.
# ' 
# 'La función que realiza el PCoA en `{vegan}` es `cmdscale` (de * Classical (Metric) Multidimensional Scaling *), y se le suministra una matriz de distancias.
# ' 
mi_fam_d_bray <- vegdist (mi_fam, method = 'bray') # En realidad, 'bray' es la opción por defecto
mi_fam_d_bray_pcoa <- cmdscale (
  mi_fam_d_bray,
  k = (nrow (mi_fam) - 1),
  agregar = T,
  eig = VERDADERO)
round (mi_fam_d_bray_pcoa $ eig, 2)
round (suma (mi_fam_d_bray_pcoa $ eig [mi_fam_d_bray_pcoa $ eig <0]), 2)
round (suma (mi_fam_d_bray_pcoa $ eig [mi_fam_d_bray_pcoa $ eig> = 0]), 2)
ordiplot (puntuaciones (mi_fam_d_bray_pcoa, elecciones = c (1, 2)),
          tipo = "t",
          main = "PCoA con promedios ponderados de especies")
abline (h = 0, lty = 3)
abline (v = 0, lty = 3)
mi_fam_d_bray_pcoa_wa <- wascores (mi_fam_d_bray_pcoa $ puntos [, 1: 2], mi_fam)
texto(
  mi_fam_d_bray_pcoa_wa,
  nombres de fila (mi_fam_d_bray_pcoa_wa),
  cex = 0,7, col = "rojo")
(mi_fam_d_bray_pcoa_env <- envfit (mi_fam_d_bray_pcoa, env_num))
plot (mi_fam_d_bray_pcoa_env, p.max = 0.05, col = 3)