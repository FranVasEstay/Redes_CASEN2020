# Redes_CASEN2020
Scripts para investigación de redes en el contexto de la Encuesta CASEN 2020.

## Descripción

Este repositorio contiene scripts que permiten realizar análisis de redes basados en los datos de la Encuesta CASEN 2020. Los scripts están organizados en un orden lógico para facilitar su uso y comprensión.

## Scripts

1. **`Integracion_nuevas_variables_fv.R`**: 
   - Contiene la integración de nuevas variables y el cálculo de medidas para esas variables. Este script es el primer paso en el análisis, asegurando que todas las variables relevantes estén disponibles y correctamente formateadas.

2. **`Networks_sapply_fv.R`**: 
   - Contiene el antiguo script "Networks_sapply.qmd" con algunas modificaciones para optimizar.

3. **`Networks_samplefv.R`**: 
   - Contiene la creación de las redes para 1000 hogares aleatorios.

4. **`Networks.R`**:
   - Contiene la creación d elas redes para todas las viviendas. 

## Requisitos

- R 
- Paquetes de R: 
  - `dplyr`
  - `tidyverse`
  - `igraph`
  - `tidyr`
  - Otros paquetes necesarios

## Instalación

Para ejecutar los scripts, asegúrate de tener R y los paquetes necesarios instalados. Puedes instalar los paquetes requeridos con el siguiente comando:

```r
install.packages(c("dplyr", "tidyverse", "igraph", "tidyr"...))
