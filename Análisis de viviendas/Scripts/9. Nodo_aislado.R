################################################################################
###################### Social Network: encuesta CASEN ##########################
###################### ANÁLISIS DE NODOS AISLADOS ##############################
###################Y SU RELACIÓN CON EL JEFE DE HOGAR###########################
######################Por tipología y por macrogrupo############################
 
library(igraph)
library(dplyr)
library(tidyr)
library(furrr)
library(ggplot2)
library(forcats)
library(doParallel)
library(parallel)
library(ggraph)
# ------------------------------------------------------------------------------
# 1. CARGAR DATOS
# ------------------------------------------------------------------------------

# Redes con atributos
load("Análisis de viviendas/Redes/kinship_igrpah.RData")
# Cargar los datos originales
load("Análisis de viviendas/Data/Data.RData")

# Crear vector de mapeo numérico -> etiqueta
mapeo_pco1 <- levels(data$pco1)         # las etiquetas en orden 1..15
names(mapeo_pco1) <- seq_along(mapeo_pco1)   # asignar nombres 1,2,...,15

# Mapeo de household -> tipología hasta T48
households_por_tipologia <- readRDS(
  "Análisis de viviendas/Analisis/Resultados_Tipologias/Reportes/households_por_tipologia.rds"
)

# Clasificación de tipologías en macrogrupos (T1-T48)
macrogrupos <- read.csv(
  "Análisis de viviendas/Analisis/Resultados_tipologias/clasificacion_tipo.csv",
  stringsAsFactors = FALSE
) %>% filter(Tipologia %in% paste0("T", 1:48))

tipologias_interes <- paste0("T", 1:48)

# ------------------------------------------------------------------------------
# 2. PREPARAR MAPEO household
# ------------------------------------------------------------------------------

# Convertir household_i a character para evitar problemas de tipo
ids_redes <- data.frame(
  indice = seq_along(kinship_igrpah),
  household_i = sapply(kinship_igrpah, function(x) as.character(x$household_i)),
  stringsAsFactors = FALSE
) %>% mutate(household_i = trimws(household_i))   # eliminar espacios

# Expandir households_por_tipologia a formato largo
tipologia_larga <- households_por_tipologia %>%
  select(Tipologia, Households) %>%
  mutate(Households = strsplit(as.character(Households), ";")) %>%
  unnest(Households) %>%
  mutate(household = as.character(Households)) %>%
  select(Tipologia, household)

# Unir con los índices de red
hogares_con_indice <- tipologia_larga %>%
  inner_join(ids_redes, by = c("household" = "household_i"))

# Añadir macrogrupo
hogares_con_indice <- hogares_con_indice %>%
  left_join(macrogrupos, by = c("Tipologia" = "Tipologia"))

# ------------------------------------------------------------------------------
# 3. FUNCIÓN PARA PROCESAR UN HOGAR Y EXTRAER NODOS AISLADOS
# ------------------------------------------------------------------------------
extraer_aislados <- function(indice_red, tipologia, macrogrupo) {
  g <- kinship_igrpah[[indice_red]]$kinship_net
  if (!inherits(g, "igraph") || vcount(g) == 0) return(NULL)
  
  pco1_raw <- vertex_attr(g, "pco1")
  if (is.null(pco1_raw)) {
    warning(paste("Hogar", kinship_igrpah[[indice_red]]$household_i, "sin atributo pco1"))
    return(NULL)
  }
  
  # Traducir a etiquetas si es numérico
  if (is.numeric(pco1_raw)) {
    pco1_etiqueta <- mapeo_pco1[as.character(pco1_raw)]
    jefe_idx <- which(pco1_raw == 1)   # El jefe siempre es el código 1
  } else {
    # Si ya es texto, buscar la palabra "jefe"
    pco1_etiqueta <- as.character(pco1_raw)
    jefe_idx <- which(grepl("jefe", tolower(iconv(pco1_etiqueta, to = "ASCII//TRANSLIT"))))
  }
  
  if (length(jefe_idx) != 1) {
    warning(paste("Hogar", kinship_igrpah[[indice_red]]$household_i, 
                  "sin jefe único, encontrados:", length(jefe_idx)))
    return(NULL)
  }
  
  grados <- degree(g, mode = "total")
  aislados_idx <- which(grados == 0 & seq_along(grados) != jefe_idx)
  if (length(aislados_idx) == 0) return(NULL)
  
  # Relación de los aislados (como cadena de texto)
  relaciones <- pco1_etiqueta[aislados_idx]
  
  tibble(
    tipologia = tipologia,
    relacion = relaciones
  )
}


# ------------------------------------------------------------------------------
# 4. APLICAR LA FUNCIÓN A TODOS LOS HOGARES
# ------------------------------------------------------------------------------
plan(multisession, workers = availableCores() - 1)

start.time <- Sys.time()

resultados <- hogares_con_indice %>%
  filter(Tipologia %in% tipologias_interes) %>%
  mutate(result = future_pmap(list(indice, Tipologia, macrogrupo), extraer_aislados)) %>%
  unnest(result, keep_empty = FALSE)

end.time <- Sys.time()

cat("Tiempo de ejecución (paralelo):", difftime(end.time, start.time), "segundos\n")
cat("Número de observaciones (nodo aislado - relación):", nrow(resultados), "\n")

plan(sequential)

 # Ver los primeros valores de ambos lados
head(ids_redes$household_i)
head(tipologia_larga$household)

# Cuántos hogares de las tipologías aparecen en las redes
nrow(hogares_con_indice)  # debe ser > 0

# ------------------------------------------------------------------------------
# 5. TABLAS DE FRECUENCIAS Y PORCENTAJES POR TIPOLOGÍA
# ------------------------------------------------------------------------------
# Vector of translation (all 15 original levels, if any)
traduccion_relacion <- c(
  "Jefe(a) de Hogar"                    = "Household head",
  "Esposo(a) o pareja de distinto sexo" = "Spouse/partner (opposite sex)",
  "Esposo(a) o pareja de igual sexo"    = "Spouse/partner (same sex)",
  "Hijo(a) de ambos"                    = "Child of both",
  "Hijo(a) sólo del jefe(a)"            = "Child of head only",
  "Hijo(a) sólo del esposo(a)/pareja"   = "Child of spouse/partner only",
  "Padre o madre"                       = "Parent",
  "Suegro(a)"                           = "Father/mother-in-law",
  "Yerno o nuera"                       = "Son/daughter-in-law",
  "Nieto(a)"                            = "Grandchild",
  "Hermano(a)"                          = "Brother/sister",
  "Cuñado(a)"                           = "Brother/sister-in-law",
  "Otro Familiar"                       = "Other relative",
  "No familiar"                         = "Not related",
  "Servicio Doméstico puertas adentro"  = "Live-in domestic servant"
)

# Recode the 'relacion' column in resultados
resultados <- resultados %>%
  mutate(relacion = recode(relacion, !!!traduccion_relacion))

# Frecuencias absolutas
tabla_tipologia_abs <- resultados %>%
  group_by(tipologia, relacion) %>%
  summarise(n = n(), .groups = "drop")

# Porcentajes dentro de cada tipología
tabla_tipologia_pct <- tabla_tipologia_abs %>%
  group_by(tipologia) %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  ungroup() %>%
  arrange(tipologia, desc(porcentaje))

write.csv(tabla_tipologia_pct,
          "Análisis de viviendas/Analisis/Relaciones_aislados_por_tipologia.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

# ------------------------------------------------------------------------------
# 6. TABLAS POR MACROGRUPO
# ------------------------------------------------------------------------------

# Reemplazar NAs en macrogrupo
resultados <- resultados %>%
  mutate(macrogrupo = ifelse(is.na(macrogrupo), "Sin clasificar", macrogrupo))

tabla_macro_abs <- resultados %>%
  group_by(macrogrupo, relacion) %>%
  summarise(n = n(), .groups = "drop")

tabla_macro_pct <- tabla_macro_abs %>%
  group_by(macrogrupo) %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  ungroup() %>%
  arrange(macrogrupo, desc(porcentaje))

write.csv(tabla_macro_pct,
          "Análisis de viviendas/Analisis/Relaciones_aislados_por_macrogrupo.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

# ------------------------------------------------------------------------------
# 7. GRÁFICOS DE BARRAS
# ------------------------------------------------------------------------------

# Macrogrupos (excluyendo "Sin clasificar")
datos_plot <- tabla_macro_pct %>%
  filter(macrogrupo != "Sin clasificar") %>%
  mutate(relacion = fct_reorder(relacion, porcentaje, .desc = FALSE))

ggplot(datos_plot, aes(x = porcentaje, y = relacion, fill = macrogrupo)) +
  geom_col(position = "dodge") +
  facet_wrap(~macrogrupo, scales = "free_y") +
  labs(title = "Relación con el jefe de hogar de los nodos aislados",
       x = "Porcentaje dentro del macrogrupo (%)", y = "Relación (pco1)") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("Análisis de viviendas/Analisis/grafico_aislados_macrogrupos.png",
       width = 10, height = 6)

# ==============================================================================
# GRÁFICOS ESPECÍFICOS POR TIPOLOGÍA
# ==============================================================================
#  CONFIGURACIÓN PARA LOS GRÁFICOS DE RED

primer_g <- kinship_igrpah[[1]]$kinship_net
edge_attrs <- edge_attr_names(primer_g)
EDGE_ATTR_NAME <- NULL
for (attr in edge_attrs) {
  if (grepl("kin|relation|type", attr, ignore.case = TRUE)) {
    EDGE_ATTR_NAME <- attr
    break
  }
}
if (!is.null(EDGE_ATTR_NAME)) {
  message("Usando atributo de arista: ", EDGE_ATTR_NAME)
} else {
  message("No se encontró atributo de parentesco en las aristas. Todas las aristas serán grises.")
}

# Mapeo de colores para tipos de arista (ampliable)
edge_colour_map <- c(
  "marriage"             = "#2ca02c",   # green
  "Marriage"             = "#2ca02c",
  "spouse"               = "#2ca02c",
  "Spouse"               = "#2ca02c",
  "couple"               = "#2ca02c",
  "parent-child"         = "#ff7f0e",   # orange
  "parent_child"         = "#ff7f0e",
  "child"                = "#ff7f0e",
  "Child"                = "#ff7f0e",
  "son/daughter"         = "#ff7f0e",
  "parent"               = "#ff7f0e"
)

# Función para obtener el color de una arista dada su categoría
edge_color_mapper <- function(val) {
  key <- tolower(as.character(val))
  if (key %in% names(edge_colour_map)) {
    edge_colour_map[key]
  } else {
    "grey60"
  }
}

# FUNCIONES AUXILIARES PARA LOS GRÁFICOS COMBINADOS

# Obtener el índice del primer hogar de una tipología
get_hogar_indice <- function(tipologia, datos_hogares = hogares_con_indice) {
  idx <- which(datos_hogares$Tipologia == tipologia)
  if (length(idx) == 0) return(NA_integer_)
  datos_hogares$indice[idx[1]]
}

# Construir un panel (red + barras) para una tipología
build_panel <- function(tipologia, datos_resultados, datos_hogares, red_list) {
  idx <- get_hogar_indice(tipologia, datos_hogares)
  if (is.na(idx)) return(NULL)
  
  g <- red_list[[idx]]$kinship_net
  if (is.null(g) || vcount(g) == 0) return(NULL)
  g <- upgrade_graph(g)
  
  # Nodo jefe
  V(g)$es_jefe <- ifelse(V(g)$pco1 == 1, "Head", "Other")
  
  # Grafo de red con ggraph
  net_plot <- ggraph(g, layout = "fr") +
    theme_void() +
    ggtitle(tipologia)
  
  # Aristas con color según parentesco, con flecha
  if (!is.null(EDGE_ATTR_NAME) && EDGE_ATTR_NAME %in% edge_attr_names(g)) {
    # Asignar color en función del atributo
    edge_colors <- sapply(edge_attr(g, EDGE_ATTR_NAME), edge_color_mapper)
    net_plot <- net_plot +
      geom_edge_link(aes(color = factor(edge_colors)),   # color directo, sin leyenda
                     arrow = arrow(type = "closed", length = unit(0.1, "inches")),
                     end_cap = circle(2, 'mm')) +
      scale_edge_color_identity(guide = "none")
  } else {
    net_plot <- net_plot +
      geom_edge_link(color = "grey60",
                     arrow = arrow(type = "closed", length = unit(0.1, "inches")),
                     end_cap = circle(2, 'mm'))
  }
  
  # Nodos (rojo para jefe, gris para otros)
  net_plot <- net_plot +
    geom_node_point(aes(color = es_jefe), size = 3) +
    scale_color_manual(values = c("Head" = "#d62728", "Other" = "grey60"),
                       guide = "none")
  
  # Datos de aislados para esa tipología
  datos_bar <- datos_resultados %>%
    filter(tipologia == !!tipologia) %>%
    count(relacion) %>%
    mutate(porcentaje = n / sum(n) * 100) %>%
    arrange(desc(porcentaje))
  
  if (nrow(datos_bar) == 0) {
    bar_plot <- ggplot() +
      annotate("text", x = 0, y = 0, label = "No isolates") +
      theme_void()
  } else {
    bar_plot <- ggplot(datos_bar, aes(x = porcentaje, y = reorder(relacion, porcentaje))) +
      geom_col(fill = "#1f77b4") +
      labs(x = "% within typology", y = NULL) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8))
  }
  
  net_plot + bar_plot + plot_layout(widths = c(1, 1))
}

# Leyenda unificada (nodo rojo + aristas de colores)
make_unified_legend <- function() {
  # Leyenda del nodo (jefe de hogar)
  node_legend <- ggplot(data.frame(x = 0, y = 0, Node = "Most likely household head")) +
    geom_point(aes(x, y, color = Node), size = 3) +
    scale_color_manual(name = NULL,
                       values = c("Most likely household head" = "#d62728")) +
    theme_void() +
    theme(legend.position = "bottom", legend.direction = "horizontal")
  
  # Leyenda de las aristas (si hay atributo)
  if (!is.null(EDGE_ATTR_NAME)) {
    edge_legend <- ggplot() +
      annotate("segment", x = 0, xend = 1, y = 0, yend = 0,
               arrow = arrow(type = "closed", length = unit(0.1, "inches")),
               color = "#2ca02c", linewidth = 1) +
      annotate("text", x = 1.5, y = 0, label = "Marriage", hjust = 0) +
      annotate("segment", x = 0, xend = 1, y = 1, yend = 1,
               arrow = arrow(type = "closed", length = unit(0.1, "inches")),
               color = "#ff7f0e", linewidth = 1) +
      annotate("text", x = 1.5, y = 1, label = "Parent-child", hjust = 0) +
      xlim(0, 3) + ylim(-0.5, 1.5) +
      theme_void()
    
    # Extraer las leyendas como objetos gráficos
    leg_node <- get_legend(node_legend)
    leg_edge <- get_legend(edge_legend)
    # Combinarlas horizontalmente
    combined_legend <- plot_grid(leg_node, NULL, leg_edge, nrow = 1,
                                 rel_widths = c(0.5, 0.1, 0.5))
  } else {
    leg_node <- get_legend(node_legend)
    combined_legend <- plot_grid(leg_node)
  }
  
  combined_legend
}

# Función principal para generar la figura de un macrogrupo (solo tipologías con aislados)
plot_macrogrupo_solo_aislados <- function(macro, ncol = 1) {
  tips <- intersect(
    unique(hogares_con_indice$Tipologia[hogares_con_indice$macrogrupo == macro]),
    unique(resultados$tipologia[resultados$macrogrupo == macro])
  )
  tips <- tips[!is.na(tips)]
  
  if (length(tips) == 0) {
    return(ggplot() +
             annotate("text", x = 0, y = 0,
                      label = paste("No isolated nodes in macrogroup:", macro)) +
             theme_void())
  }
  
  panels <- lapply(tips, function(tip) {
    build_panel(tip, resultados, hogares_con_indice, kinship_igrpah)
  })
  panels <- compact(panels)
  
  if (length(panels) == 0) return(NULL)
  
  combined_panels <- wrap_plots(panels, ncol = ncol) +
    plot_annotation(title = paste("Macrogroup:", macro))
  
  # Añadir leyenda unificada debajo
  legend_grob <- make_unified_legend()
  
  final_plot <- wrap_plots(
    combined_panels,
    legend_grob,
    ncol = 1,
    heights = c(1, 0.15)
  )
  return(final_plot)
}

# GENERAR Y GUARDAR LAS FIGURAS

# Para "Isolated"
fig_isolated <- plot_macrogrupo_solo_aislados("Isolated", ncol = 1)
n_tips_isol <- length(intersect(
  unique(hogares_con_indice$Tipologia[hogares_con_indice$macrogrupo == "Isolated"]),
  unique(resultados$tipologia[resultados$macrogrupo == "Isolated"])
))
height_isol <- max(7, 3.5 * n_tips_isol + 1)   # mínimo 7 pulgadas
ggsave("AnálisisFigura_Isolated_aislados.png", fig_isolated,
       width = 14, height = height_isol, limitsize = FALSE)

# Para "Extended"
fig_extended <- plot_macrogrupo_solo_aislados("Extended", ncol = 1)
n_tips_ext <- length(intersect(
  unique(hogares_con_indice$Tipologia[hogares_con_indice$macrogrupo == "Extended"]),
  unique(resultados$tipologia[resultados$macrogrupo == "Extended"])
))
height_ext <- max(7, 3.5 * n_tips_ext + 1)
ggsave("Figura_Extended_aislados.png", fig_extended,
       width = 14, height = height_ext, limitsize = FALSE)
