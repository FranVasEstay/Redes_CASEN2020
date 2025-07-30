
# Networks & Covariates files
load("Ergomitos/Redes/dependency_network.RData")
#class(dependency_network)
load("Ergomitos/Redes/descent_network.RData")
load("Ergomitos/Redes/marriage_network.RData")

###### filter by network size #########

lists_dependency <- list()
lists_descent <- list()
lists_marriage <- list()

for (i in seq_along(dependency_network)) {
  dependency <- dependency_network[[i]][['dependency_net']]
  descent <- descent_network[[i]][["descent_net"]]
  marriage <- marriage_network[[i]][["marriage_net"]]
  
  if (length(dependency[["val"]]) >= 0 &&
      length(dependency[["mel"]]) >= 0 &&
      length(descent[["val"]]) >= 0 &&
      length(marriage[["val"]]) >= 0) {
    
    lists_dependency[[length(lists_dependency) + 1]] <- dependency
    lists_descent[[length(lists_descent) + 1]] <- descent
    lists_marriage[[length(lists_marriage) + 1]] <- marriage
  }
}
length(lists_dependency)
length(lists_descent)
length(lists_marriage)

library(ergm.multi)
library(ergm)
library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(knitr)

G <- lists_dependency
descent <- lists_descent 
marriage <- lists_marriage

##################### COVARIATES #################################################

library(purrr)
library(dplyr)
library(tidyr)
library(janitor)
# G = lists_dependency

## -------- 0. Which numeric covariates?  ------------------
# Fast & safe: keep only attrs that exist AND are numeric in most nets
attr_freq <- table(unlist(lapply(G, list.vertex.attributes)))
# choose those present in, say, >= 0.9 of nets (tune threshold)
attrs_common <- names(attr_freq)[attr_freq >= 0.9 * length(G)]

is_num_attr <- function(g, a) {
  a %in% list.vertex.attributes(g) &&
    is.numeric(get.vertex.attribute(g, a))
}

covars_num <- attrs_common[ vapply(attrs_common,
                                   function(a) is_num_attr(G[[1]], a),
                                   logical(1)) ]
# or just set manually:
# covars_num <- c("edad","autoeficacia","autoestima")

## -------- 1. Basic per-network vectors -------------------
 #sizes  <- vapply(G, network.size, integer(1))
 #edges  <- vapply(G, network.edgecount, integer(1))
 #direct <- vapply(G, is.directed, logical(1))
# Basic vectors
sizes  <- vapply(G, network.size,  numeric(1))   # or purrr::map_dbl(G, network.size)
edges  <- vapply(G, network.edgecount, numeric(1))
direct <- vapply(G, is.directed,    logical(1))
denoms <- ifelse(direct, sizes * (sizes - 1), choose(sizes, 2))
density <- edges / denoms

net_basic <- tibble(net_id = seq_along(G), size = sizes,
                    edges = edges, density = density)

## -------- 2. Vertex covariates long table ----------------
# We'll build a long DF with columns: size, attr, value
get_attr_values <- function(g, attrs){
  # returns a tibble size x attr x value
  sz <- network.size(g)
  map_dfr(attrs, function(a){
    if(a %in% list.vertex.attributes(g)){
      tibble(attr = a, value = get.vertex.attribute(g, a))
    } else {
      tibble(attr = a, value = rep(NA_real_, sz))
    }
  }) %>% mutate(size = sz, .before = 1)
}

cov_long <- map_dfr(G, get_attr_values, attrs = covars_num)

## -------- 3. Summaries by size ---------------------------
# Edges/density
by_size_basic <- net_basic %>%
  group_by(size) %>%
  summarise(
    n_nets      = n(),
    total_actors= sum(size),              # optional
    mean_edges  = mean(edges),
    sd_edges    = sd(edges),
    mean_density= mean(density),
    .groups = "drop"
  )

# Covariates (actor-weighted: all actors in all nets of a given size)
by_size_cov <- cov_long %>%
  group_by(size, attr) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(mean, sd),
               names_to = "stat", values_to = "val") %>%
  mutate(col = paste0(attr, "_", stat)) %>%
  select(size, col, val) %>%
  pivot_wider(names_from = col, values_from = val)

## -------- 4. Final table ---------------------------------
summary_by_size <- by_size_basic %>%
  left_join(by_size_cov, by = "size") %>%
  arrange(size) %>%
  clean_names()

table<-knitr::kable(summary_by_size, digits = 2,
             caption = "Dependency networks aggregated by size (|V|)")
print(table)

#install.packages("kableExtra")
library(kableExtra)
tab <- summary_by_size %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  kbl(format = "html", caption = "Dependency networks aggregated by size (|V|)") %>%
  kable_styling()

print(tab)   # se abre en el Viewer de RStudio



##################### NETWORKS #################################################

############################################################
# 0. Paquetes
############################################################
pkgs <- c("statnet", "sna", "purrr", "dplyr", "tibble", "tidyr", "kableExtra")
invisible(lapply(pkgs, function(p) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)))
lapply(pkgs, library, character.only = TRUE)

############################################################
# 1. Helper para coerción a matriz
############################################################
safe_as_matrix <- function(x){
  if (inherits(x, "network")) {
    return(network::as.matrix.network(x, matrix.type = "adjacency"))
  }
  if (is.matrix(x)) return(x)
  if (is.data.frame(x)) return(as.matrix(x))
  if (is.list(x) && length(x) == 1 && (is.matrix(x[[1]]) || is.data.frame(x[[1]]))) {
    return(as.matrix(x[[1]]))
  }
  stop("Object cannot be coerced to a matrix.")
}

############################################################
# 2. Métricas auxiliares
############################################################
safe_trans_weak <- function(A){
  tr <- sna::gtrans(A, measure = "weak")
  if (is.nan(tr)) return(NA_real_) else tr
}

safe_recip <- function(A){
  rec <- sna::grecip(A, measure = "dyadic")
  if (is.nan(rec)) return(NA_real_) else rec
}

############################################################
# 3. Funciones de resumen
############################################################
summ_dep <- function(net){
  A <- safe_as_matrix(net)
  A[is.na(A)] <- 0
  diag(A) <- 0
  
  n      <- nrow(A)
  edges  <- sum(A)
  dens   <- ifelse(n <= 1, NA_real_, edges/(n*(n-1)))
  outdeg <- rowSums(A)
  indeg  <- colSums(A)
  
  tibble(
    n           = n,
    density     = dens,
    reciprocity = safe_recip(A),
    trans_weak  = safe_trans_weak(A),
    mean_outdeg = mean(outdeg),
    sd_outdeg   = sd(outdeg),
    mean_indeg  = mean(indeg),
    sd_indeg    = sd(indeg)
  )
}

summ_mat <- function(M){
  A <- safe_as_matrix(M)
  A[is.na(A)] <- 0
  diag(A) <- 0
  
  n      <- nrow(A)
  edges  <- sum(A)
  dens   <- ifelse(n <= 1, NA_real_, edges/(n*(n-1)))
  outdeg <- rowSums(A)
  indeg  <- colSums(A)  # si es no dirigido, será igual a outdeg
  
  tibble(
    n           = n,
    density     = dens,
    reciprocity = NA_real_,   # solo para dependency
    trans_weak  = NA_real_,   # solo para dependency
    mean_outdeg = mean(outdeg),
    sd_outdeg   = sd(outdeg),
    mean_indeg  = mean(indeg),
    sd_indeg    = sd(indeg)
  )
}

############################################################
# 4. Wrapper para capturar errores
############################################################
summarise_wrap <- function(obj, id, fun, type){
  tryCatch(
    fun(obj) %>% mutate(net_id = id, type = type),
    error = function(e) tibble(net_id = id, type = type, error = conditionMessage(e))
  )
}

############################################################
# 5. ---- TUS LISTAS ----------------------------------------------------------
# Reemplaza estas listas por las tuyas reales
# G        : lista de objetos 'network' (dependency)
# descent  : lista de matrices/listas (affiliation)
# marriage : lista de matrices/listas (marriage)

# G <- ...
# descent <- ...
# marriage <- ...

############################################################
# 6. Mapear y unir
############################################################
dep_df <- purrr::imap_dfr(G,       ~ summarise_wrap(.x, .y, summ_dep,  "dependency"))
aff_df <- purrr::imap_dfr(descent, ~ summarise_wrap(.x, .y, summ_mat,  "affiliation"))
mar_df <- purrr::imap_dfr(marriage,~ summarise_wrap(.x, .y, summ_mat,  "marriage"))

all_df <- dplyr::bind_rows(dep_df, aff_df, mar_df)

# Separar errores (opcional)
err_df   <- all_df %>% dplyr::filter(!is.na(error))
clean_df <- all_df %>% dplyr::filter(is.na(error)) %>% dplyr::select(-error)

############################################################
# 7. Resumen por tamaño exacto (|V|) y tipo, desde 2 en adelante
############################################################
summ_by_size_type <- clean_df %>%
  dplyr::filter(n >= 2) %>%
  dplyr::group_by(n, type) %>%
  dplyr::summarise(
    density_mean     = mean(density,     na.rm = TRUE),
    density_sd       = sd(density,       na.rm = TRUE),
    reciprocity_mean = mean(reciprocity, na.rm = TRUE),
    reciprocity_sd   = sd(reciprocity,   na.rm = TRUE),
    trans_weak_mean  = mean(trans_weak,  na.rm = TRUE),
    trans_weak_sd    = sd(trans_weak,    na.rm = TRUE),
    outdeg_mean      = mean(mean_outdeg, na.rm = TRUE),
    outdeg_sd        = sd(mean_outdeg,   na.rm = TRUE),
    indeg_mean       = mean(mean_indeg,  na.rm = TRUE),
    indeg_sd         = sd(mean_indeg,    na.rm = TRUE),
    .groups = "drop"
  )

# Función para "media (sd)"
fmt_ms <- function(m, s) ifelse(is.na(m), NA_character_, sprintf("%.2f (%.2f)", m, s))

summ_disp <- summ_by_size_type %>%
  dplyr::mutate(
    density     = fmt_ms(density_mean,    density_sd),
    reciprocity = fmt_ms(reciprocity_mean,reciprocity_sd),
    trans_weak  = fmt_ms(trans_weak_mean, trans_weak_sd),
    outdeg      = fmt_ms(outdeg_mean,     outdeg_sd),
    indeg       = fmt_ms(indeg_mean,      indeg_sd)
  ) %>%
  dplyr::select(n, type, density, reciprocity, trans_weak, outdeg, indeg)

############################################################
# 8. Pivot a formato ancho: una fila por tamaño, columnas por tipo
############################################################
table_wide <- summ_disp %>%
  tidyr::pivot_wider(
    names_from  = type,
    values_from = c(density, reciprocity, trans_weak, outdeg, indeg),
    names_glue  = "{.value}_{type}"
  ) %>%
  dplyr::arrange(n) %>%
  dplyr::rename(`|V|` = n)

# Orden de columnas (asegúrate que existan; si no, ajusta)
cols_order <- c(
  "|V|",
  "density_dependency", "density_affiliation", "density_marriage",
  "reciprocity_dependency",
  "trans_weak_dependency",
  "outdeg_dependency", "outdeg_affiliation", "outdeg_marriage",
  "indeg_dependency", "indeg_affiliation", "indeg_marriage"
)
table_wide <- table_wide[, intersect(cols_order, names(table_wide))]

############################################################
# 9. Tabla con kableExtra
############################################################
tab <- table_wide %>%
  kbl(format = "html",
      caption = "Indicadores de redes por tamaño (|V| ≥ 2). Media (SD).",
      col.names = c(
        "|V|",
        "Density (Dep.)", "Density (Aff.)", "Density (Mar.)",
        "Reciprocity (Dep.)",
        "Weak trans. (Dep.)",
        "Outdeg (Dep.)", "Outdeg (Aff.)", "Outdeg (Mar.)",
        "Indeg (Dep.)", "Indeg (Aff.)", "Indeg (Mar.)"
      )) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  add_header_above(c(
    " " = 1,
    "Density" = 3,
    "Reciprocity" = 1,
    "Weak transitivity" = 1,
    "Outdegree" = 3,
    "Indegree" = 3
  ))

print(tab)   # Viewer de RStudio
