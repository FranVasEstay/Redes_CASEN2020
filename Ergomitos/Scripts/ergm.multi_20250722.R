## =========================
## 0. Packages & Setup
## =========================
library(ergm.multi)
library(ergm)
library(purrr)
library(dplyr)
library(tibble)
library(network)
library(parallel)
library(sna)  

set.seed(123)

n_cores <- detectCores() - 1

## =========================
## 1. Load networks
## =========================
load("Ergomitos/Redes/dependency_network.RData")
load("Ergomitos/Redes/descent_network.RData")
load("Ergomitos/Redes/marriage_network.RData")

# Extract lists of network objects / matrices
G_dep     <- lapply(dependency_network, `[[`, "dependency_net")
G_descent <- lapply(descent_network,    `[[`, "descent_net")
G_mar     <- lapply(marriage_network,   `[[`, "marriage_net")

## Basic size filter (avoid empty nets)
#keep_nonempty <- vapply(G_dep, network.size, integer(1)) > 0
# Option 1: expect a double
sizes <- vapply(G_dep, network.size, numeric(1))
keep_nonempty <- sizes > 0

G_dep     <- G_dep[keep_nonempty]
G_descent <- G_descent[keep_nonempty]
G_mar     <- G_mar[keep_nonempty]

## Attach descent & marriage matrices as network attributes (optional)
for(i in seq_along(G_dep)){
  G_dep[[i]] %n% "descent"  <- as.matrix(G_descent[[i]])
  G_dep[[i]] %n% "marriage" <- as.matrix(G_mar[[i]])
}

## =========================
## 2. Helper functions
## =========================

# Union kinship adjacency (descent OR marriage) to compute relational distance
make_kin_union <- function(descent_mat, marriage_mat){
  # treat any non-zero as kin tie (undirected for distance)
  kin <- (descent_mat > 0) | (t(descent_mat) > 0) | (marriage_mat > 0) | (t(marriage_mat) > 0)
  kin*1
}

# Return binary matrices for distance bins (1,2,3+). Baseline = 3+
make_distance_bins <- function(adj){
  # adj: matriz de adyacencia (0/1)
  adj_u <- (adj + t(adj)) > 0  # forzar no-dirigida
  gd <- sna::geodist(adj_u * 1, inf.replace = NA)$gdist  # sin gmode
  # Si tu versión devuelve $dist en vez de $gdist:
  if(is.null(gd)) gd <- sna::geodist(adj_u * 1, inf.replace = NA)$dist
  
  list(
    dist1 = 1*(gd == 1),
    dist2 = 1*(gd == 2),
    dist3p = 1*(gd >= 3)
  )
}

# Blood vs affinal dyads (undirected semantics, but keep directed matrices)
make_tietype_mats <- function(descent_mat, marriage_mat){
  blood  <- 1*((descent_mat > 0) | (t(descent_mat) > 0))      # consanguinity
  affinal<- 1*((marriage_mat > 0) | (t(marriage_mat) > 0))    # affinal
  list(blood = blood, affinal = affinal)
}

# Descendant vs ancestor edges (directed parent-child)
make_down_up <- function(descent_mat){
  down <- 1*(descent_mat > 0)     # parent -> child
  up   <- 1*(t(descent_mat) > 0)  # child  -> parent
  list(down = down, up = up)
}

# Eligibility checks
eligible_H1 <- function(bin_list){  # needs at least dist1 & dist2 (baseline=3+ optional)
  any(bin_list$dist1 == 1, na.rm=TRUE) && any(bin_list$dist2 == 1, na.rm=TRUE)
}
eligible_H2 <- function(tt){
  any(tt$blood == 1, na.rm=TRUE) && any(tt$affinal == 1, na.rm=TRUE)
}
eligible_H3 <- function(du){
  any(du$down == 1, na.rm=TRUE) && any(du$up == 1, na.rm=TRUE)
}

## =========================
## 3. Build dyadic covariates for each household
## =========================
dist1_list <- dist2_list <- NULL
blood_list <- affinal_list <- NULL
down_list  <- up_list <- NULL

# Also store eligibility flags
flag_H1 <- flag_H2 <- flag_H3 <- logical(length(G_dep))

for(i in seq_along(G_dep)){
  dmat <- G_dep[[i]] %n% "descent"
  mmat <- G_dep[[i]] %n% "marriage"
  
  kin_union_mat <- make_kin_union(dmat, mmat)
  
  db <- make_distance_bins(kin_union_mat)
  
  dist1_list[[i]] <- db$dist1
  dist2_list[[i]] <- db$dist2
  
  # 3b. tie types -------------------------------------------------------------
  tt <- make_tietype_mats(dmat, mmat)
  blood_list[[i]]   <- tt$blood
  affinal_list[[i]] <- tt$affinal
  
  # 3c. direction (descendant vs ancestor) -----------------------------------
  du <- make_down_up(dmat)
  down_list[[i]] <- du$down
  up_list[[i]]   <- du$up
  
  # Flags ---------------------------------------------------------------------
  flag_H1[i] <- eligible_H1(db)
  flag_H2[i] <- eligible_H2(tt)
  flag_H3[i] <- eligible_H3(du)
}

## =========================
## 4. Filter networks per hypothesis
## =========================
# You can ALSO add size constraints here if desired:
# e.g., size>=3 for H1, >=3 for H2, >=2 for H3.
sizes <- sapply(G_dep, network.size)

# Apply eligibility + size filters
idx_H1 <- which(flag_H1 & sizes >= 3)
idx_H2 <- which(flag_H2 & sizes >= 7)
idx_H3 <- which(flag_H3 & sizes >= 2)

G_H1 <- G_dep[idx_H1]
G_H2 <- G_dep[idx_H2]
G_H3 <- G_dep[idx_H3]

dist1_H1 <- dist1_list[idx_H1]
dist2_H1 <- dist2_list[idx_H1]

blood_H2   <- blood_list[idx_H2]
# Baseline = affinal (not included)
affinal_H2 <- affinal_list[idx_H2]  # Only for checks

down_H3 <- down_list[idx_H3]
up_H3   <- up_list[idx_H3]

cat("Nets kept:\n",
    "H1:", length(G_H1), "\n",
    "H2:", length(G_H2), "\n",
    "H3:", length(G_H3), "\n")

## =========================
## 5. Fit models
## =========================
# Control (tune as needed)
ctrl <- control.ergm(
  parallel = n_cores,
  parallel.type = "PSOCK"
  # You can also tweak MCMC.* here for speed.
)

## 2. Generic timer wrapper
time_fit <- function(formula, label, ctrl){
  t0  <- Sys.time()
  fit <- ergm(formula, control = ctrl)
  el  <- Sys.time() - t0
  cat(sprintf("\n[%s] Elapsed: %.1f min (%.2f h)\n",
              label,
              as.numeric(el, units = "mins"),
              as.numeric(el, units = "hours")))
  fit
}


### H1: Relational proximity (dist1, dist2; baseline = dist>=3)

### H2: Blood vs Affinal (blood; baseline = affinal)

### H3: Downward vs Upward (down; baseline = up)
# Option A: include only 'down' and let 'up' be baseline

## Attach dyadic covariates to each network -------------------------------
for(i in seq_along(G_H1)){
  G_H1[[i]] %n% "dist1" <- dist1_H1[[i]]
  G_H1[[i]] %n% "dist2" <- dist2_H1[[i]]
}

for(i in seq_along(G_H2)){
  G_H2[[i]] %n% "blood" <- blood_H2[[i]]
  # baseline = affinal (so we don't include it)
}

for(i in seq_along(G_H3)){
  G_H3[[i]] %n% "down" <- down_H3[[i]]
  G_H3[[i]] %n% "up"   <- up_H3[[i]]
}

## Rebuild formulas --------------------------------------------------------
f.H1 <- Networks(G_H1) ~ 
  N(~ edges + mutual + edgecov("dist1") + edgecov("dist2"))

f.H2 <- Networks(G_H2) ~ 
  N(~ edges  + edgecov("blood") )   # baseline = affinal

f.H3 <- Networks(G_H3) ~ 
  N(~ edges + mutual + edgecov("down") + edgecov("up"))


# Check one network by hand before running multi:
 #ergm(G_H1[[1]] ~ edges + edgecov("dist1") + edgecov("dist2"))

fit.H1 <- time_fit(f.H1, "H1 (distance)", ctrl)
fit.H2 <- time_fit(f.H2, "H2 (blood vs affinal)", ctrl)
fit.H3 <- time_fit(f.H3, "H3 (down vs up)", ctrl)

summary(fit.H1)
summary(fit.H2)
summary(fit.H3)


# Save them if needed
saveRDS(list(fit.H1 = fit.H1, fit.H2 = fit.H2, fit.H3 = fit.H3), "fits_H123.rds")





## Save them
saveRDS(fit.H1, "Ergomitos/Models/fit_H1.rds")
saveRDS(fit.H2, "Ergomitos/Models/fit_H2.rds")
saveRDS(fit.H3, "Ergomitos/Models/fit_H3.rds")


## =========================
## 6. GOF (optional & lightweight)
## =========================
# plotting the model
library(ggrepel)
library(ggplot2)

# For multi.ergm, use gofN
gof.H1 <- gofN(fit.H1, GOF = ~ edges)
gof.H2 <- gofN(fit.H2, GOF = ~ edges)
gof.H3 <- gofN(fit.H3, GOF = ~ edges)

# plot(gof.H1); plot(gof.H2); plot(gof.H3)

## Graficar ajuste del modelo---------------------------------------------------------------------------------------------------------------------------------
autoplot(gof.H2)
#plot(gof.wd)
## Comparación con otras métricas-----------------------------------------------------------------------------------------------------------------------------
autoplot(gof.wd, against=sqrt(.fitted))
autoplot(gof.wd, against=ordered(n))
