## =====================================================
## Multi-ERGM script with cleaned dyadic covariates
##  - affinal = in-laws ONLY (no spouses)
##  - blood & affinal provided as separate edgecov matrices
##  - distance bins with NA -> 0 to avoid ergm NA stats crash
##  - safety cleaners for all matrices
## =====================================================

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
n_cores <- parallel::detectCores() - 1

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
sizes <- vapply(G_dep, network.size, numeric(1))
keep_nonempty <- sizes > 0

G_dep     <- G_dep[keep_nonempty]
G_descent <- G_descent[keep_nonempty]
G_mar     <- G_mar[keep_nonempty]

## Attach descent & marriage matrices as network attributes
for(i in seq_along(G_dep)){
  G_dep[[i]] %n% "descent"  <- as.matrix(G_descent[[i]])
  G_dep[[i]] %n% "marriage" <- as.matrix(G_mar[[i]])
}

## =========================
## 2. Helper functions
## =========================

# 2a. Kin union (used for geodesic distances)
make_kin_union <- function(D, M){
  kin <- (D > 0) | (t(D) > 0) | (M > 0) | (t(M) > 0)
  kin * 1
}

# 2b. Distance bins (1, 2, 3+) on an undirected adjacency. NAs -> 0
make_distance_bins <- function(adj){
  adj_u <- (adj + t(adj)) > 0
  gd <- sna::geodist(adj_u * 1, inf.replace = NA)$gdist
  if(is.null(gd)) gd <- sna::geodist(adj_u * 1, inf.replace = NA)$dist
  
  dist1  <- (gd == 1)
  dist2  <- (gd == 2)
  dist3p <- (gd >= 3)
  
  dist1[is.na(dist1)]   <- 0
  dist2[is.na(dist2)]   <- 0
  dist3p[is.na(dist3p)] <- 0
  
  storage.mode(dist1)  <- "double"
  storage.mode(dist2)  <- "double"
  storage.mode(dist3p) <- "double"
  
  list(dist1 = 1*dist1, dist2 = 1*dist2, dist3p = 1*dist3p)
}

# 2c. Blood & Affinal (in-laws only, exclude spouses)
# D: descent matrix (directed parent->child), M: marriage matrix (directed or undirected)
make_tietype_mats_no_spouse <- function(D, M){
  # Undirected versions
  D_u <- ((D > 0) | (t(D) > 0)) * 1          # consanguinity edges
  M_u <- ((M > 0) | (t(M) > 0)) * 1          # spouse edges
  
  # Direct blood
  blood_direct   <- D_u == 1
  # Direct spouses
  spouse_direct  <- M_u == 1
  
  # In-law ties: 2-step paths with one marriage edge and one descent edge
  # ego --M-- x --D-- alter  OR  ego --D-- x --M-- alter
  inlaw_paths <- ((M_u %*% D_u) > 0) | ((D_u %*% M_u) > 0)
  
  # Remove direct blood and spouses from affinal set
  affinal_inlaws <- inlaw_paths & !blood_direct & !spouse_direct
  
  # Clean diagonals
  diag(blood_direct)   <- 0
  diag(spouse_direct)  <- 0
  diag(affinal_inlaws) <- 0
  
  list(
    blood   = 1 * blood_direct,
    affinal = 1 * affinal_inlaws,
    spouse  = 1 * spouse_direct  # kept for inspection if needed
  )
}

# 2d. Down vs Up (directional)
make_down_up <- function(D){
  down <- 1*(D > 0)        # parent -> child
  up   <- 1*(t(D) > 0)     # child  -> parent
  list(down = down, up = up)
}

# 2e. Eligibility checks
eligible_H1 <- function(db){
  any(db$dist1 == 1, na.rm=TRUE) && any(db$dist2 == 1, na.rm=TRUE)
}
eligible_H2 <- function(blood_mat, affinal_mat){
  any(blood_mat == 1, na.rm=TRUE) && any(affinal_mat == 1, na.rm=TRUE)
}
eligible_H3 <- function(du){
  any(du$down == 1, na.rm=TRUE) && any(du$up == 1, na.rm=TRUE)
}

# 2f. Cleaner for matrices used in edgecov
clean_mat <- function(M){
  M[is.na(M)] <- 0
  diag(M) <- 0
  storage.mode(M) <- "double"
  M
}

## =========================
## 3. Build dyadic covariates for each household
## =========================

dist1_list <- dist2_list <- NULL
blood_list <- affinal_list <- NULL
down_list  <- up_list <- NULL

flag_H1 <- flag_H2 <- flag_H3 <- logical(length(G_dep))

for(i in seq_along(G_dep)){
  D <- G_dep[[i]] %n% "descent"
  M <- G_dep[[i]] %n% "marriage"
  
  # 3a. distance bins
  kin_union_mat <- make_kin_union(D, M)
  db <- make_distance_bins(kin_union_mat)
  dist1_list[[i]] <- db$dist1
  dist2_list[[i]] <- db$dist2
  
  # 3b. tie types (blood vs affinal-inlaw, no spouses)
  tt <- make_tietype_mats_no_spouse(D, M)
  blood_list[[i]]   <- tt$blood
  affinal_list[[i]] <- tt$affinal
  
  # 3c. direction (descendant vs ancestor)
  du <- make_down_up(D)
  down_list[[i]] <- du$down
  up_list[[i]]   <- du$up
  
  # Flags
  flag_H1[i] <- eligible_H1(db)
  flag_H2[i] <- eligible_H2(tt$blood, tt$affinal)
  flag_H3[i] <- eligible_H3(du)
}

## =========================
## 4. Filter networks per hypothesis
## =========================

sizes <- sapply(G_dep, network.size)

# Adjust thresholds as needed
idx_H1 <- which(flag_H1 & sizes >= 7)  #3
idx_H2 <- which(flag_H2 & sizes >= 7)  #3
idx_H3 <- which(flag_H3 & sizes >= 7)  #2

G_H1 <- G_dep[idx_H1]
G_H2 <- G_dep[idx_H2]
G_H3 <- G_dep[idx_H3]

dist1_H1 <- dist1_list[idx_H1]
dist2_H1 <- dist2_list[idx_H1]

blood_H2   <- blood_list[idx_H2]
affinal_H2 <- affinal_list[idx_H2]

down_H3 <- down_list[idx_H3]
up_H3   <- up_list[idx_H3]

cat("Nets kept:\n",
    "H1:", length(G_H1), "\n",
    "H2:", length(G_H2), "\n",
    "H3:", length(G_H3), "\n")

## =========================
## 5. Attach covariates & fit models
## =========================

# Control (set drop=FALSE to avoid silent term dropping after NA removal)
ctrl <- control.ergm(
  parallel = n_cores,
  parallel.type = "PSOCK",
  drop = FALSE
)

# Timer wrapper
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

## Attach dyadic covariates -------------------------------
for(i in seq_along(G_H1)){
  G_H1[[i]] %n% "dist1" <- clean_mat(dist1_H1[[i]])
  G_H1[[i]] %n% "dist2" <- clean_mat(dist2_H1[[i]])
}

for(i in seq_along(G_H2)){
  G_H2[[i]] %n% "blood"   <- clean_mat(blood_H2[[i]])
  G_H2[[i]] %n% "affinal" <- clean_mat(affinal_H2[[i]])
}

for(i in seq_along(G_H3)){
  G_H3[[i]] %n% "down" <- clean_mat(down_H3[[i]])
  G_H3[[i]] %n% "up"   <- clean_mat(up_H3[[i]])
}

## Optional sanity checks -----------------------------------------------------
chk <- function(nw, name){
  m <- nw %n% name
  anyNA(m) || any(dim(m) != network.size(nw))
}
# sapply(G_H1, chk, "dist1")
# sapply(G_H2, chk, "blood")

## Formulas ------------------------------------------------
# H1: distance1 & distance2; baseline = dist>=3
f.H1 <- Networks(G_H1) ~ 
  N(~ edges + mutual + edgecov("dist1") + edgecov("dist2"))

# H2: blood vs affinal-inlaw (both included; baseline = neither)
f.H2 <- Networks(G_H2) ~ 
  N(~ edges + mutual + edgecov("blood") + edgecov("affinal"))

# H3: down vs up (include both; baseline = neither)
f.H3 <- Networks(G_H3) ~ 
  N(~ edges + mutual + edgecov("down") + edgecov("up"))

## Optional: single-network test before multi -------------------------------
# ergm(G_H2[[1]] ~ edges + edgecov("blood") + edgecov("affinal"))

## Fits ----------------------------------------------------
fit.H1 <- time_fit(f.H1, "H1 (distance)", ctrl)
fit.H2 <- time_fit(f.H2, "H2 (blood vs affinal-inlaw)", ctrl)
fit.H3 <- time_fit(f.H3, "H3 (down vs up)", ctrl)

summary(fit.H1)
summary(fit.H2)
summary(fit.H3)

## Save ----------------------------------------------------
saveRDS(list(fit.H1 = fit.H1, fit.H2 = fit.H2, fit.H3 = fit.H3), "fits_H123.rds")
saveRDS(fit.H1, "Ergomitos/Models/fit_H1.rds")
saveRDS(fit.H2, "Ergomitos/Models/fit_H2.rds")
saveRDS(fit.H3, "Ergomitos/Models/fit_H3.rds")

## =========================
## 6. GOF (optional)
## =========================
# gofN is the multi-ERGM GOF

# Basic GOF
gof.H1 <- gofN(fit.H1, GOF = ~ edges)
gof.H2 <- gofN(fit.H2, GOF = ~ edges)
gof.H3 <- gofN(fit.H3, GOF = ~ edges)

# plot(gof.H1); plot(gof.H2); plot(gof.H3)

# If you want ggplot-style autoplot and have methods loaded:
# library(ggplot2)
# 
# 
 autoplot(gof.H1)
 autoplot(gof.H1, against = sqrt(.fitted))
 autoplot(gof.H1, against = ordered(n))

 autoplot(gof.H2)
 autoplot(gof.H2, against = sqrt(.fitted))
 autoplot(gof.H2, against = ordered(n))

 autoplot(gof.H3)
 autoplot(gof.H3, against = sqrt(.fitted))
 autoplot(gof.H3, against = ordered(n))
 
## =========================
## 7. Debug helper (optional)
## =========================
# If it crashes again, inspect observed stats:
# ergm::ergm.getglobalstats(f.H1)
# ergm::ergm.getglobalstats(f.H2)
# ergm::ergm.getglobalstats(f.H3)
