# ============================================================
# 0) Pacotes e configurações gerais
# ============================================================

rm(list = ls())
gc()

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(forecast)
library(demography)
library(StMoMo)
library(ggplot2)
library(scales)

options(scipen = 999)
set.seed(123)

# ============================================================
# 1) Leitura das bases
# ============================================================

# AJUSTE OS CAMINHOS CONFORME SEU COMPUTADOR
path_mort <- "C:\\Users\\luana\\Downloads\\Artigo - Arquivos\\Mortalidade (SIM).xlsx"
path_pop  <- "C:\\Users\\luana\\Downloads\\Artigo - Arquivos\\Projeções População por idade simples 2000 a 2070 (IBGE, 2024).xlsx"

Mort <- read_excel(path_mort)
Pop  <- read_excel(path_pop, skip = 5)

names(Mort) <- gsub("\\.0$", "", names(Mort))
names(Pop)  <- gsub("\\.0$", "", names(Pop))

# ============================================================
# 2) Parâmetros do estudo
# ============================================================

ANO_INI   <- 1994
ANO_FIM   <- 2022
IDADE_MAX <- 90
DEC       <- 5

TRAIN_END  <- 2017
TEST_YEARS <- 2018:2022

OUT_DIR <- "outputs_resultados"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT_DIR, "tabelas"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT_DIR, "graficos"), showWarnings = FALSE, recursive = TRUE)

# ============================================================
# 3) Tema e paletas padronizadas
# ============================================================

tema_tcc <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5, size = 11, colour = "grey35"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10.5, colour = "grey20"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10.5),
    legend.box = "horizontal",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.28, colour = "grey85"),
    axis.line = element_line(linewidth = 0.3, colour = "grey45"),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey95", colour = NA)
  )

pal_sexo <- c(
  "Masculino" = "#0B3C8C",
  "Feminino"  = "#A11D33",
  "Ambos"     = "#4D4D4D"
)

pal_modelo <- c(
  "LC-SVD" = "#0B3C8C",
  "LC-Poisson" = "#A11D33"
)

pal_serie_prev <- c(
  "Observado" = "#1A1A1A",
  "Previsto LC-SVD" = "#0B3C8C",
  "Previsto LC-Poisson" = "#A11D33"
)

pal_serie_composta <- c(
  "Masculino — LC-SVD" = "#0B3C8C",
  "Masculino — LC-Poisson" = "#5DA5DA",
  "Feminino — LC-SVD" = "#A11D33",
  "Feminino — LC-Poisson" = "#F28E8E"
)

# ============================================================
# 4) Filtro Brasil e padronização
# ============================================================

Mort_BR <- Mort %>% filter(SIGLA == "BR")
Pop_BR  <- Pop  %>% filter(SIGLA == "BR")

Pop_BR <- Pop_BR %>%
  mutate(
    SEXO = recode(
      SEXO,
      "Homens"   = "Masculino",
      "Mulheres" = "Feminino",
      .default   = as.character(SEXO)
    )
  )

# ============================================================
# 5) Construção da base: Dx, Ex, mx, logmx, qx
# ============================================================

anos_mort <- names(Mort_BR)[grepl("^[0-9]{4}$", names(Mort_BR))]
anos_pop  <- names(Pop_BR )[grepl("^[0-9]{4}$", names(Pop_BR ))]

anos_comuns <- intersect(anos_mort, anos_pop) %>%
  as.integer() %>%
  sort()

anos_comuns <- anos_comuns[anos_comuns >= ANO_INI & anos_comuns <= ANO_FIM]

if (length(anos_comuns) == 0) stop("Não há anos comuns no intervalo escolhido.")

anos_comuns_chr <- as.character(anos_comuns)

Dx_long <- Mort_BR %>%
  select(IDADE, SEXO, all_of(anos_comuns_chr)) %>%
  pivot_longer(
    cols = all_of(anos_comuns_chr),
    names_to = "Year",
    values_to = "Dx"
  ) %>%
  transmute(
    Year = as.integer(Year),
    Age  = as.integer(IDADE),
    Sex  = as.character(SEXO),
    Dx   = as.numeric(Dx)
  )

Ex_long <- Pop_BR %>%
  select(IDADE, SEXO, all_of(anos_comuns_chr)) %>%
  pivot_longer(
    cols = all_of(anos_comuns_chr),
    names_to = "Year",
    values_to = "Ex"
  ) %>%
  transmute(
    Year = as.integer(Year),
    Age  = as.integer(IDADE),
    Sex  = as.character(SEXO),
    Ex   = as.numeric(Ex)
  )

base_mf <- Dx_long %>%
  left_join(Ex_long, by = c("Year", "Age", "Sex")) %>%
  filter(Sex %in% c("Masculino", "Feminino")) %>%
  complete(
    Sex,
    Year = anos_comuns,
    Age = 0:IDADE_MAX
  ) %>%
  arrange(Sex, Year, Age)

base_mf <- base_mf %>%
  mutate(
    Dx = as.numeric(Dx),
    Ex = as.numeric(Ex),
    mx = if_else(is.na(Ex) | Ex <= 0, NA_real_, Dx / Ex),
    logmx = if_else(!is.na(mx) & mx > 0, log(mx), NA_real_)
  )

calc_a <- function(sex, age, mx) {
  if (is.na(mx)) return(NA_real_)
  
  if (age == 0) {
    if (mx >= 0.107) {
      if (sex == "Masculino") return(0.330)
      if (sex == "Feminino")  return(0.350)
    } else {
      if (sex == "Masculino") return(0.045 + 2.684 * mx)
      if (sex == "Feminino")  return(0.053 + 2.800 * mx)
    }
  }
  
  if (age >= 1 && age <= 4) {
    if (mx >= 0.107) {
      if (sex == "Masculino") return(1.352)
      if (sex == "Feminino")  return(1.361)
    } else {
      if (sex == "Masculino") return(1.651 - 2.816 * mx)
      if (sex == "Feminino")  return(1.522 - 1.518 * mx)
    }
  }
  
  return(0.5)
}

base_mf <- base_mf %>%
  mutate(
    n  = 1,
    ax = mapply(calc_a, Sex, Age, mx),
    qx = if_else(
      is.na(mx),
      NA_real_,
      (n * mx) / (1 + (n - n * ax) * mx)
    )
  )

ax_m <- base_mf %>%
  filter(Sex == "Masculino") %>%
  select(Year, Age, ax_m = ax)

ax_f <- base_mf %>%
  filter(Sex == "Feminino") %>%
  select(Year, Age, ax_f = ax)

base_ambos <- base_mf %>%
  group_by(Year, Age) %>%
  summarise(
    Dx = sum(Dx, na.rm = TRUE),
    Ex = sum(Ex, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Sex = "Ambos",
    mx = if_else(is.na(Ex) | Ex <= 0, NA_real_, Dx / Ex),
    logmx = if_else(!is.na(mx) & mx > 0, log(mx), NA_real_)
  ) %>%
  left_join(ax_m, by = c("Year", "Age")) %>%
  left_join(ax_f, by = c("Year", "Age")) %>%
  mutate(
    n = 1,
    ax = if_else(Age <= 4, (ax_m + ax_f) / 2, 0.5),
    qx = if_else(
      is.na(mx),
      NA_real_,
      (n * mx) / (1 + (n - n * ax) * mx)
    )
  ) %>%
  select(Year, Age, Sex, Dx, Ex, mx, logmx, ax, qx) %>%
  arrange(Year, Age)

base <- base_mf %>%
  select(Year, Age, Sex, Dx, Ex, mx, logmx, ax, qx) %>%
  bind_rows(base_ambos) %>%
  arrange(Sex, Year, Age)

write_delim(
  base,
  file.path(OUT_DIR, "tabelas", "base_completa_Dx_Ex_mx_qx.tsv"),
  delim = "\t"
)

export_sexo <- function(sex_label) {
  out <- base %>%
    filter(Sex == sex_label) %>%
    mutate(
      mx_fmt = if_else(is.na(mx), NA_character_, formatC(mx, format = "f", digits = DEC)),
      qx_fmt = if_else(is.na(qx), NA_character_, formatC(qx, format = "f", digits = DEC))
    ) %>%
    select(Year, Age, mx = mx_fmt, qx = qx_fmt) %>%
    arrange(Year, Age)
  
  write_delim(
    out,
    file.path(OUT_DIR, "tabelas", paste0("TaxaMortalidade_", sex_label, ".tsv")),
    delim = "\t"
  )
  out
}

mxqx_M <- export_sexo("Masculino")
mxqx_F <- export_sexo("Feminino")
mxqx_A <- export_sexo("Ambos")

# ============================================================
# 6) Diagnósticos iniciais
# ============================================================

tab_missing <- base %>%
  group_by(Sex) %>%
  summarise(
    anos = n_distinct(Year),
    idades = n_distinct(Age),
    pct_mx_na = mean(is.na(mx)),
    pct_ex_na = mean(is.na(Ex)),
    pct_dx_na = mean(is.na(Dx)),
    .groups = "drop"
  )

write_delim(
  tab_missing,
  file.path(OUT_DIR, "tabelas", "diagnostico_missing.tsv"),
  delim = "\t"
)

g1 <- base %>%
  filter(Sex %in% c("Masculino", "Feminino", "Ambos")) %>%
  group_by(Sex, Year) %>%
  summarise(media_logmx = mean(logmx, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Year, y = media_logmx, color = Sex, linetype = Sex, shape = Sex)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.2) +
  scale_color_manual(values = pal_sexo) +
  scale_linetype_manual(values = c("Masculino" = "solid", "Feminino" = "22", "Ambos" = "42")) +
  scale_shape_manual(values = c("Masculino" = 16, "Feminino" = 17, "Ambos" = 15)) +
  labs(
    title = "Média anual de log(m[x,t]) por sexo",
    x = "Ano",
    y = "Média de log(m[x,t])"
  ) +
  guides(linetype = "none", shape = "none") +
  tema_tcc

ggsave(
  file.path(OUT_DIR, "graficos", "01_media_logmx_por_ano.png"),
  g1,
  width = 10,
  height = 5,
  dpi = 300
)

# ============================================================
# 7) Preparação das matrizes
# ============================================================

make_mats <- function(df_sex, idade_max = IDADE_MAX) {
  ages  <- 0:idade_max
  years <- sort(unique(df_sex$Year))
  
  mx_mat <- df_sex %>%
    select(Year, Age, mx) %>%
    complete(Year = years, Age = ages) %>%
    arrange(Age, Year) %>%
    pivot_wider(names_from = Year, values_from = mx) %>%
    arrange(Age)
  
  mx <- as.matrix(mx_mat[, -1])
  rownames(mx) <- mx_mat$Age
  
  D_mat <- df_sex %>%
    select(Year, Age, Dx) %>%
    complete(Year = years, Age = ages) %>%
    arrange(Age, Year) %>%
    pivot_wider(names_from = Year, values_from = Dx) %>%
    arrange(Age)
  
  E_mat <- df_sex %>%
    select(Year, Age, Ex) %>%
    complete(Year = years, Age = ages) %>%
    arrange(Age, Year) %>%
    pivot_wider(names_from = Year, values_from = Ex) %>%
    arrange(Age)
  
  D <- as.matrix(D_mat[, -1])
  rownames(D) <- D_mat$Age
  
  E <- as.matrix(E_mat[, -1])
  rownames(E) <- E_mat$Age
  
  list(
    ages = ages,
    years = years,
    mx = mx,
    D = D,
    E = E
  )
}

data_M <- make_mats(base %>% filter(Sex == "Masculino"))
data_F <- make_mats(base %>% filter(Sex == "Feminino"))
data_A <- make_mats(base %>% filter(Sex == "Ambos"))

# ============================================================
# 8) Ajuste dos modelos
# ============================================================

fit_models <- function(data_list, label = "") {
  ages  <- data_list$ages
  years <- data_list$years
  
  dmx <- demogdata(
    data  = data_list$mx,
    pop   = data_list$E,
    ages  = ages,
    years = years,
    type  = "mortality",
    label = "BR",
    name  = label
  )
  
  fit_svd <- lca(dmx)
  
  lc_model <- lc()
  fit_pois <- fit(
    lc_model,
    Dxt = data_list$D,
    Ext = data_list$E,
    ages = ages,
    years = years
  )
  
  list(svd = fit_svd, pois = fit_pois)
}

models_M <- fit_models(data_M, "Masculino")
models_F <- fit_models(data_F, "Feminino")
models_A <- fit_models(data_A, "Ambos")

# ============================================================
# 9) Componentes dos modelos
# ============================================================

extract_components <- function(fit_svd, fit_pois) {
  list(
    svd = list(
      ages = fit_svd$age,
      years = fit_svd$year,
      ax = fit_svd$ax,
      bx = fit_svd$bx,
      kt = fit_svd$kt
    ),
    pois = list(
      ages = fit_pois$ages,
      years = fit_pois$years,
      ax = fit_pois$ax,
      bx = fit_pois$bx,
      kt = fit_pois$kt
    )
  )
}

comp_M <- extract_components(models_M$svd, models_M$pois)
comp_F <- extract_components(models_F$svd, models_F$pois)
comp_A <- extract_components(models_A$svd, models_A$pois)

# ============================================================
# 10) Gráficos padronizados dos componentes
# ============================================================

plot_ax_bx_by_sex <- function(comp_M, comp_F, out_dir = OUT_DIR) {
  
  df_ax <- bind_rows(
    tibble(
      Age = comp_M$svd$ages,
      Sexo = "Masculino",
      `LC-SVD` = as.numeric(comp_M$svd$ax),
      `LC-Poisson` = as.numeric(comp_M$pois$ax)
    ),
    tibble(
      Age = comp_F$svd$ages,
      Sexo = "Feminino",
      `LC-SVD` = as.numeric(comp_F$svd$ax),
      `LC-Poisson` = as.numeric(comp_F$pois$ax)
    )
  ) %>%
    pivot_longer(
      cols = c(`LC-SVD`, `LC-Poisson`),
      names_to = "Modelo",
      values_to = "valor"
    ) %>%
    mutate(
      Serie = case_when(
        Sexo == "Masculino" & Modelo == "LC-SVD" ~ "Masculino — LC-SVD",
        Sexo == "Masculino" & Modelo == "LC-Poisson" ~ "Masculino — LC-Poisson",
        Sexo == "Feminino" & Modelo == "LC-SVD" ~ "Feminino — LC-SVD",
        Sexo == "Feminino" & Modelo == "LC-Poisson" ~ "Feminino — LC-Poisson"
      ),
      Serie = factor(
        Serie,
        levels = c(
          "Masculino — LC-SVD",
          "Masculino — LC-Poisson",
          "Feminino — LC-SVD",
          "Feminino — LC-Poisson"
        )
      )
    )
  
  g_ax <- ggplot(df_ax, aes(x = Age, y = valor, color = Serie, linetype = Serie, shape = Serie)) +
    geom_line(linewidth = 1.05) +
    geom_point(size = 2.3, stroke = 0.8) +
    scale_color_manual(values = pal_serie_composta) +
    scale_linetype_manual(values = c(
      "Masculino — LC-SVD" = "solid",
      "Masculino — LC-Poisson" = "22",
      "Feminino — LC-SVD" = "solid",
      "Feminino — LC-Poisson" = "22"
    )) +
    scale_shape_manual(values = c(
      "Masculino — LC-SVD" = 16,
      "Masculino — LC-Poisson" = 17,
      "Feminino — LC-SVD" = 16,
      "Feminino — LC-Poisson" = 17
    )) +
    labs(
      title = "Parâmetro a[x] por idade",
      x = "Idade",
      y = "a[x]"
    ) +
    guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      linetype = "none",
      shape = "none"
    ) +
    tema_tcc
  
  df_bx <- bind_rows(
    tibble(
      Age = comp_M$svd$ages,
      Sexo = "Masculino",
      `LC-SVD` = as.numeric(comp_M$svd$bx),
      `LC-Poisson` = as.numeric(comp_M$pois$bx)
    ),
    tibble(
      Age = comp_F$svd$ages,
      Sexo = "Feminino",
      `LC-SVD` = as.numeric(comp_F$svd$bx),
      `LC-Poisson` = as.numeric(comp_F$pois$bx)
    )
  ) %>%
    pivot_longer(
      cols = c(`LC-SVD`, `LC-Poisson`),
      names_to = "Modelo",
      values_to = "valor"
    ) %>%
    mutate(
      Serie = case_when(
        Sexo == "Masculino" & Modelo == "LC-SVD" ~ "Masculino — LC-SVD",
        Sexo == "Masculino" & Modelo == "LC-Poisson" ~ "Masculino — LC-Poisson",
        Sexo == "Feminino" & Modelo == "LC-SVD" ~ "Feminino — LC-SVD",
        Sexo == "Feminino" & Modelo == "LC-Poisson" ~ "Feminino — LC-Poisson"
      ),
      Serie = factor(
        Serie,
        levels = c(
          "Masculino — LC-SVD",
          "Masculino — LC-Poisson",
          "Feminino — LC-SVD",
          "Feminino — LC-Poisson"
        )
      )
    )
  
  g_bx <- ggplot(df_bx, aes(x = Age, y = valor, color = Serie, linetype = Serie, shape = Serie)) +
    geom_line(linewidth = 1.05) +
    geom_point(size = 2.3, stroke = 0.8) +
    scale_color_manual(values = pal_serie_composta) +
    scale_linetype_manual(values = c(
      "Masculino — LC-SVD" = "solid",
      "Masculino — LC-Poisson" = "22",
      "Feminino — LC-SVD" = "solid",
      "Feminino — LC-Poisson" = "22"
    )) +
    scale_shape_manual(values = c(
      "Masculino — LC-SVD" = 16,
      "Masculino — LC-Poisson" = 17,
      "Feminino — LC-SVD" = 16,
      "Feminino — LC-Poisson" = 17
    )) +
    labs(
      title = "Parâmetro b[x] por idade",
      x = "Idade",
      y = "b[x]"
    ) +
    guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      linetype = "none",
      shape = "none"
    ) +
    tema_tcc
  
  ggsave(file.path(out_dir, "graficos", "02_ax_MF_conjunto.png"), g_ax, width = 10, height = 5.4, dpi = 300)
  ggsave(file.path(out_dir, "graficos", "03_bx_MF_conjunto.png"), g_bx, width = 10, height = 5.4, dpi = 300)
  
  list(g_ax = g_ax, g_bx = g_bx)
}

plot_kt_by_sex <- function(comp_M, comp_F, out_dir = OUT_DIR) {
  
  df_kt <- bind_rows(
    tibble(
      Year = comp_M$svd$years,
      Sexo = "Masculino",
      `LC-SVD` = as.numeric(comp_M$svd$kt),
      `LC-Poisson` = as.numeric(comp_M$pois$kt)
    ),
    tibble(
      Year = comp_F$svd$years,
      Sexo = "Feminino",
      `LC-SVD` = as.numeric(comp_F$svd$kt),
      `LC-Poisson` = as.numeric(comp_F$pois$kt)
    )
  ) %>%
    pivot_longer(
      cols = c(`LC-SVD`, `LC-Poisson`),
      names_to = "Modelo",
      values_to = "valor"
    ) %>%
    mutate(
      Serie = case_when(
        Sexo == "Masculino" & Modelo == "LC-SVD" ~ "Masculino — LC-SVD",
        Sexo == "Masculino" & Modelo == "LC-Poisson" ~ "Masculino — LC-Poisson",
        Sexo == "Feminino" & Modelo == "LC-SVD" ~ "Feminino — LC-SVD",
        Sexo == "Feminino" & Modelo == "LC-Poisson" ~ "Feminino — LC-Poisson"
      ),
      Serie = factor(
        Serie,
        levels = c(
          "Masculino — LC-SVD",
          "Masculino — LC-Poisson",
          "Feminino — LC-SVD",
          "Feminino — LC-Poisson"
        )
      )
    )
  
  g_kt <- ggplot(df_kt, aes(x = Year, y = valor, color = Serie, linetype = Serie, shape = Serie)) +
    geom_line(linewidth = 1.05) +
    geom_point(size = 2.2, stroke = 0.8) +
    scale_color_manual(values = pal_serie_composta) +
    scale_linetype_manual(values = c(
      "Masculino — LC-SVD" = "solid",
      "Masculino — LC-Poisson" = "22",
      "Feminino — LC-SVD" = "solid",
      "Feminino — LC-Poisson" = "22"
    )) +
    scale_shape_manual(values = c(
      "Masculino — LC-SVD" = 16,
      "Masculino — LC-Poisson" = 17,
      "Feminino — LC-SVD" = 16,
      "Feminino — LC-Poisson" = 17
    )) +
    labs(
      title = "Índice temporal k[t]",
      x = "Ano",
      y = "k[t]"
    ) +
    guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      linetype = "none",
      shape = "none"
    ) +
    tema_tcc
  
  ggsave(file.path(out_dir, "graficos", "04_kt_MF_conjunto.png"), g_kt, width = 10, height = 5.4, dpi = 300)
  
  g_kt
}

plots_axbx_MF <- plot_ax_bx_by_sex(comp_M, comp_F, out_dir = OUT_DIR)
g_kt_MF <- plot_kt_by_sex(comp_M, comp_F, out_dir = OUT_DIR)

# ============================================================
# 11) Avaliação fora da amostra — split fixo
# ============================================================

metricas <- function(df, pred_col = "pred_mx") {
  w <- df$Ex
  w <- ifelse(is.na(w) | w <= 0, NA_real_, w)
  w <- w / sum(w, na.rm = TRUE)
  
  e_m  <- df$mx - df[[pred_col]]
  e_lm <- df$logmx - log(df[[pred_col]])
  
  tibble(
    RMSE       = sqrt(mean(e_m^2, na.rm = TRUE)),
    MAE        = mean(abs(e_m), na.rm = TRUE),
    RMSE_log   = sqrt(mean(e_lm^2, na.rm = TRUE)),
    MAE_log    = mean(abs(e_lm), na.rm = TRUE),
    RMSE_w     = sqrt(sum(w * (e_m^2), na.rm = TRUE)),
    MAE_w      = sum(w * abs(e_m), na.rm = TRUE),
    RMSE_log_w = sqrt(sum(w * (e_lm^2), na.rm = TRUE)),
    MAE_log_w  = sum(w * abs(e_lm), na.rm = TRUE)
  )
}

as_num_vec <- function(x) {
  x <- unlist(x, use.names = FALSE)
  suppressWarnings(as.numeric(x))
}

pred_svd_multi_from_kt <- function(lca_fit, h = 5, train_end = TRAIN_END) {
  ax <- as.numeric(lca_fit$ax)
  bx <- as.numeric(lca_fit$bx)
  kt <- as.numeric(lca_fit$kt)
  
  fit_arima <- forecast::auto.arima(kt)
  fc_kt <- forecast::forecast(fit_arima, h = h)
  
  kt_fc <- as.numeric(fc_kt$mean)
  
  mx_fc <- sapply(kt_fc, function(k) exp(ax + bx * k))
  mx_fc <- as.matrix(mx_fc)
  
  if (ncol(mx_fc) != h) {
    mx_fc <- matrix(mx_fc, nrow = length(ax), ncol = h)
  }
  
  colnames(mx_fc) <- as.character((train_end + 1):(train_end + h))
  rownames(mx_fc) <- names(ax)
  
  mx_fc
}

run_fixed_split <- function(df_sex,
                            idade_max = IDADE_MAX,
                            train_end = TRAIN_END,
                            test_years = TEST_YEARS) {
  
  train <- df_sex %>% filter(Year <= train_end)
  test  <- df_sex %>% filter(Year %in% test_years) %>% arrange(Year, Age)
  
  mats <- make_mats(train, idade_max = idade_max)
  mods <- fit_models(mats, label = paste0(unique(df_sex$Sex), "_split_fixo"))
  
  svd_fc_mat <- pred_svd_multi_from_kt(mods$svd, h = length(test_years), train_end = train_end)
  
  df_svd <- lapply(seq_along(test_years), function(i) {
    yr <- test_years[i]
    tibble(
      Year = yr,
      Age = 0:idade_max,
      pred_svd = as.numeric(svd_fc_mat[, i])
    )
  }) %>% bind_rows()
  
  pois_fc <- forecast::forecast(mods$pois, h = length(test_years))
  rates_p <- pois_fc$rates
  
  if (length(dim(rates_p)) == 3) rates_p <- rates_p[, , 1]
  rates_p <- as.matrix(rates_p)
  
  years_p <- pois_fc$years
  if (is.null(years_p)) years_p <- test_years
  
  df_pois <- lapply(seq_along(test_years), function(i) {
    yr <- test_years[i]
    idx <- which(years_p == yr)
    
    if (length(idx) == 0 && !is.null(colnames(rates_p)) && as.character(yr) %in% colnames(rates_p)) {
      pred <- as_num_vec(rates_p[, as.character(yr), drop = TRUE])
    } else if (length(idx) > 0) {
      pred <- as_num_vec(rates_p[, idx[1], drop = TRUE])
    } else {
      stop(paste("Ano previsto não encontrado no forecast LC-Poisson:", yr))
    }
    
    tibble(
      Year = yr,
      Age = 0:idade_max,
      pred_pois = pred
    )
  }) %>% bind_rows()
  
  test %>%
    left_join(df_svd,  by = c("Year", "Age")) %>%
    left_join(df_pois, by = c("Year", "Age")) %>%
    mutate(
      h = Year - train_end,
      t0 = train_end,
      YearForecast = Year
    ) %>%
    arrange(Year, Age)
}

bt_M <- run_fixed_split(base %>% filter(Sex == "Masculino"))
bt_F <- run_fixed_split(base %>% filter(Sex == "Feminino"))
bt_A <- run_fixed_split(base %>% filter(Sex == "Ambos"))

# ============================================================
# 12) Checagens
# ============================================================

check_bt <- function(bt, label) {
  cat("\n==", label, "==\n")
  print(
    bt %>%
      summarise(
        n = n(),
        anos_previstos = n_distinct(YearForecast),
        idades = n_distinct(Age),
        h_vals = paste(sort(unique(h)), collapse = ", "),
        t0_min = min(t0),
        t0_max = max(t0),
        y_min = min(YearForecast),
        y_max = max(YearForecast)
      )
  )
}

audit_preds <- function(bt, label) {
  cat("\n== Audit:", label, "==\n")
  print(
    bt %>%
      summarise(
        pct_pred_svd_na  = mean(is.na(pred_svd)),
        pct_pred_pois_na = mean(is.na(pred_pois)),
        min_pred_svd     = min(pred_svd, na.rm = TRUE),
        min_pred_pois    = min(pred_pois, na.rm = TRUE)
      )
  )
}

check_bt(bt_M, "Masculino")
check_bt(bt_F, "Feminino")
check_bt(bt_A, "Ambos")

audit_preds(bt_M, "Masculino")
audit_preds(bt_F, "Feminino")
audit_preds(bt_A, "Ambos")

compare_levels <- function(bt, label) {
  bt %>%
    group_by(YearForecast) %>%
    summarise(
      obs  = mean(mx, na.rm = TRUE),
      svd  = mean(pred_svd, na.rm = TRUE),
      pois = mean(pred_pois, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Sexo = label)
}

lvl_M <- compare_levels(bt_M, "Masculino")
lvl_F <- compare_levels(bt_F, "Feminino")
lvl_A <- compare_levels(bt_A, "Ambos")

g_lvl <- bind_rows(lvl_M, lvl_F, lvl_A) %>%
  pivot_longer(
    cols = c(obs, svd, pois),
    names_to = "Serie",
    values_to = "valor"
  ) %>%
  mutate(
    Serie = recode(
      Serie,
      obs  = "Observado",
      svd  = "Previsto LC-SVD",
      pois = "Previsto LC-Poisson"
    ),
    Serie = factor(Serie, levels = c("Observado", "Previsto LC-SVD", "Previsto LC-Poisson"))
  ) %>%
  ggplot(aes(x = YearForecast, y = valor, color = Serie, linetype = Serie, shape = Serie)) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.6) +
  facet_wrap(~Sexo, scales = "free_y") +
  scale_color_manual(values = pal_serie_prev) +
  scale_linetype_manual(values = c(
    "Observado" = "solid",
    "Previsto LC-SVD" = "22",
    "Previsto LC-Poisson" = "42"
  )) +
  scale_shape_manual(values = c(
    "Observado" = 16,
    "Previsto LC-SVD" = 17,
    "Previsto LC-Poisson" = 15
  )) +
  labs(
    title = "Nível médio observado e previsto de m[x,t]",
    x = "Ano",
    y = "Média de m[x,t]"
  ) +
  guides(linetype = "none", shape = "none") +
  tema_tcc

ggsave(
  file.path(OUT_DIR, "graficos", "05_nivel_medio_observado_previsto_splitfixo.png"),
  g_lvl,
  width = 10,
  height = 5,
  dpi = 300
)

write_delim(bt_M, file.path(OUT_DIR, "tabelas", "backtest_detalhado_M.tsv"), delim = "\t")
write_delim(bt_F, file.path(OUT_DIR, "tabelas", "backtest_detalhado_F.tsv"), delim = "\t")
write_delim(bt_A, file.path(OUT_DIR, "tabelas", "backtest_detalhado_A.tsv"), delim = "\t")

# ============================================================
# 13) Tabela final de métricas
# ============================================================

summarise_metrics <- function(bt_df, sex_label) {
  bt_df %>%
    group_by(h) %>%
    group_modify(~{
      df <- .x %>% filter(!is.na(mx), !is.na(Ex))
      m_svd  <- metricas(df, "pred_svd")  %>% mutate(Modelo = "LC-SVD")
      m_pois <- metricas(df, "pred_pois") %>% mutate(Modelo = "LC-Poisson")
      bind_rows(m_svd, m_pois)
    }) %>%
    ungroup() %>%
    mutate(Sexo = sex_label) %>%
    relocate(Sexo, h, Modelo)
}

tab_M <- summarise_metrics(bt_M, "Masculino")
tab_F <- summarise_metrics(bt_F, "Feminino")
tab_A <- summarise_metrics(bt_A, "Ambos")

tab_all <- bind_rows(tab_M, tab_F, tab_A) %>%
  arrange(Sexo, h, Modelo)

write_delim(
  tab_all,
  file.path(OUT_DIR, "tabelas", "resultado_metricas_RMSE_MAE.tsv"),
  delim = "\t"
)

# ============================================================
# 14) Heatmaps
# ============================================================

make_heatmap <- function(bt_df, sex_label, pred_col, file_name) {
  df_err <- bt_df %>%
    mutate(err = mx - .data[[pred_col]]) %>%
    group_by(YearForecast, Age) %>%
    summarise(err_mean = mean(err, na.rm = TRUE), .groups = "drop")
  
  g <- ggplot(df_err, aes(x = YearForecast, y = Age, fill = err_mean)) +
    geom_tile() +
    scale_y_reverse() +
    scale_fill_gradient2(
      low = "#3B4CC0",
      mid = "white",
      high = "#B40426",
      midpoint = 0
    ) +
    labs(
      title = paste0("Erro por idade e ano — ", sex_label),
      subtitle = ifelse(pred_col == "pred_svd", "Modelo: LC-SVD", "Modelo: LC-Poisson"),
      x = "Ano de teste",
      y = "Idade",
      fill = "Erro"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
      plot.subtitle = element_text(hjust = 0.5, size = 11, colour = "grey35"),
      axis.title = element_text(face = "bold"),
      legend.position = "right",
      panel.grid = element_blank()
    )
  
  ggsave(file.path(OUT_DIR, "graficos", file_name), g, width = 10, height = 6, dpi = 300)
  g
}

g_hm_M_svd  <- make_heatmap(bt_M, "Masculino", "pred_svd",  "06_heatmap_erro_M_LCSVD.png")
g_hm_M_pois <- make_heatmap(bt_M, "Masculino", "pred_pois", "07_heatmap_erro_M_LCPoisson.png")
g_hm_F_svd  <- make_heatmap(bt_F, "Feminino",  "pred_svd",  "06_heatmap_erro_F_LCSVD.png")
g_hm_F_pois <- make_heatmap(bt_F, "Feminino",  "pred_pois", "07_heatmap_erro_F_LCPoisson.png")
g_hm_A_svd  <- make_heatmap(bt_A, "Ambos",     "pred_svd",  "06_heatmap_erro_A_LCSVD.png")
g_hm_A_pois <- make_heatmap(bt_A, "Ambos",     "pred_pois", "07_heatmap_erro_A_LCPoisson.png")

# ============================================================
# 15) Curvas observado vs previsto
# ============================================================

plot_age_series <- function(bt_df,
                            sex_label,
                            ages_sel = c(0, 1, 5, 20, 40, 60, 80),
                            file_name = "") {
  
  df <- bt_df %>%
    filter(Age %in% ages_sel) %>%
    select(YearForecast, Age, mx, pred_svd, pred_pois) %>%
    pivot_longer(
      cols = c(mx, pred_svd, pred_pois),
      names_to = "Serie",
      values_to = "valor"
    ) %>%
    mutate(
      Serie = recode(
        Serie,
        mx = "Observado",
        pred_svd = "Previsto LC-SVD",
        pred_pois = "Previsto LC-Poisson"
      ),
      Serie = factor(
        Serie,
        levels = c("Observado", "Previsto LC-SVD", "Previsto LC-Poisson")
      )
    )
  
  g <- ggplot(df, aes(x = YearForecast, y = valor, color = Serie, linetype = Serie, shape = Serie)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.8, stroke = 0.9) +
    facet_wrap(~Age, scales = "free_y") +
    scale_color_manual(values = pal_serie_prev) +
    scale_linetype_manual(values = c(
      "Observado" = "solid",
      "Previsto LC-SVD" = "22",
      "Previsto LC-Poisson" = "42"
    )) +
    scale_shape_manual(values = c(
      "Observado" = 16,
      "Previsto LC-SVD" = 17,
      "Previsto LC-Poisson" = 15
    )) +
    scale_x_continuous(breaks = TEST_YEARS) +
    scale_y_continuous(labels = label_number()) +
    labs(
      title = paste0("Observado vs previsto — ", sex_label),
      subtitle = "Período de teste: 2018–2022",
      x = "Ano",
      y = "Taxa central de mortalidade m[x,t]"
    ) +
    guides(linetype = "none", shape = "none") +
    tema_tcc
  
  if (file_name != "") {
    ggsave(file.path(OUT_DIR, "graficos", file_name), g, width = 12, height = 6.8, dpi = 300)
  }
  
  g
}

g_series_M <- plot_age_series(bt_M, "Masculino", file_name = "08_series_mx_M_splitfixo.png")
g_series_F <- plot_age_series(bt_F, "Feminino",  file_name = "08_series_mx_F_splitfixo.png")
g_series_A <- plot_age_series(bt_A, "Ambos",     file_name = "08_series_mx_A_splitfixo.png")

# ============================================================
# 16) Ranking
# ============================================================

rank_table <- tab_all %>%
  group_by(Sexo, h) %>%
  arrange(RMSE_w) %>%
  mutate(Rank = row_number()) %>%
  ungroup()

write_delim(
  rank_table,
  file.path(OUT_DIR, "tabelas", "ranking_modelos_por_sexo_h.tsv"),
  delim = "\t"
)

# ============================================================
# 17) Checklist dos artefatos
# ============================================================

artefatos <- tibble(
  Categoria = c(
    "Base",
    "Diagnóstico",
    "Componentes",
    "Componentes",
    "Componentes",
    "Acurácia",
    "Diagnóstico",
    "Validação Visual"
  ),
  Item = c(
    "base_completa_Dx_Ex_mx_qx.tsv",
    "diagnostico_missing.tsv",
    "02_ax_MF_conjunto.png",
    "03_bx_MF_conjunto.png",
    "04_kt_MF_conjunto.png",
    "resultado_metricas_RMSE_MAE.tsv",
    "heatmaps de erro por sexo/modelo",
    "series por idade 2018-2022"
  )
)

write_delim(
  artefatos,
  file.path(OUT_DIR, "tabelas", "artefatos_gerados.tsv"),
  delim = "\t"
)

# ============================================================
# 18) Objetos finais no console
# ============================================================

tab_missing
tab_all
rank_table

g1
plots_axbx_MF$g_ax
plots_axbx_MF$g_bx
g_kt_MF
g_lvl
g_series_M
g_series_F
g_series_A