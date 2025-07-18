rm(list = ls())

# cargo los paquetes  -----------------------------------------------------

library(tidyverse)
library(Epi)
library(janitor)
library(openxlsx)
library(patchwork)

Y# cargo la base  ----------------------------------------------------------



data_hb <- openxlsx::read.xlsx("base_final.xlsx")


data_hb <- clean_names(data_hb)

glimpse(data_hb)
names(data_hb)

data_hb <- data_hb[, -c(2,3)]


data_hb[437,"linf_no_hodgkin" ] <- 2

  
  


cols_to_convert <- names(data_hb)[sapply(data_hb, function(x) {
  valores <- as.character(na.omit(x))
  all(valores %in% c("1", "2"))
})]


# Imprimir las columnas que se convertirán (para verificar)
print("Columnas a convertir:")
print(cols_to_convert)

# Aplicar la transformación a las columnas identificadas:
# Convertir "1" a "sí" y "2" a "no"

data_hb <- data_hb %>% 
  mutate(
    sexo = ifelse(sexo == 1, "Femmale", "Male" )
    
  )


data_hb <- data_hb %>%
  mutate(across(all_of(cols_to_convert), ~ recode(as.character(.x), "1" = "Yes", "2" = "No")))


data_hb <- data_hb %>% 
  mutate(
    
    cirrosis_total = if_else(cirrosis > 1, "Yes", "No"),
    cirrosis = factor(cirrosis, levels = c(1,2,3,4), 
                      labels = c("sin cirrosis", "Child Pug A", "Child Pug B", "Child Pug C"))  )

data_hb$ci


data_hb <- data_hb %>% 
  mutate(
 estado_final_total = ifelse(estado_final > 1, "Alive", "Death"),
 estado_final = factor(estado_final, levels = c(1,2,3), labels = c("Death", "Alive", "Referred"))
 )

names(data_hb)

data_hb <- data_hb %>% 
  mutate(
    ifelse(
      leu_mie_cro == "Yes" |leu_linf_agu == "Yes" | neo_mielo_proli == "Yes" | sd_mielodisp == "Yes" | mielo_multi == "Yes" | mielo_quies == "Yes" | 
        linf_no_hodgkin == 1 | linf_hodgkin == "Yes" , "Yes", "No"
    )
    
  )


data_hb <- data_hb %>% 
  mutate( 
    neo_hemato = ifelse(
      leu_mie_cro == "Yes" |leu_linf_agu == "Yes" | neo_mielo_proli == "Yes" | sd_mielodisp == "Yes" | mielo_multi == "Yes" | mielo_quies == "Yes" | 
        linf_no_hodgkin == 1 | linf_hodgkin == "Yes" | linf_malt == "Yes"  , "Yes", "No"
    )
    
  )

data_hb <- data_hb %>% 
  mutate(
    neo_solida = ifelse(
     ca_gastri == "Yes" |ca_cervi_ute == "Yes" | ca_colang == "Yes" | ca_colon == "Yes" | ca_hepato == "Yes" | ca_pancre == "Yes" | 
        ca_mama == "Yes" | ca_prosta == "Yes" | ca_vejiga == "Yes"  , "Yes", "No"
    )
    
  )

data_hb <- data_hb %>% 
  mutate(
    hepatitis = ifelse(
      hepatitis > 1, "No", "Yes"
    )
    
  )


data_hb <- data_hb %>% 
  mutate(
    estan_uci = ifelse(
      estan_uci > 1, "No", "Yes"
    )
    
  )

data_hb <- data_hb %>% 
  mutate(
    cel_linfos = as.numeric(cel_linfos)
  )

data_hb <- data_hb %>% 
  mutate(
    cel_neu = as.numeric(cel_neu)
  )

data_hb %>%
  group_by(cirrosis) %>%
  summarise(
    total = n(),
    n_yes = sum(enf_renal_cro == "Yes"),
    porc_yes = n_yes / nrow(data_hb),
    .groups = "drop"
  ) %>% 
  sum(n_yes)

0.0239 + 0.00703 + 0.0127
17+ 5 + 9 

# analisis  ---------------------------------------------------------------


hemogram_vars <- c("creatinina", "cel_leucos", "cel_linfos", "cel_neu",
                   "cel_mono", "cel_eos", "plaquetas", "hemoglobina")


resumen_numericas <- function(data, variables = NULL) {
  # Si no se especifican variables, usar todas las numéricas
  if (is.null(variables)) {
    variables <- data %>% 
      select(where(is.numeric)) %>% 
      names()
  }
  
  # Calcular resumen: min, max, media, sd, Q1, Mediana, Q3
  data_hb %>% 
    select(all_of(variables)) %>% 
    pivot_longer(everything(), names_to = "Variable", values_to = "Valor") %>% 
    group_by(Variable) %>% 
    summarise(
      Min = min(Valor, na.rm = TRUE),
      Q1 = quantile(Valor, 0.25, na.rm = TRUE),
      Media = mean(Valor, na.rm = TRUE),
      Mediana = median(Valor, na.rm = TRUE),
      Q3 = quantile(Valor, 0.75, na.rm = TRUE),
      Max = max(Valor, na.rm = TRUE),
      Desv_Estandar = sd(Valor, na.rm = TRUE),
      .groups = "drop"
    )
}




resumen_cuanti <-resumen_numericas(data_hb)



write.xlsx(resumen_cuanti, "resumen_cuanti_tabla1.xlsx")


summary(hemogram_vars)


shapiro.test(data_hb$edad)

shapiro.test(data_hb$niv_b12)

shapiro.test(data_hb$creatinina)

shapiro.test(data_hb$cel_leucos)


resultados_shapiro[[1]]







summary(data_hb$niv_b12)


shapiro.test(data_hb$hemoglobina)

shapiro.test(data_hb$niv_b12)

shapiro.test(data_hb$creatinina)

shapiro.test(data_hb$cel_leucos)


glimpse(data_hb)


library(tidyverse)
library(tidytable)



# Variables categóricas ---------------------------------------------------


table(data_hb$sexo)
prop.table(table(data_hb$sexo))

table(data_hb$estado_final_total)
prop.table(table(data_hb$estado_final_total))


table(data_hb$cirrosis_total)
prop.table(table(data_hb$cirrosis_total))


table(data_hb$cirrosis_total)
prop.table(table(data_hb$cirrosis_total))


table(data_hb$neo_hemato)
prop.table(table(data_hb$neo_hemato))

table(data_hb$neo_solida)
prop.table(table(data_hb$neo_solida))


table(data_hb$hepatitis)
prop.table(table(data_hb$hepatitis))

table(data_hb$enf_renal_cro)
prop.table(table(data_hb$enf_renal_cro))


table(data_hb$les_renal_agu)
prop.table(table(data_hb$les_renal_agu))

table(data_hb$estan_uci)
prop.table(table(data_hb$estan_uci))


# seccion nueva -----------------------------------------------------------



table(data_hb$lupus_eri_sis)
prop.table(table(data_hb$lupus_eri_sis))


table(data_hb$sepsis)
prop.table(table(data_hb$sepsis))

table(data_hb$enf_still)
prop.table(table(data_hb$enf_still))

table(data_hb$vih_sida)
prop.table(table(data_hb$vih_sida))

table(data_hb$enf_conec)
prop.table(table(data_hb$enf_conec))

table(data_hb$dialisis)
prop.table(table(data_hb$dialisis))

table(data_hb$choque_sep)
prop.table(table(data_hb$choque_sep))

table(data_hb$covid_19)
prop.table(table(data_hb$covid_19))

revisar_hepatitis <- data_hb %>% 
  filter(hepatitis == "Yes" & cirrosis_total == "Yes") 


write_xlsx(resumen_no_param, "resumen_no_parametrico.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Resumen")

# Escribir los datos
writeData(wb, sheet = "Resumen", x = resumen_no_param)

# Guardar el archivo
saveWorkbook(wb, file = "resumen_no_parametrico.xlsx", overwrite = TRUE)

boxplot_b12 <- ggplot(data_hb, aes(x = "Entire population", y = niv_b12)) +
  geom_boxplot(fill = "white", color = "black") +
  theme_minimal() +
  labs(
    title = "Distribution of Vitamin B12 Levels",
    x = NULL,
    y = "Vitamin B12 level (pg/mL)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

print(boxplot_b12)
# Guardar como EPS
ggsave("boxplot_niv_b12.eps", plot = boxplot_b12, device = "eps", width = 6, height = 5, units = "in")


# síntomas ----------------------------------------------------------------

library(tidyverse)

# Seleccionar las variables de síntomas
sintomas_vars <- c("per_peso", "asten_adina", "sint_gi", "fiebre")

# Transformar a formato largo y contar los "Yes"
sintomas_plot_df <- data_hb %>%
  select(all_of(sintomas_vars)) %>%
  pivot_longer(cols = everything(), names_to = "symptom", values_to = "presence") %>%
  filter(presence == "Yes") %>%
  count(symptom, name = "count") %>%
  mutate(symptom = recode(symptom,
                          per_peso = "Weight loss",
                          asten_adina = "Fatigue",
                          sint_gi = "GI symptoms",
                          fiebre = "Fever"))

# Gráfico de barras

library(tidyverse)

# Seleccionar las variables de síntomas
sintomas_vars <- c("per_peso", "asten_adina", "sint_gi", "fiebre")

# Transformar a formato largo y contar los "Yes"
sintomas_plot_df <- data_hb %>%
  select(all_of(sintomas_vars)) %>%
  pivot_longer(cols = everything(), names_to = "symptom", values_to = "presence") %>%
  filter(presence == "Yes") %>%
  count(symptom, name = "count") %>%
  mutate(symptom = recode(symptom,
                          per_peso = "Weight loss",
                          asten_adina = "Fatigue",
                          sint_gi = "GI symptoms",
                          fiebre = "Fever"))

# Gráfico de barras


bar_sintomas <- ggplot(sintomas_plot_df, aes(x = fct_reorder(symptom, count), y = count)) +
  geom_col(fill = "steelblue", color = "black") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Frequency of Reported Symptoms in Patients with Hypervitaminosis B12",
    x = NULL,
    y = "Number of Patients"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(face = "bold")
  )





ggsave("bar_symp_b12.eps", plot = bar_sintomas, device = "eps", width = 6, height = 5, units = "in")


# graficos_por_neoplasia --------------------------------------------------

# General boxplot: comparativo entre hematológicas y sólidas
plot_general <- data_hb %>%
  pivot_longer(cols = c(neo_hemato, neo_solida), names_to = "neoplasia_tipo", values_to = "presente") %>%
  filter(presente == "Yes") %>%
  mutate(neoplasia_tipo = recode(neoplasia_tipo,
                                 neo_hemato = "Hematologic",
                                 neo_solida = "Solid")) %>%
  ggplot(aes(x = neoplasia_tipo, y = niv_b12)) +
  geom_boxplot(fill = "white", color = "black") +
  theme_minimal() +
  labs(
    title = "Vitamin B12 Levels by Neoplasia Type",
    x = NULL,
    y = "Vitamin B12 level (pg/mL)"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# neoplasias hematologica -------------------------------------------------

plot_hema_grupo <- data_hb %>%
  pivot_longer(cols = all_of(hemas), names_to = "hemo_type", values_to = "presente") %>%
  filter(presente == "Yes") %>%
  mutate(
    hemo_type = case_when(
      hemo_type %in% c("mielo_multi", "mielo_quies", "mgus") ~ "plasma_cell_disorders",
      TRUE ~ hemo_type
    ),
    hemo_type = recode(hemo_type,
                       linf_no_hodgkin = "Non-Hodgkin lymphoma",
                       linf_hodgkin = "Hodgkin lymphoma",
                       leu_mie_cro = "Chronic myeloid leukemia",
                       leu_linf_agu = "Acute lymphoblastic leukemia",
                       neo_mielo_proli = "Myeloproliferative neoplasm",
                       sd_mielodisp = "Myelodysplastic syndrome",
                       sd_hipereosi = "Hypereosinophilic syndrome",
                       plasma_cell_disorders = "Plasma cell disorders"
    )
  )

# Gráfico con boxplot
plot_hema_boxplot_grupo <- ggplot(plot_hema_grupo, aes(x = fct_infreq(hemo_type), y = niv_b12)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(
    title = "Vitamin B12 Levels in Hematologic Neoplasms",
    x = NULL,
    y = "Vitamin B12 level (pg/mL)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


print(plot_hema_boxplot_grupo)



# neoplasias_solidas ------------------------------------------------------

solidas <- c("ca_gastri", "ca_colon", "ca_hepato", "ca_colang", 
             "ca_pancre", "ca_mama", "ca_prosta", "ca_cervi_ute", "ca_vejiga")

# Traducciones
nombres_ingles_solidas <- c(
  ca_gastri = "Gastric cancer",
  ca_colon = "Colon cancer",
  ca_hepato = "Hepatocellular carcinoma",
  ca_colang = "Cholangiocarcinoma",
  ca_pancre = "Pancreatic cancer",
  ca_mama = "Breast cancer",
  ca_prosta = "Prostate cancer",
  ca_cervi_ute = "Cervical/uterine cancer",
  ca_vejiga = "Bladder cancer"
)

# Gráfico
plot_solida_boxplot <- data_hb %>%
  pivot_longer(cols = all_of(solidas), names_to = "solid_type", values_to = "presente") %>%
  filter(presente == "Yes") %>%
  mutate(solid_type = recode(solid_type, !!!nombres_ingles_solidas)) %>%
  ggplot(aes(x = fct_infreq(solid_type), y = niv_b12)) +
  geom_boxplot(fill = "lightcoral", color = "black") +
  theme_minimal() +
  labs(
    title = "Vitamin B12 Levels in Solid Neoplasms",
    x = NULL,
    y = "Vitamin B12 level (pg/mL)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

print(plot_solida_boxplot)


# unir graficas -----------------------------------------------------------

figura_ABC <- plot_general / plot_solida_boxplot / plot_hema_boxplot_grupo +
  plot_layout(ncol = 1, heights = c(1, 1.3, 1.3)) +
  plot_annotation(tag_levels = 'A')  # Etiquetas automáticas A, B, C

# Mostrar
print(figura_ABC)

ggsave(
  filename = "figura_ABC_neoplasias.eps",
  plot = figura_ABC,
  device = "eps",
  dpi = 600,                  # Alta resolución
  width = 10,                 # Ancho en pulgadas
  height = 14,                # Altura aumentada para evitar solapamiento
  units = "in"
)


ggsave(
  filename = "figura_ABC_neoplasias_p.png",
  plot = figura_ABC,
  device = "png",
  dpi = 600,                  # Alta resolución
  width = 10,                 # Ancho en pulgadas
  height = 14,                # Altura aumentada para evitar solapamiento
  units = "in"
)


# datos de resumen para cancer  -------------------------------------------



vars_ca <- names(data_hb) %>% 
  str_subset("^ca_")


glimpse(data_hb)

# resmen_ neoplasias ------------------------------------------------------



vars_sino <- c(
  "leu_mie_cro", "leu_linf_agu", "neo_mielo_proli", 
  "sd_mielodisp", "sd_hipereosi", "mielo_multi", 
  "mielo_quies", "mgus", "linf_hodgkin"
)

resumen_sino <- map_dfr(
  vars_sino,
  ~ data_hb %>%
    filter(.data[[.x]] == "sí") %>%
    summarise(
      variable = .x,
      n = sum(!is.na(niv_b12)),
      mediana_b12 = median(niv_b12, na.rm = TRUE),
      p25_b12 = quantile(niv_b12, 0.25, na.rm = TRUE),
      p75_b12 = quantile(niv_b12, 0.75, na.rm = TRUE)
    )
)

# Parte 2: linf_no_hodgkin (suponiendo 1 = sí)
resumen_linf <- data_hb %>%
  filter(linf_no_hodgkin == 1) %>%
  summarise(
    variable = "linf_no_hodgkin",
    n = sum(!is.na(niv_b12)),
    mediana_b12 = median(niv_b12, na.rm = TRUE),
    p25_b12 = quantile(niv_b12, 0.25, na.rm = TRUE),
    p75_b12 = quantile(niv_b12, 0.75, na.rm = TRUE)
  )

# Unir ambos resultados
resumen_total <- bind_rows(resumen_sino, resumen_linf)

# Exportar a Excel con openxlsx
wb <- createWorkbook()
addWorksheet(wb, "B12 por hemato-onco")
writeData(wb, sheet = "B12 por hemato-onco", x = resumen_total)
saveWorkbook(wb, file = "resumen_b12_hemato_onco.xlsx", overwrite = TRUE)



# neoplasias solidas versus hemato ----------------------------------------

vars_neoplasias <- c(
  "leu_mie_cro", "leu_linf_agu", "neo_mielo_proli", 
  "sd_mielodisp", "sd_hipereosi", "mielo_multi", 
  "mielo_quies", "mgus", "linf_hodgkin"
)
vars_ca_solido <- names(data_hb) %>% str_subset("^ca_")

data_hb <- data_hb %>%
  mutate(
    cancer_solido = if_else(rowSums(across(all_of(vars_ca_solido), ~ .x == "sí")) > 0, TRUE, FALSE),
    neoplasia_hema = if_else(rowSums(across(all_of(vars_neoplasias), ~ .x == "sí")) > 0, TRUE, FALSE),
    grupo_neoplasia = case_when(
      cancer_solido ~ "Cáncer sólido",
      neoplasia_hema ~ "Neoplasia hematológica",
      TRUE ~ "Sin neoplasia"
    )
  )


ggplot(data_hb, aes(x = grupo_neoplasia, y = niv_b12)) +
  geom_boxplot(fill = "white", color = "black") +
  theme_minimal() +
  labs(
    title = "Distribución de niveles de B12 por grupo de neoplasia",
    x = "Grupo de neoplasia",
    y = "Nivel de B12"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


library(tidyverse)

# Paso 1: Definir las variables
vars_ca_solido <- names(data_hb) %>% str_subset("^ca_")
vars_neoplasias <- c(
  "leu_mie_cro", "leu_linf_agu", "neo_mielo_proli", 
  "sd_mielodisp", "sd_hipereosi", "mielo_multi", 
  "mielo_quies", "mgus", "linf_hodgkin"
)

# Paso 2: Crear la variable agrupadora
data_hb <- data_hb %>%
  mutate(
    cancer_solido = if_else(rowSums(across(all_of(vars_ca_solido), ~ .x == "sí")) > 0, TRUE, FALSE),
    neoplasia_hema = if_else(rowSums(across(all_of(vars_neoplasias), ~ .x == "sí")) > 0, TRUE, FALSE),
    grupo_neoplasia = case_when(
      cancer_solido ~ "Cáncer sólido",
      neoplasia_hema ~ "Neoplasia hematológica",
      TRUE ~ "Sin neoplasia"
    )
  )

# Paso 3: Crear el gráfico
boxplot_neoplasia <- ggplot(data_hb, aes(x = grupo_neoplasia, y = niv_b12)) +
  geom_boxplot(fill = "white", color = "black") +
  theme_minimal() +
  labs(
    title = "Distribución de niveles de B12 por grupo de neoplasia",
    x = "Grupo de neoplasia",
    y = "Nivel de B12"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Paso 4: Guardar como EPS
ggsave("niv_b12_por_neoplasia.eps", plot = boxplot_neoplasia, device = "eps", width = 6, height = 5, units = "in")


# cirrosis  ---------------------------------------------------------------

resumen_cirrosis <- data_hb %>%
  filter(!is.na(cirrosis), !is.na(niv_b12)) %>%
  summarise(
    n = .N,
    mediana = median(niv_b12, na.rm = TRUE),
    p25 = quantile(niv_b12, 0.25, na.rm = TRUE),
    p75 = quantile(niv_b12, 0.75, na.rm = TRUE),
    .by = cirrosis
  ) %>%
  arrange(cirrosis)


data_hb$cirrosis

resumen_cirrosis <- data_hb %>%
  filter(!is.na(cirrosis), !is.na(niv_b12)) %>%
  group_by(cirrosis) %>%
  summarise(
    n = n(),
    mediana = median(niv_b12, na.rm = TRUE),
    p25 = quantile(niv_b12, 0.25, na.rm = TRUE),
    p75 = quantile(niv_b12, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
wb <- createWorkbook()
addWorksheet(wb, "Resumen B12 Cirrosis")
writeData(wb, sheet = "Resumen B12 Cirrosis", x = resumen_cirrosis)
saveWorkbook(wb, file = "resumen_b12_por_cirrosis.xlsx", overwrite = TRUE)


boxplot_cirrosis <- ggplot(data_hb, aes(x = cirrosis, y = niv_b12)) +
  geom_boxplot(fill = "white", color = "black") +
 
  theme_minimal() +
  labs(
    title = "Distribución de niveles de B12 según clasificación de cirrosis",
    x = "Clasificación de cirrosis (Child-Pugh)",
    y = "Nivel de B12"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

medianas <- data_hb %>%
  filter(!is.na(cirrosis), !is.na(niv_b12)) %>%
  group_by(cirrosis) %>%
  summarise(Mediana = median(niv_b12, na.rm = TRUE), .groups = "drop")

ggsave("niv_b12_por_cirrosis.eps", plot = boxplot_cirrosis, device = "eps", width = 6, height = 5, units = "in")

glimpse(data_hb$cirrosis)


# enfermedades crónicas ---------------------------------------------------

library(tidyverse)
library(patchwork)

# ---------------- A. CKD vs Cirrosis vs ambos ----------------
plot_A <- data_hb %>%
  mutate(grupo = case_when(
    enf_renal_cro == "Yes" & cirrosis_total == "Yes" ~ "CKD + Cirrhosis",
    enf_renal_cro == "Yes" & cirrosis_total != "Yes" ~ "CKD only",
    enf_renal_cro != "Yes" & cirrosis_total == "Yes" ~ "Cirrhosis only",
    TRUE ~ "Neither"
  )) %>%
  filter(grupo != "Neither") %>%
  ggplot(aes(x = grupo, y = niv_b12)) +
  geom_boxplot(fill = "white", color = "black") +
  theme_minimal() +
  labs(
    title = "A. Vitamin B12 Levels: CKD, Cirrhosis and Both",
    x = NULL,
    y = "Vitamin B12 level (pg/mL)"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# ---------------- B. CKD: Dialysis vs No Dialysis ----------------
plot_B <- data_hb %>%
  filter(enf_renal_cro == "Yes") %>%
  mutate(dialysis_status = ifelse(dialisis == "Yes", "On dialysis", "No dialysis")) %>%
  ggplot(aes(x = dialysis_status, y = niv_b12)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(
    title = "B. Vitamin B12 Levels in CKD Patients by Dialysis Status",
    x = NULL,
    y = "Vitamin B12 level (pg/mL)"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# ---------------- C. Cirrhosis by Child-Pugh ----------------
# Asegúrate que haya una variable tipo `child_pugh` con valores "A", "B", "C"
# Si no existe, adapta esta parte

plot_C <- data_hb %>%
  filter(cirrosis_total == "Yes", !is.na(cirrosis)) %>%  # Reemplaza 'cirrosis' por tu variable Child-Pugh si aplica
  ggplot(aes(x = cirrosis, y = niv_b12)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(
    title = "C. Vitamin B12 Levels by Child-Pugh Class",
    x = "Child-Pugh classification",
    y = "Vitamin B12 level (pg/mL)"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# ---------------- D. CKD vs AKI ----------------
plot_D <- data_hb %>%
  mutate(grupo_renal = case_when(
    enf_renal_cro == "Yes" & les_renal_agu == "Yes" ~ "CKD + AKI",
    enf_renal_cro == "Yes" & les_renal_agu != "Yes" ~ "CKD only",
    enf_renal_cro != "Yes" & les_renal_agu == "Yes" ~ "AKI only",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(grupo_renal)) %>%
  ggplot(aes(x = grupo_renal, y = niv_b12)) +
  geom_boxplot(fill = "lightcoral", color = "black") +
  theme_minimal() +
  labs(
    title = "D. Vitamin B12 Levels: CKD vs Acute Kidney Injury",
    x = NULL,
    y = "Vitamin B12 level (pg/mL)"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# ---------------- Figura compuesta A–D ----------------
figura_b12_ckd_cirrosis <- plot_A / plot_B / plot_C / plot_D +
  plot_layout(ncol = 1, heights = c(1.2, 1.1, 1.1, 1.2)) +
  plot_annotation(tag_levels = 'A')

# Mostrar
print(figura_b12_ckd_cirrosis)

ggsave(
  filename = "figura_b12_ckd_cirrosis.eps",
  plot = figura_b12_ckd_cirrosis,
  device = "eps",
  dpi = 600,
  width = 10,
  height = 16,
  units = "in"
)

ggsave(
  filename = "figura_b12_ckd_cirrosis_p.png",
  plot = figura_b12_ckd_cirrosis,
  device = "png",
  dpi = 600,
  width = 10,
  height = 16,
  units = "in"
)


# narrativo  --------------------------------------------------------------

clinicas <- c("cirrosis_total", "cirrosis", "enf_renal_cro", 
              "dialisis", "les_renal_agu", "choque_sep", "sepsis")
hemas <- c("linf_hodgkin", "linf_no_hodgkin", "leu_mie_cro", "leu_linf_agu",
           "neo_mielo_proli", "sd_mielodisp", "sd_hipereosi", 
           "mielo_multi", "mielo_quies", "mgus")
solidas <- c("ca_gastri", "ca_colon", "ca_hepato", "ca_colang", 
             "ca_pancre", "ca_mama", "ca_prosta", "ca_cervi_ute", "ca_vejiga")

vars <- c(clinicas, hemas, solidas)

# Función para resumen estadístico por grupo
resumen_b12 <- map_dfr(vars, function(var) {
  data_hb %>%
    filter(!is.na(.data[[var]]), !is.na(niv_b12)) %>%
    group_by(value = .data[[var]]) %>%
    summarise(
      Variable = var,
      n = n(),
      Median = median(niv_b12, na.rm = TRUE),
      Mean = mean(niv_b12, na.rm = TRUE),
      Q1 = quantile(niv_b12, 0.25, na.rm = TRUE),
      Q3 = quantile(niv_b12, 0.75, na.rm = TRUE),
      Min = min(niv_b12, na.rm = TRUE),
      Max = max(niv_b12, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    relocate(Variable, .before = value)
})
write_csv(resumen_b12, "resumen_b12_por_grupo.csv")
