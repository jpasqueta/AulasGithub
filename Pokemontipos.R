setwd("C:/Users/jessi/Área de Trabalho/Aula R")
dados <- read.csv("Pokemon_full.csv")
dados %>% filter(type == "grass")
df_grass <- filter(dados, type == "grass")
grepl("ap","banana")
x <- "apple"
x %>% grepl("ap",.)

df_fogo_e_agua <- dados %>% filter(type == "fire" | type == "water")
df_fly <- dados %>% filter(grepl("fly", name))

df_fogo_e_agua      

df_bee_e_saur <- dados %>% filter(type == "bee")|type == "saur")
