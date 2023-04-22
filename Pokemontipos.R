library(dplyr)
setwd("C:/Users/jessi/Área de Trabalho/AulasGitHub")
dados <- read.csv("Pokemon_full.csv")
dados %>% filter(type == "grass")
df_grass <- filter(dados, type == "grass")
grepl("ap","banana")
x <- "apple"
x %>% grepl("ap",.)

dados %>% filter(grepl("fly",name))
df_fogo_e_agua <- dados %>% filter(type == "fire" | type == "water")
df_fly <- dados %>% filter(grepl("fly", name))

dados %>% filter(grepl("bee",name)|grepl("saur",name))
dados %>% head
dados

#? a função pull devolve um vetor - crtl shif m pra aparecer %>5
dados %>%
  filter(type == "fire") %>% 
  pull(secundary.type) %>% 
  unique

dados2 <- dados[dados$type == "fire",]
unique(dados2$secundary.type)

unique(dados[dados$type == "fire",]$secundary.type)

dados %>% select(type) %>% unique
dados %>% select(type, secundary.type)
dados %>% select(type, secundary.type) %>% unique
  #? a funçao select seleciona colunas
  
  dados %>% select(c(1,2,3)) #? pelo numero
  df <- dados %>% select(name, type, height) #? pelo nome
  
  df <- dados %>% select(height, weight,hp)
  df <- as.matrix(df)
  
#? outras possibilidades
  
  dados %>% names
  dados %>% 
    select(starts_with("h")) %>% head
  dados %>% select(-name) %>% head
  
  #? a função mutate modifica ou cria uma coluna com base em outras
  
  mutate(dados,height2 = 2*height) %>% head
  
  dados <- dados %>% 
    mutate(
      height2 = 2*height,
      speed2 = 2*speed
    ) %>% head
  
  dados %>% head

  #? a funçao arrange organiza o data frame
  
  dados %>% 
    arrange(name) %>% 
    head()

  dados %>% 
    arrange(name) %>% 
    tail()
  
  dados %>% 
    arrange(desc(name)) %>% 
    head()
  
  dados %>% 
    arrange(desc(name),height)
    
    df <- data.frame(nome = c("maria","zé","joão","maria"), altura = c(2,3,4,1)
                     )
  df %>% 
    arrange(nome,altura)
  df
  
  #? Vamos fazer algumas contas
  
  dados %>% 
    group_by(type,secundary.type) %>% 
    summarise( 
      media_altura = mean(height),
      media_peso = mean(weight),
      N = n()
    ) %>% 
    arrange(media_altura,media_peso)
#filtrar os pokemons que tem o peso acima da media da altura do seu grupo
  
  dados %>% 
    group_by(type) %>% 
    mutate(
      media_altura = mean(height),
    ) -> df
  
  write.csv(df,"df.csv")
  slsx::write.xlsx(df,"df.csv")
  
  dados %>% 
    group_by(type) %>% 
    mutate(
      media_altura = mean(height)
      ) %>% 
    filter(height > media_altura) %>% select(-secundary.type,-attack, -defense)
  select(-media_altura) -> df
  write.csv(df,"df.csv")
  df

  dados %>% 
    group_by(type) %>% 
    mutate(
      media_altura = mean(height),
      media_peso = mean(weight)
    ) %>% 
    filter(height > media_altura, weight > media_peso) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(
      imc = weight/height^2,
      mm = sum(weight)
    ) -> df
df  

dados %>% 
  rowwise() %>% 
  mutate(
    nova_var = f(height)
  ) %>% 
  select(height,nova_var) %>% head(30)
df


dados %>% 
   group_by(type) %>% 
   mutate(across(where(is.numeric),scale)) 
dados



  