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

#Criar uma coluna com a transformação z-score para altura por type utilizando

dados %>% 
  group_by(type) %>% 
  mutate(
    z_height = (height-mean(height))/sd(height),
    z_height = (weight-mean(weight))/sd(weight))
  )

  library(ggplot2)

dados %>% pull(type) %>% unique

ggplot(df)+
  geom_density(aes(x = z_height, color = type))+
  theme_bw()

f <- function(x){
  if(x <= 15){ #? no caso, o valor é 300
    return("Executei essa ação")
  }else{
      return("Executei aquela ação")
  }
}

x1 <- c(30, 16, 20, 3)
f(x1)

#* O código abaixo funciona
#todo

dados %>% 
rowwise() %>% 
mutate(
  nova_var = f(height)
) %>% 
  select(height, nova_var) %>% head(30)

#? ifelse e case_when

dados %>% 
mutate(
  tamanho = ifelse(
    height < 15,
    "baixinho",
    "altão"
  )
) %>% head

dados %>% 
mutate(
  tamanho = case_when(
    height < 5 ~ "baixinho",
    height < 10 ~ "pequeno",
    height < 15 ~ "médio",
    TRUE ~ "altão"
  )
) %>% head

ff <- function(y){
  resposta <- c()
  
  for(i in 1:length(y)){
    if(y[i] <= 15){ #? no caso, o valor é 300
      resposta[i] <- "baixinho"
  }else{
    
    resposta[i] <- "altão"
  }
}

return(resposta)
}

dados %>% 
  mutate(
    tamanho = case_when(
      height < 5 ~ "baixinho",
      height < 10 ~ "pequeno",
      height < 15 ~ "médio",
      TRUE ~ NA
    )
  ) %>% head

# Juntar dados
#bind
#rbind = junta a linha
#cbind = junta a coluna

df_A = data.frame(A = c(1, 2, 3, 4),B = c(5,6,7,8))
df_B = data.frame(A = c(12, 22, 32, 42),C = c(7,5,9,8))
rbind(df_A, df_B)

df_A = data.frame(A = c(1, 2, 3, 4))
df_B = data.frame(B = c(12, 22, 32, 42))
cbind(df_A, df_B)

#outras funções

# bind_rows tudo que nao existir de colunas ele poe NA
bind_rows(df_A, df_B)


#? Vamos falar de JOIN

df_means <- dados %>% 
  group_by(type) %>% 
  summarise(
    media_h = mean(height),
    media_w = mean(weight)
  )
df_means

#? vamos excluir os grupos que começam com "g"
#Todo

df_means <- df_means %>% 
  filter(!grepl("^g",type))

df_means
# o ! do começo significa 'não tem' e o ^antes do g significa que é a inicial

#? vamos adicionar um grupo que nao existe

novo_grupo <- data.frame(
  type = "Vozes da minha cabeça",
  media_h = 1000,
  media_w = 400.82
)

#TODO adicionar o grupo
df_means <- rbind(df_means, novo_grupo)

### Boa prática

dados <- dados %>% 
  mutate_if(is.character,function(x) trimws(x,"both"))
#? full_join
#TODO

# full -> vai manter tudo de todos

df <- full_join(dados,df_means,by = "type")
View(df)

#? inner_join
#TODO

# inner -> só deixa o que tiver correspondencia em todos
df <- inner_join(dados,df_means,by = "type")
View(df)

#? left_join
#TODO

# left -> mantem o que tiver no df da esquerda que nao tiver correspondencia e exclui os da direita que nao tem
df <- left_join(dados,df_means,by = "type")
View(df)

#? right_join
#TODO

df <- right_join(dados,df_means,by = "type")
View(df)

# Sintaxe

names(dados)

df_means <- dados %>% 
  group_by(type, secundary.type) %>% 
  summarise(
    media_h = mean(height),
    media_w = mean(weight)
  )
df_means

df <- right_join(dados,df_means,by = c("type" = "type","secundary.type" = "secundary.type"))
View(df)

#? vamos adicionar um grupo que JÁ existe

novo_grupo <- data.frame(
  type = "bug",
  media_h = 10,
  media_w = 800
)
df_means <- rbind(df_means, novo_grupo)
View(df_means)

#?#############################################

library(tidyr)

#? baixado de https://livro.curso-r.com/

dados <- readr::read_rds(file = "imdb.rds")

head(dados)
names(dados)

df <- dados %>% 
  select(titulo, orcamento, receita, receita_eua)
df

# um grafico com 10 primeiros filmes
# gráfico de barras
# cada barra vem de uma coluna e aparece com uma cor diferente
# eixo x o filme

#TODO checar se cada filme tem apenas um genero associado.

#? Pivoteamento

#? pivot_wider

#? pivot_longer

df_long <- df %>% 
  slice(1:10) %>% 
tidyr::pivot_longer(2:4, values_to = "Valor", names_to = "Tipo de Valor")
View(df %>% slice(1:10))
View(df_long)

df_long

library(ggplot2)

ggplot()+
  geom_col(data = df_long, aes(x = titulo, y = Valor, fill = `Tipo de Valor`),
      position = position_dodge2()
      )+
  theme_bw()+
  theme(
axis.text.x = element_text(angle = 45, hjust = 1.0)
)

## Pivot wider

df_long %>% 
  tidyr::pivot_wider(names_from = `Tipo de Valor`, values_from = "Valor")

# correlaçao cor() - recebe uma matriz

## Tarefa para o dia 25-05

setwd("C:/Users/jessi/Área de Trabalho/AulasGitHub")
dados <- readr::read_rds(file = "imdb.rds")

Orçamento <- dados$orcamento
Receita <- dados$receita
Receita_USA <- dados$receita_eua
cbind(Orçamento, Receita, Receita_USA)

correlacao <- cor(Orçamento, Receita, Receita_USA, use = "pairwise.complete.obs")

print(correlacao)
