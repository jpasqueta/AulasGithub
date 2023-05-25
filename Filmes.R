## Tarefa para o dia 25-05
library(tidyr)

setwd("C:/Users/jessi/Área de Trabalho/AulasGitHub")
dados <- readr::read_rds(file = "imdb.rds")

Orçamento <- dados$orcamento
Receita <- dados$receita
Receita_USA <- dados$receita_eua
cbind(Orçamento, Receita, Receita_USA)

correlacao <- cor(cbind(Orçamento, Receita, Receita_USA), use = "pairwise.complete.obs")
print(correlacao)


##Outra forma de fazer a correlação

library(dplyr)

df <- dados %>% 
  select(titulo, orcamento, receita, receita_eua)
df

df %>% 
  select_if(is.numeric) %>% 
  filter(complete.cases(.)) %>% 
  cor()

## Aula dia 25-05-23

library(knitr)
---
title: "GGPLOT2"
author: "Jessica Pasqueta"
date: "2023-05-25"
output: html_document
---
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
``` 

Digitar qualquer coisa!
