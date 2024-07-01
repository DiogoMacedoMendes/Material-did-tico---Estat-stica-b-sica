summary(horta)

shapiro.test(hortas$Medidas1)

shapiro.test(hortas$Medidas2)

shapiro.test(hortas$Medidas1A)

shapiro.test(hortas$Medidas2A)

shapiro.test(hortas$Medidas3)

shapiro.test(hortas$Medidas3A)

shapiro.test(hortas$Medidas4)

shapiro.test(hortas$Medidas4A)

shapiro.test(hortas$Medidas5)

shapiro.test(hortas$Medidas5A)


#Teste de variâncias

teste_de_variância1 = fligner.test(hortas$Medidas1, hortas$Medidas1A)

teste_de_variância2 = fligner.test(hortas$Medidas2, hortas$Medidas2A)

teste_de_variância3 = fligner.test(hortas$Medidas3, hortas$Medidas3A)

teste_de_variância4 = fligner.test(hortas$Medidas4, hortas$Medidas4A)

teste_de_variância5 = fligner.test(hortas$Medidas5, hortas$Medidas5A)



#p valor > 0.5 (Variâncias são significativamente iguais)

#test-t

test_t = t.test(hortas$Medidas1, hortas$Medidas1A, var.equal = TRUE)

test_t2 = t.test(hortas$Medidas2, hortas$Medidas2A, var.equal = TRUE)

test_t3 = t.test(hortas$Medidas3, hortas$Medidas3A, var.equal = TRUE)

Stest_t4 = t.test(hortas$Medidas4, hortas$Medidas4A, var.equal = TRUE)

test_t5 = t.test(hortas$Medidas5, hortas$Medidas5A, var.equal = TRUE)


# teste de variância pareado ar livre

teste_de_variância1p = fligner.test(hortas$Medidas1, hortas$Medidas2)

teste_de_variância2p = fligner.test(hortas$Medidas2, hortas$Medidas3)

teste_de_variância3p = fligner.test(hortas$Medidas3, hortas$Medidas4)

teste_de_variância4p = fligner.test(hortas$Medidas4, hortas$Medidas5)


        # test-t pareado ar livre

test_tp = t.test(hortas$Medidas1, hortas$Medidas2, var.equal = TRUE, paired = TRUE)
test_t2p = t.test(hortas$Medidas2, hortas$Medidas3, var.equal = TRUE, paired = TRUE)
test_t3p = t.test(hortas$Medidas3, hortas$Medidas4, var.equal = TRUE, paired = TRUE)
test_t4p = t.test(hortas$Medidas4, hortas$Medidas5, var.equal = TRUE, paired = TRUE)


# teste de variância pareado aquaponia

teste_de_variância1pA = fligner.test(hortas$Medidas1A, hortas$Medidas2A)

teste_de_variância2pA = fligner.test(hortas$Medidas2A, hortas$Medidas3A)

teste_de_variância3pA = fligner.test(hortas$Medidas3A, hortas$Medidas4A)

teste_de_variância4pA = fligner.test(hortas$Medidas4A, hortas$Medidas5A)


      # # test-t pareado aquaponia

test_tpA = t.test(hortas$Medidas1A, hortas$Medidas2A, var.equal = TRUE, paired = TRUE)
test_t2pA = t.test(hortas$Medidas2A, hortas$Medidas3A, var.equal = TRUE, paired = TRUE)
test_t3pA = t.test(hortas$Medidas3A, hortas$Medidas4A, var.equal = TRUE, paired = TRUE)
test_t4pA = t.test(hortas$Medidas4A, hortas$Medidas5A, var.equal = TRUE, paired = TRUE)





# Elaboração gráfica

# Semana 1

Semana1 <- data.frame(
  Grupo = rep(c("Semana1", "Semana1A"), each = nrow(hortas)),
  Valor = c(hortas$Medidas1, hortas$Medidas1A)
)

install.packages("ggplot2")
library(ggplot2)


        # Gráfico de barras
ggplot(Semana1, aes(x = Grupo, y = Valor, fill = Grupo)) + # Chama a tabela "Semana1", definindo o eixo "x" sendo a coluna "Grupo" e o eixo "y" a coluna Valor, além de dizer para organizar pelo "Grupo" 
  geom_bar(stat = "summary", position = "dodge", fun = "mean", width = 0.7, color = "black") + # Comando que faz um gráfico de barras, definindo que será pela média, definindo a cor da borda da barra e seu tamanho
  labs(title = "Comparação de crescimento da horta ao ar livre e aquaponia (A) - Semana 1",
       y = "Tamanho (cm)", fill = "Legenda", x = "") + # Define título, nome dos eixos e da legenda.
  scale_fill_brewer(palette = "Reds") + # Define a cor em paletas.
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + # Ajusta a barra no eixo x
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.5) + # Coloca a barra de erro no gráfico
  theme_classic() # Define o plano de fundo do gráfico


         # boxplot

ggplot(Semana1, aes(x = Grupo, y = Valor, fill = Grupo)) +
  geom_boxplot() +
  labs(title = "Comparação de crescimento da horta ao ar livre e aquaponia (A) - Semana 1 ",
       x = "", y = "Tamanho (cm)", fill ="Legenda") +
  scale_fill_brewer(palette = "Reds") +
  theme_minimal()



# Semana 2

Semana2 <- data.frame(
  Grupo = rep(c("Semana2", "Semana2A"), each = nrow(hortas)),
  Valor = c(hortas$Medidas2, hortas$Medidas2A)
)


      # Gráfico de barras
ggplot(Semana2, aes(x = Grupo, y = Valor, fill = Grupo)) +
  geom_bar(stat = "summary", position = "dodge", fun = "mean", width = 0.7, color = "black") +
  labs(title = "Comparação de crescimento da horta ao ar livre e aquaponia (A) - Semana 2",
       y = "Tamanho (cm)", fill = "Legenda", x = "") +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.5) +
  theme_classic()


      # Box plot

ggplot(Semana2, aes(x = Grupo, y = Valor, fill = Grupo)) +
  geom_boxplot() +
  labs(title = "Comparação de crescimento da horta ao ar livre e aquaponia (A) - Semana 2 ",
       x = "", y = "Tamanho (cm)", fill ="Legenda") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal()


#Semana3



Semana3 <- data.frame(
  Grupo = rep(c("Semana3", "Semana3A"), each = nrow(hortas)),
  Valor = c(hortas$Medidas3, hortas$Medidas3A)
)


# Gráfico de barras
ggplot(Semana3, aes(x = Grupo, y = Valor, fill = Grupo)) +
  geom_bar(stat = "summary", position = "dodge", fun = "mean", width = 0.7, color = "black") +
  labs(title = "Comparação de crescimento da horta ao ar livre e aquaponia (A) - Semana 3",
       y = "Tamanho (cm)", fill = "Legenda", x = "") +
  scale_fill_brewer(palette = "Purples") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.5) +
  theme_classic()


# Box plot

ggplot(Semana3, aes(x = Grupo, y = Valor, fill = Grupo)) +
  geom_boxplot() +
  labs(title = "Comparação de crescimento da horta ao ar livre e aquaponia (A) - Semana 3 ",
       x = "", y = "Tamanho (cm)", fill ="Legenda") +
  scale_fill_brewer(palette = "Purples") +
  theme_minimal()



# Gráfico de barras todas as semanas

library(tidyverse)

  dados <- hortas %>%
    pivot_longer(cols = everything(),
                 names_to = "Semana",
                 values_to = "Tamanho") %>%
    mutate(
      Tratamento = ifelse(str_detect(Semana, "A$"), "Aquaponia", "Horta ao Ar Livre"),
      Semana = str_remove(Semana, "A"),
      Semana = case_when(
        Semana == "Medidas1" ~ "Semana 1",
        Semana == "Medidas2" ~ "Semana 2",
        Semana == "Medidas3" ~ "Semana 3",
        Semana == "Medidas4" ~ "Semana 4",
        Semana == "Medidas5" ~ "Semana 5"
      )
    )
  
  dados$Tratamento <- factor(dados$Tratamento, levels = c("Horta ao Ar Livre", "Aquaponia"))
  
  
  
  ggplot(dados, aes(x = Semana, y = Tamanho, fill = Tratamento)) +
    geom_bar(stat = "summary", position = "dodge", fun = "mean", color = "black") +
    labs(title = "Diferença no Tamanho das Folhas de Alface entre Ar livre e Aquaponia por Semana",
         x = "",
         y = "Folha de alface (cm)",
         fill = "Tratamento") +
    geom_errorbar(stat = "summary", position = position_dodge(0.9), fun.data = mean_sdl, fun.args = list(mult = 1), width = 0.25) +
    theme_classic() +
  scale_fill_manual(values = c("green", "#0077BE")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))





# Perfil de média

ggplot(dados, aes(x=Semana, y=Tamanho, group=Tratamento, fill=Tratamento)) + 
  stat_summary(fun = mean, geom = "line", aes(linetype = Tratamento)) + # linha
  stat_summary(fun = mean, geom = "point", aes(shape = Tratamento)) + # pontos
  theme_classic() +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", width = 0.2) + # barra de erro
  labs(title = "Perfil da Média de Crescimento em Centímetros das Folhas de Alface por Semana",
       x = "Semanas",
       y = "Folhas de Alface (cm)",
       color = "Tratamento",
       linetype = "Tratamento",
       shape = "Tratamento")




#Modelagem

require(tidyverse)

dados_horta <- dados %>% filter(Tratamento == "Ar livre")
dados_aquaponia <- dados %>% filter(Tratamento == "Aquaponia")


A_inicial_horta <- max(dados_horta$Tamanho)  # Máximo observado para horta
A_inicial_aquaponia<- max(dados_aquaponia$Tamanho)  # Máximo observado para aquaponia
b_inicial <- 1
c_inicial <- 0.1

modelo_horta <- nls(Tamanho ~ A * exp(-b * exp(-c * Tempo)), 
                    data = dados_horta, 
                    start = list(A = A_inicial_horta, b = b_inicial, c = c_inicial))

# Ajuste do modelo de Gompertz para aquaponia
modelo_aquaponia <- nls(Tamanho ~ A * exp(-b * exp(-c * Tempo)), 
                        data = dados_aquaponia, 
                        start = list(A = A_inicial_aquaponia, b = b_inicial, c = c_inicial))

# Resumo dos modelos ajustados
summary(modelo_horta)
summary(modelo_aquaponia)


# Predições dos modelos ajustados
dados_horta$pred <- predict(modelo_horta, newdata = dados_horta)
dados_aquaponia$pred <- predict(modelo_aquaponia, newdata = dados_aquaponia)

# Combinar os dados preditos em um único dataframe
dados_pred <- rbind(dados_horta, dados_aquaponia)

# Plot dos dados e dos modelos ajustados
ggplot(dados_pred, aes(x = Tempo, y = Tamanho, color = Tratamento)) +
  geom_point() + # Dados observados
  geom_line(aes(y = pred), linetype = "dashed") + # Modelos ajustados
  labs(title = "Comparação do Crescimento dos Alfaces", x = "Semanas", y = "Crescimento em centímetros ") +
  theme_minimal()






                