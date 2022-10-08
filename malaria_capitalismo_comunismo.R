
# Malária em países capitalistas e comunistas ----------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 07/10/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/malaria -------------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Malária é uma doença transmitida de pessoa para pessoa através de mosquitos infectados.
### A picada de um mosquito Anopheles infectado transmite um parasita que entra no sistema 
### sanguíneo da vítima e viaja para o fígado da pessoa onde o parasita se reproduz.
### Aí o parasita causa uma febre alta que envolve tremores e dores. Nos piores dos casos o
### parasita leva ao coma e a morte.

### Os parasitas são microorganismos unicelulares do grupo plasmodium. Plasmodium falciparum 
### é de longe o mais letal nos seres humanos e responsável pela maioria das mortes.
### O número de mortes anuais é certamente de centenas de milhares, mas as estimativas 
### diferem entre as diferentes organizações de saúde mundiais: a Organização Mundial de
### Saúde estima que 558.000 pessoas morreram por causa da malária em 2019. Institute of 
### Health Metrics and Evaluation (IHME) coloca essa estimativa em 643.000.

### A maioria das vítimas são crianças. Ela é uma das principais causas de mortalidade
### infantil. Cada duodécima criança que morreu em 2017, morreu por causa da malária.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

malaria <- read.csv("malaria-death-rates.csv")
view(malaria)
names(malaria)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

malaria <- malaria %>%
  select(-Code) %>%
  rename(taxa_morte = Deaths...Malaria...Sex..Both...Age..Age.standardized..Rate.) %>%
  view()

malaria1 <- malaria %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  group_by(Entity) %>%
  summarise(media = mean(taxa_morte),
            sd = sd(taxa_morte), n = n(),
            se = sd/sqrt(n)) %>%
  view()

malaria2 <- malaria %>%
  filter(Entity %in% c("United States", "Germany", "Japan",
                       "China", "Cuba", "North Korea")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 6)

ggplot(malaria1, aes(x = fct_reorder(Entity, media), 
                     y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                size = 0.8, width = 0.2) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  labs(x = "Países", y = "Taxa de morte por malária") +
  theme_ipsum(axis_title_size = 16, axis_text_size = 14) +
  theme(legend.position = "none", axis.text = element_text(color = "black"))

ggplot(malaria2, aes(x = Year, y = taxa_morte,
                     group = Entity, color = Entity)) +
  geom_point() +
  geom_line(1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499")) +
  labs(x = "Tempo (anos)", y = "Taxa de morte por malária") +
  theme_ipsum(axis_title_size = 16, axis_text_size = 14) +
  theme(legend.position = "none", axis.text = element_text(color = "black"))
