#Eudes C. dos Anjos Filho, UFRJ
#Case para a vaga de Analista de Dados, Base dos Dados - 18-02-2022


#Importando os pacotes necessários
library(basedosdados)
library(tidyverse)
library(ggplot2)
options(scipen = 99)

#Importando os dados relativos às despesas dos candidatos no ano de 2020
set_billing_id("base-dos-dados-341622")
query <- bdplyr("basedosdados.br_tse_eleicoes.bens_candidato")
df <- read_sql(query = 'SELECT ano, sigla_uf, sigla_partido, valor_despesa
FROM `basedosdados.br_tse_eleicoes.despesas_candidato` 
WHERE ano=2020' , billing_project_id = "base-dos-dados-341622")

#Filtrando pelos 10 partidos com maior representatividade no Congresso Nacional
#Fonte:https://www.camara.leg.br/Internet/Deputado/bancada.asp

df1 <- df %>% 
  filter(sigla_partido %in% c("PSL", "PT", "PL", "PP", "PSD",
                              "MDB", "PSDB", "REPUBLICANOS", "PSB", "DEM")) %>% 
  group_by(sigla_partido) %>% 
  summarise(gastototal= sum(valor_despesa))

#Barplot
ggplot(data = df1, aes(x = reorder(sigla_partido, -gastototal), y = gastototal,
                       fill=sigla_partido)) +
  geom_col() +
  theme_bw()+
  ggtitle("Despesa dos partidos com maior bancada no Congresso Nacional")+
  scale_x_discrete(name = "Partidos")+
  scale_y_continuous(name = "Total da despesa em R$(2020)")+
  scale_fill_brewer(palette = "Spectral")+
  theme(legend.position = "none")

################################################################################


  