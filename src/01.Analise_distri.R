
library( tidyverse )

# Analise ENAMED

path_files <- "./data/raw/ENAMED/DADOS/Enade/"

files <- list.files( path_files )

df_enamed <-
  map( paste0( path_files, files ), read.csv, sep = ";")

# Desempenho Medio

desemp <- 
  df_enamed[[17]] %>%
  select( CO_CURSO, PROFICIENCIA, NT_GER, PER_ACERTO_ENARE )

# Detalhes do Curso (TIPO E REGIAO)

nota_curso_final <-
  mutate( desemp,
          prof_dum = if_else( NT_GER > 60, 1, 0 ) )%>%
  group_by( CO_CURSO )%>%
  summarise( perc_prof = mean(prof_dum,na.rm=TRUE) )%>%
  mutate( nota_curso = case_when( 0.0  <= perc_prof & perc_prof < 0.4  ~ "1",
                                  0.4  <= perc_prof & perc_prof < 0.6  ~ "2",
                                  0.6  <= perc_prof & perc_prof < 0.75 ~ "3",
                                  0.75 <= perc_prof & perc_prof < 0.9  ~ "4",
                                  0.9  <= perc_prof & perc_prof < 1    ~ "5" ))
  
det_curso <-
  df_enamed[[1]] %>%
  group_by( CO_CURSO, CO_CATEGAD, CO_REGIAO_CURSO )%>%
  summarise()

# Analise Da distribuição de notas por Conceito

cruzamento <-
  left_join( desemp, det_curso, by = c("CO_CURSO") )%>%
  left_join( ., nota_curso_final, by = c("CO_CURSO"))


dist_nota_conceito <-
  ggplot( filter( cruzamento, !is.na( nota_curso ) & nota_curso %in% c("1","2") ) )+
  geom_density( aes( x = NT_GER, fill = nota_curso  ), alpha = 0.4 )+
  geom_vline( xintercept = 60, linetype = 2 )+
  annotate("text", x = 81, y = 0.05, label = "Faixa de proficiência")+
  labs( x = "Nota Geral", y = "Densidade", fill = "Conceito")+
  theme_bw( )+
  theme( legend.position = "bottom",
         axis.text  = element_text( size = 12),
         axis.title = element_text( size = 12))

ggsave( filename = paste0( "./pics/dist_nota_conceito.pdf"),
        width = 1200, height = 1000, units = "px")

# Tipo de ADM

ggplot( filter( cruzamento, !is.na( nota_curso ) & nota_curso %in% c("1","2") ) )+
  geom_density( aes( x = NT_GER, fill = as.character(CO_CATEGAD), group = as.character(CO_CATEGAD)  ), 
                alpha = 0.4 )+
  geom_vline( xintercept = 60, linetype = 2 )+
  annotate("text", x = 81, y = 0.05, label = "Faixa de proficiência")+
  labs( x = "Nota Geral", y = "Densidade", fill = "Região")+
  theme_bw( )+
  theme( legend.position = "bottom",
         axis.text  = element_text( size = 12),
         axis.title = element_text( size = 12))+
  facet_wrap( ~nota_curso )


# Analise Da distribuicao de notas por TP ADM