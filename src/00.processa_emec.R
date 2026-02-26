library( tidyverse )
library( readxl )

# Idade Curso

path <- "./data/raw/EMEC/"

file <- "EMEC_DTA.csv"

emec <- read.csv( paste0( path, file ), sep = ";")

emec_final <-
  filter( emec,
          Nome.do.Curso == "MEDICINA" &
            Data.início.funcionamento != "Não iniciado" &
            Situação == "Em Atividade" ) %>%
  mutate( dt_inicio  = as.Date( `Data.início.funcionamento`, format = "%d/%m/%Y"),
          dt_criacao = as.Date( `Data.Ato.de.Criação`, format = "%d/%m/%Y"),
          
          idade_curso = as.numeric(difftime( as.Date("2026-01-01"), dt_inicio, units = "weeks"))/52)

plot_idade_curso <-
  emec_final %>%
  mutate( cat_adm =
            case_when( Categoria.Administrativa == "Pública Federal" ~ "1.Pública\nFederal",
                       Categoria.Administrativa == "Pública Estadual" ~ "2.Pública\nEstadual",
                       Categoria.Administrativa == "Pública Municipal" ~ "3.Pública\nMunicipal",
                       Categoria.Administrativa == "Privada sem fins lucrativos" ~ "4.Privada \nsem fins\nlucrativos",
                       Categoria.Administrativa == "Privada com fins lucrativos" ~ "5.Privada \ncom fins\nlucrativos",
                       Categoria.Administrativa == "Especial" ~ "6.Especial"))%>%
  ggplot( )+
  geom_boxplot( aes( y = idade_curso, x = cat_adm ))+
  labs( x = "Categoria Adm", y = "Idade")+
#  ylim( c(0,50))+
  theme_bw()
  
path_out_pic <-
  "./pics/"

ggsave( plot = plot_idade_curso,
        filename = paste0( path_out, "plot_idade_curso.pdf"),
        units = "px", width = 1500, height = 1000 )

saveRDS( emec_final, paste0( path_out_pic, "enamed_final.rds") )
