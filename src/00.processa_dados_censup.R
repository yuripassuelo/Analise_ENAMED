
library( tidyverse )
library( geobr )
library( sf )

# Leitura de dados de Curso no CENSUP

path_censup <-
  "./data/raw/CENSUP/"

ano_ini <- 2009

func_read_dta_curso <-
  function( ano ){
  
  if( ano < 2019 ){
    comp    <- "/DADOS/"
  } else{
    comp    <- "/dados/"
  }
  # Strring de Separacao
  if( ano < 2020 ){
    str_sep <- "|"
  } else{
    str_sep <- ";"
  }
    
  # Origem dos Dados
  path_dta <-
    paste0( path_censup, "/", ano, comp )
  
  # Lista arquivos com nome CURSO
  files_list <-
    list.files( path_dta )
  file_curso <- files_list[ grepl( "CURSO", files_list ) & !grepl( "rar", files_list ) & !grepl( "zip", files_list ) ]
  print( file_curso )
  
  # Lendo arquivo CSV
  arquivo <- read.csv( paste0( path_dta, file_curso ), sep = str_sep, encoding = "utf8" )
  # Retorna Arquivo
  
  return( arquivo )
  
  }


list_files <- 
  map( 2009:2024, func_read_dta_curso )

list_med <- 
  list_files %>%
  map( ., names )
  #map( ., .f = function( x ){ filter(x , NO_CURSO == "MEDICINA")})

vec_ano <- 2009:2024

filtra_resume <- function( .i ){
  
  print( .i )
  .dta <- filter( list_files[[.i]], NO_CURSO %in% c( "MEDICINA", "Medicina" ) )
  
  if( .i == 1 ){
    return(
      select( .dta, CO_IES, CO_CURSO, CO_CATEGORIA_ADMINISTRATIVA, QT_MATRICULA_CURSO, QT_CONCLUINTE_CURSO, QT_VAGAS_INTEGRAL, QT_VAGAS_MATUTINO, QT_VAGAS_NOTURNO, QT_VAGAS_VESPERTINO )%>%
        mutate( ano = vec_ano[.i],
                vagas = QT_VAGAS_INTEGRAL + QT_VAGAS_MATUTINO + QT_VAGAS_NOTURNO + QT_VAGAS_VESPERTINO )%>%
        rename( tp_adm     = CO_CATEGORIA_ADMINISTRATIVA,
                concluinte = QT_CONCLUINTE_CURSO ,
                matricula  = QT_MATRICULA_CURSO )%>%
        select( ano, CO_IES, CO_CURSO, tp_adm, vagas, concluinte, matricula ) )
  } 
  if( .i > 1 & .i < 9 ){
    
    if( .i >= 2 & .i <=4 ){
      return(
        select( .dta, CO_IES, CO_CURSO, CO_CATEGORIA_ADMINISTRATIVA, QT_MATRICULA_CURSO, QT_CONCLUINTE_CURSO, QT_VAGAS_INTEGRAL_PRES, QT_VAGAS_MATUTINO_PRES, QT_VAGAS_NOTURNO_PRES, QT_VAGAS_VESPERTINO_PRES )%>%
          mutate( ano = vec_ano[.i],
                  across( .cols = contains("VAGAS"),
                          .fns  = ~replace(.,is.na(.),0)),
                  vagas = QT_VAGAS_INTEGRAL_PRES + QT_VAGAS_MATUTINO_PRES + QT_VAGAS_NOTURNO_PRES + QT_VAGAS_VESPERTINO_PRES )%>%
          rename( tp_adm     = CO_CATEGORIA_ADMINISTRATIVA,
                  concluinte = QT_CONCLUINTE_CURSO ,
                  matricula  = QT_MATRICULA_CURSO )%>%
          select( ano, CO_IES, CO_CURSO, tp_adm, vagas, concluinte, matricula ) )
    }
    if( .i == 5 ){
      return(
        select( .dta, CO_IES, CO_CURSO, CO_CATEGORIA_ADMINISTRATIVA, QT_MATRICULA_CURSO, QT_CONCLUINTE_CURSO, QT_VAGAS_PRINCIPAL_INTEGRAL, QT_VAGAS_PRINCIPAL_MATUTINO, QT_VAGAS_PRINCIPAL_NOTURNO, QT_VAGAS_PRINCIPAL_VESPERTINO )%>%
          mutate( ano = vec_ano[.i],
                  across( .cols = contains("VAGAS"),
                          .fns  = ~replace(.,is.na(.),0)),
                  vagas = QT_VAGAS_PRINCIPAL_INTEGRAL + QT_VAGAS_PRINCIPAL_MATUTINO + QT_VAGAS_PRINCIPAL_NOTURNO + QT_VAGAS_PRINCIPAL_VESPERTINO )%>%
          rename( tp_adm     = CO_CATEGORIA_ADMINISTRATIVA,
                  concluinte = QT_CONCLUINTE_CURSO ,
                  matricula  = QT_MATRICULA_CURSO )%>%
          select( ano, CO_IES, CO_CURSO, tp_adm, vagas, concluinte, matricula ) )
    }
    if( .i == 6 ){
      return(
        select( .dta, CO_IES, CO_CURSO, CO_CATEGORIA_ADMINISTRATIVA, QT_MATRICULA_CURSO, QT_CONCLUINTE_CURSO, QT_INGRESSO_VAGAS_NOVAS )%>%
          mutate( ano = vec_ano[.i],
                  tp_adm     = CO_CATEGORIA_ADMINISTRATIVA,
                  vagas      = QT_INGRESSO_VAGAS_NOVAS,
                  concluinte = QT_CONCLUINTE_CURSO ,
                  matricula  = QT_MATRICULA_CURSO )%>%
          select( ano, CO_IES, CO_CURSO, tp_adm, vagas, concluinte, matricula ) )
      
    }
    if( .i == 7 ){
      return(
        select( .dta, CO_IES, CO_CURSO, CO_CATEGORIA_ADMINISTRATIVA, QT_MATRICULA_CURSO, QT_CONCLUINTE_CURSO, QT_INGRESSO_VAGAS_NOVAS )%>%
          mutate( ano = vec_ano[.i],
                  tp_adm     = CO_CATEGORIA_ADMINISTRATIVA,
                  vagas      = QT_INGRESSO_VAGAS_NOVAS,
                  concluinte = QT_CONCLUINTE_CURSO ,
                  matricula  = QT_MATRICULA_CURSO )%>%
          select( ano, CO_IES, CO_CURSO, tp_adm, vagas, concluinte, matricula ) )
      
    }
    if( .i == 8 ){
      return(
        select( .dta, CO_IES, CO_CURSO, CO_CATEGORIA_ADMINISTRATIVA, QT_MATRICULA_CURSO, QT_CONCLUINTE_CURSO, QT_VAGAS_TOTAIS )%>%
          mutate( ano = vec_ano[.i] )%>%
          rename( tp_adm     = CO_CATEGORIA_ADMINISTRATIVA,
                  vagas      = QT_VAGAS_TOTAIS,
                  concluinte = QT_CONCLUINTE_CURSO ,
                  matricula  = QT_MATRICULA_CURSO )%>%
          select( ano, CO_IES, CO_CURSO, tp_adm, vagas, concluinte, matricula ) )
    }
    
    
  } 
  if( .i >= 9 & .i <= 11 ){
    return(
        select( .dta, CO_IES, CO_CURSO, TP_CATEGORIA_ADMINISTRATIVA, QT_MATRICULA_TOTAL, QT_CONCLUINTE_TOTAL, QT_VAGA_TOTAL )%>%
          mutate( ano = vec_ano[.i] )%>%
          rename( tp_adm     = TP_CATEGORIA_ADMINISTRATIVA,
                  concluinte = QT_CONCLUINTE_TOTAL ,
                  matricula  = QT_MATRICULA_TOTAL,
                  vagas      = QT_VAGA_TOTAL)%>%
          select( ano, CO_IES, CO_CURSO, tp_adm, vagas, concluinte, matricula ) )
  } 
  else{
    return(
      select( .dta, CO_IES, CO_CURSO, TP_CATEGORIA_ADMINISTRATIVA, QT_MAT, QT_CONC, QT_VG_TOTAL )%>%
        mutate( ano = vec_ano[.i] )%>%
        rename( tp_adm     = TP_CATEGORIA_ADMINISTRATIVA,
                concluinte = QT_CONC ,
                matricula  = QT_MAT,
                vagas      = QT_VG_TOTAL)%>%
        select( ano, CO_IES, CO_CURSO, tp_adm, vagas, concluinte, matricula ) )
  }
}



med <- 1:length(list_files ) %>%
  map( ., .f = filtra_resume )

dados_gerais_curso_med <- 
  bind_rows( med ) %>%
  group_by( ano, tp_adm ) %>%
  summarise( n = n(),
             across( .cols = c("vagas","concluinte","matricula"),
                     .fns  = ~sum(.,na.rm=TRUE )) )%>%
  pivot_longer( cols = !c("ano","tp_adm"),
                names_to = "var",
                values_to = "val")

bind_rows( med ) %>%
  group_by( ano, tp_adm ) %>%
  summarise()%>% View()

dados_desc <-
  mutate( filter( dados_gerais_curso_med, ano > 2009 ),
          `Categoria Adm` = case_when( tp_adm == 1 ~ "1. Federal",
                                tp_adm == 2 ~ "2. Estadual",
                                tp_adm == 3 ~ "3. Municipal",
                                tp_adm == 4 ~ "4. Priv c fins Luc",
                                tp_adm == 5 ~ "5. Priv s fins Luc",
                                tp_adm == 6 ~ "6. Especial",
                                tp_adm == 7 ~ "6. Especial"),
          Desc_Var = case_when( var == "n"          ~ "Cursos" ,
                                var == "concluinte" ~ "Concluintes" ,
                                var == "matricula"  ~ "Matriculas",
                                var == "vagas"      ~ "Vagas")) %>%
  filter( !is.na( `Categoria Adm` ))


plot_censup_1 <-
  ggplot( dados_desc )+
  geom_line( aes( x = ano, y = val, col =`Categoria Adm` ), linewidth = 1.1 )+
  facet_wrap( ~Desc_Var, scales = "free" )+
  labs( x = "Ano", y = "", color = "" )+
  theme_bw()+
  theme( legend.position = "bottom",
         axis.title  = element_text(size = 10),
         axis.text   = element_text(size = 10),
         legend.text = element_text(size = 10))

path_pics <-
  "./pics/"

ggsave( plot = plot_censup_1,
        filename = paste0( path_pics, "plot_censup_1.pdf"),
        units = "px", width = 1400, height = 1200 )

# Analise Geografia dos cursos/Matriculas/Vagas etc...

dta_2024 <-
  read.csv( paste0( path_censup, "2024/dados/MICRODADOS_CADASTRO_CURSOS_2024.CSV" ), 
            sep = ";", encoding = "utf8" )

# Analisa Por Municipio e Codigo

base_geo <- 
  filter( dta_2024, NO_CURSO == "Medicina" )%>% 
  group_by( CO_CURSO,  TP_CATEGORIA_ADMINISTRATIVA, CO_MUNICIPIO, CO_UF, CO_REGIAO )%>%
  summarise( n = n (),
             conc = sum( QT_CONC ),
             mat  = sum( QT_MAT ),
             vaga = sum( QT_VG_TOTAL ) )%>%
  mutate( Categoria_Adm = case_when( TP_CATEGORIA_ADMINISTRATIVA == 1 ~ "1. Federal",
                                       TP_CATEGORIA_ADMINISTRATIVA == 2 ~ "2. Estadual",
                                       TP_CATEGORIA_ADMINISTRATIVA == 3 ~ "3. Municipal",
                                       TP_CATEGORIA_ADMINISTRATIVA == 4 ~ "4. Priv c fins Luc",
                                       TP_CATEGORIA_ADMINISTRATIVA == 5 ~ "5. Priv s fins Luc",
                                       TP_CATEGORIA_ADMINISTRATIVA == 6 ~ "6. Especial",
                                       TP_CATEGORIA_ADMINISTRATIVA == 7 ~ "6. Especial") )


shp_br  <- geobr::read_country()

shp_mun <- 
  sf::st_centroid( geobr::read_municipality( year = 2024 ) )%>%
  rename( geometry = geom )

shp_mun <- cbind( shp_mun, sf::st_coordinates( shp_mun ) )

resumo_ <- 
  base_geo %>%
  group_by(CO_MUNICIPIO, TP_CATEGORIA_ADMINISTRATIVA, Categoria_Adm )%>%
  summarise( across( .cols = c("n","conc","mat","vaga"),
                     .fns  = ~sum(.) ))


resumo_cruzado <-
  left_join( resumo_, shp_mun, 
           by = c("CO_MUNICIPIO" = "code_muni") )

plot_censup_2 <- ggplot()+
  geom_sf( data = shp_br )+
  geom_point( data = resumo_cruzado, aes(x = X, y = Y, size = vaga ), alpha = 0.2 )+
  facet_wrap( ~Categoria_Adm )+
  labs( x = "Latitude", y = "Longitude", size = "Vagas" )+
  theme_bw()+
  theme( legend.position = "bottom" )

plot_censup_3 <- ggplot()+
  geom_sf( data = shp_br )+
  geom_point( data = resumo_cruzado, aes(x = X, y = Y, size = mat ), alpha = 0.2 )+
  facet_wrap( ~Categoria_Adm )+
  labs( x = "Latitude", y = "Longitude", size = "Matriculas" )+
  theme_bw()+
  theme( legend.position = "bottom" )


plot_censup_4 <- ggplot()+
  geom_sf( data = shp_br )+
  geom_point( data = resumo_cruzado, aes(x = X, y = Y, size = n ), alpha = 0.2 )+
  facet_wrap( ~Categoria_Adm )+
  labs( x = "Latitude", y = "Longitude", size = "Cursos" )+
  theme_bw()+
  theme( legend.position = "bottom" )


# Exporta Plots


ggsave( plot = plot_censup_2,
        filename = paste0( path_pics, "plot_censup_2.pdf"),
        units = "px", width = 1400, height = 1200 )

ggsave( plot = plot_censup_3,
        filename = paste0( path_pics, "plot_censup_3.pdf"),
        units = "px", width = 1400, height = 1200 )

ggsave( plot = plot_censup_4,
        filename = paste0( path_pics, "plot_censup_4.pdf"),
        units = "px", width = 1400, height = 1200 )


ggsave( plot = plot_censup_1,
        filename = paste0( path_pics, "plot_censup_3.pdf"),
        units = "px", width = 1400, height = 1200 )


ggsave( plot = plot_censup_1,
        filename = paste0( path_pics, "plot_censup_4.pdf"),
        units = "px", width = 1400, height = 1200 )

