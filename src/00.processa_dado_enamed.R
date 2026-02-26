
library( tidyverse )

# Analise ENAMED

path_files <- "./data/raw/ENAMED/DADOS/Enade/"

files <- list.files( path_files )


df_enamed <-
  map( paste0( path_files, files ), read.csv, sep = ";")

# Desempenho Medio

desemp <- df_enamed[[17]] %>%
  select( CO_CURSO, PROFICIENCIA, NT_GER, PER_ACERTO_ENARE )%>%
  group_by( CO_CURSO )%>%
  summarise( prof_med   = mean( PROFICIENCIA ,na.rm=TRUE),
             nt_ger_med = mean( NT_GER ,na.rm=TRUE),
             acerto_med = mean( PER_ACERTO_ENARE ,na.rm=TRUE),
             n          = n() )

# Regiao Curso

regiao_turno <-
  df_enamed[[1]] %>% 
  select( CO_CURSO, CO_CATEGAD, CO_REGIAO_CURSO )%>%
  group_by( CO_CURSO, CO_CATEGAD, CO_REGIAO_CURSO )%>%
  summarise()%>%
  mutate( d_pub_fed = if_else( CO_CATEGAD == 1, 1, 0),
          d_pub_est = if_else( CO_CATEGAD == 2, 1, 0),
          
          d_munic   = if_else( CO_CATEGAD == 3, 1, 0),
          
          p_cfl     = if_else( CO_CATEGAD == 4, 1, 0),
          p_sfl     = if_else( CO_CATEGAD == 5, 1, 0),
          
          espec     = if_else( CO_CATEGAD == 7, 1, 0),
          comum     = if_else( CO_CATEGAD == 8, 1, 0),
          
          # Regiao 
          norte     = if_else( CO_REGIAO_CURSO == 1 ,1 , 0 ),
          nordeste  = if_else( CO_REGIAO_CURSO == 2 ,1 , 0 ),
          sudeste   = if_else( CO_REGIAO_CURSO == 3 ,1 , 0 ),
          sul       = if_else( CO_REGIAO_CURSO == 4 ,1 , 0 ),
          coeste    = if_else( CO_REGIAO_CURSO == 5 ,1 , 0 ),
  )%>%
  ungroup()%>%
  select( !c( CO_REGIAO_CURSO ))

# Idade Media

idad_med_aluno <-
  df_enamed[[20]]%>%
  group_by(CO_CURSO) %>%
  summarise( idade_med = mean( NU_IDADE ))

# Percentual Sexo

perc_sexo <- df_enamed[[19]] %>%
  group_by( CO_CURSO, TP_SEXO )%>%
  summarise( n = n() )%>%
  ungroup()%>%
  group_by( CO_CURSO )%>%
  mutate( perc = n/sum(n))%>%
  pivot_wider( id_cols = "CO_CURSO",
               names_from = "TP_SEXO",
               values_from = "perc" )%>%
  rename( perc_fem = F )%>%
  select( CO_CURSO, perc_fem )

# Satisfacao media com curso e IES

satisf <-
  df_enamed[[18]] %>% 
  mutate( across( .cols = contains( "QE" ),
                  .fns  = ~as.numeric(replace(.,.==".",NA))) )%>% 
  select( CO_CURSO, QE_I57, QE_I58 )%>%
  group_by( CO_CURSO )%>%
  summarise( across( .cols = c("QE_I57", "QE_I58"),
                     .fns  = ~mean(.,na.rm=TRUE)))%>%
  rename( satisf_ies = QE_I58 ,
          satisf_cur = QE_I57 )

# Faixa Renda

rend_faixa <-
  df_enamed[[7]] %>% 
  mutate( across( .cols = contains( "QE" ),
                  .fns  = ~replace(.,.==".",NA)) )%>% 
  select( CO_CURSO, QE_I09 )%>%
  group_by( CO_CURSO, QE_I09 )%>%
  summarise( n = n())%>%
  ungroup(  )%>%
  group_by( CO_CURSO )%>%
  mutate( perc = n/sum(n)) %>% 
  pivot_wider( id_cols = "CO_CURSO",
               names_from = "QE_I09",
               values_from = "perc")%>%
  mutate( across( .cols = everything(),
                  .fns  = ~replace(.,is.na(.),0) ),
          ate_1_5_sm = `A`,
          de_15_a_3  = `B`,
          de_3_a_45  = `C`,
          de_45_a_6  = `D`,
          de_6_a_10  = `E`,
          de_10_a_30 = `F`,
          mais_de_30 = `G`)%>%
  select( CO_CURSO, ate_1_5_sm, de_15_a_3, de_3_a_45, de_45_a_6,
          de_6_a_10, de_10_a_30, mais_de_30 )


# Educ Pais

ed_mae <-
  df_enamed[[4]] %>%
  mutate( QE_I06 = replace(QE_I06,QE_I06==".",NA) )%>%
  group_by( CO_CURSO, QE_I06 )%>%
  summarise( n = n() )%>%
  ungroup() %>%
  group_by( CO_CURSO )%>%
  mutate( perc = n/sum( n ))%>%
  pivot_wider( id_cols = "CO_CURSO",
               names_from = "QE_I06",
               values_from = "perc" )%>%
  mutate( across( .cols = everything(),
                  .fns  = ~replace(.,is.na(.),0)),
          s_educ_mae = `A`,
          ate_ef_mae = `B`+`C`,
          em_com_mae = `D`,
          es_com_mae = `E`+`F`+`G`)%>%
  select( CO_CURSO, s_educ_mae, ate_ef_mae, em_com_mae, es_com_mae )



ed_pai <-
  df_enamed[[5]] %>%
  mutate( QE_I07 = replace(QE_I07,QE_I07==".",NA) )%>%
  group_by( CO_CURSO, QE_I07 )%>%
  summarise( n = n() )%>%
  ungroup() %>%
  group_by( CO_CURSO )%>%
  mutate( perc = n/sum( n ))%>%
  pivot_wider( id_cols = "CO_CURSO",
               names_from = "QE_I07",
               values_from = "perc" )%>%
  mutate( across( .cols = everything(),
                  .fns  = ~replace(.,is.na(.),0)),
          s_educ_pai = `A`,
          ate_ef_pai = `B`+`C`,
          em_com_pai = `D`,
          es_com_pai = `E`+`F`+`G`)%>%
  select( CO_CURSO, s_educ_pai, ate_ef_pai, em_com_pai, es_com_pai )

# Trabalho

df_trab <- df_enamed[[8]]  %>%
  mutate( QE_I10 = replace(QE_I10,QE_I10==".",NA) )%>%
  group_by( CO_CURSO, QE_I10 )%>%
  summarise( n = n() )%>%
  ungroup() %>%
  group_by( CO_CURSO )%>%
  mutate( perc = n/sum( n ))%>%
  pivot_wider( id_cols = "CO_CURSO",
               names_from = "QE_I10",
               values_from = "perc" )%>%
  mutate( across( .cols = everything(),
                  .fns  = ~replace(.,is.na(.),0)),
          perc_n_trab = `A` )%>%
  select( CO_CURSO, perc_n_trab )

# Vaiavel de Idade do Curso

emec <- readRDS( "D:/ENAMED/emec_final.rds" )

emec_df <- select( emec, Código.Curso, idade_curso )%>%
  rename( CO_CURSO = Código.Curso )

# Traz notas de corte

notas_corte <- read.csv( "D:/SISU_FIES/notas_corte.csv")

notas_corte_df <-
  select( notas_corte, !CO_IES )%>%
  mutate( nota_fies = if_else( tipo == "FIES", NU_NOTACORTE, 0 ),
          nota_sisu = if_else( tipo == "SISU", NU_NOTACORTE, 0 ))%>%
  rename( CO_CURSO = CO_IES_CURSO )%>%
  select( CO_CURSO, nota_fies, nota_sisu )

# Valor Médio do curso

val_med_cur <-
  readRDS( "D:/SISU_FIES/val_cursos_fies.rds" )%>%
  rename( CO_CURSO = `Código do Curso`,
          val_curso = val )
# Cruza as bases


df_final <-
  reduce( .x = list( desemp, regiao_turno, rend_faixa, idad_med_aluno, perc_sexo, df_trab, ed_mae, ed_pai, satisf, emec_df, notas_corte_df, val_med_cur ),
          .f = dplyr::left_join,
          by = c("CO_CURSO"))%>%
  mutate( across( .cols = contains("nota"),
                  .fns  = ~replace(.,is.na(.),0)),
          mais_de_10 = de_10_a_30 + mais_de_30,
          
          d_10_ys   = if_else( idade_curso < 10, 1, 0),
          d_20_ys   = if_else( idade_curso < 20, 1, 0),
          d_50_ys   = if_else( idade_curso < 50, 1, 0))

#

path_int <-
  "./data/inter/"

saveRDS( df_final, file = paste0( path_int, "data_enamed_final.rds"))
