
# Importa Bibliotecas

library( tidyverse )
library( stargazer )
library( geobr )
library( sf )

path_int <-
  "./data/inter/"

path_files <- 
  "./data/raw/ENAMED/DADOS/Enade/"

files <- list.files( path_files )

df_enamed <-
  map( paste0( path_files, files ), read.csv, sep = ";")

# Importa dados


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

df_final <-
  readRDS( paste0( path_int, "data_enamed_final.rds"))%>%
  left_join( nota_curso_final, by = "CO_CURSO" )%>%
  mutate( dummy_conc_12 = if_else( nota_curso %in% c("1","2"),1,0) )


# Modelo Probit

mod_probit_1 <- 
  glm( formula = dummy_conc_12 ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies,
       family  = binomial(link = "probit"),
       data    = df_final )

mod_probit_2 <- 
  glm( formula = dummy_conc_12 ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + idade_curso + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies,
       family  = binomial(link = "probit"),
       data    = df_final )

mod_probit_3 <- 
  glm( formula = dummy_conc_12 ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + idade_curso + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies + nota_fies + nota_sisu,
       family  = binomial(link = "probit"),
       data    = df_final )

# Modelo Com dummy de idade

mod_probit_1_dt <- 
  glm( formula = dummy_conc_12 ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + d_10_ys + d_20_ys + + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies,
       family  = binomial(link = "probit"),
       data    = df_final )

mod_probit_2_dt <- 
  glm( formula = dummy_conc_12 ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + d_10_ys + d_20_ys + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies,
       family  = binomial(link = "probit"),
       data    = df_final )

mod_probit_3_dt <- 
  glm( formula = dummy_conc_12 ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + d_10_ys + d_20_ys + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies + nota_fies + nota_sisu,
       family  = binomial(link = "probit"),
       data    = df_final )

# Sem dummy de tipo de inst


mod_probit_1_v2 <- 
  glm( formula = dummy_conc_12 ~ norte + nordeste + sul + coeste + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies,
       family  = binomial(link = "probit"),
       data    = df_final )

mod_probit_2_v2 <- 
  glm( formula = dummy_conc_12 ~ norte + nordeste + sul + coeste + idade_curso + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies,
       family  = binomial(link = "probit"),
       data    = df_final )

mod_probit_3_v2 <- 
  glm( formula = dummy_conc_12 ~ norte + nordeste + sul + coeste + idade_curso + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies + nota_fies + nota_sisu,
       family  = binomial(link = "probit"),
       data    = df_final )

# Sem dummy de tipo mas com dummy de tempo


mod_probit_1_dt_v2 <- 
  glm( formula = dummy_conc_12 ~ norte + nordeste + sul + coeste + d_10_ys + d_20_ys + + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies,
       family  = binomial(link = "probit"),
       data    = df_final )

mod_probit_2_dt_v2 <- 
  glm( formula = dummy_conc_12 ~ norte + nordeste + sul + coeste + d_10_ys + d_20_ys + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies,
       family  = binomial(link = "probit"),
       data    = df_final )

mod_probit_3_dt_v2 <- 
  glm( formula = dummy_conc_12 ~ norte + nordeste + sul + coeste + d_10_ys + d_20_ys + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies + nota_fies + nota_sisu,
       family  = binomial(link = "probit"),
       data    = df_final )

# Resumo Modelos

stargazer( mod_probit_1, mod_probit_2, mod_probit_3 , mod_probit_1_dt) 
stargazer(mod_probit_2_dt, mod_probit_3_dt )

stargazer( mod_probit_1_v2, mod_probit_2_v2, mod_probit_3_v2, mod_probit_1_dt_v2)
stargazer( mod_probit_2_dt_v2, mod_probit_3_dt_v2 )
