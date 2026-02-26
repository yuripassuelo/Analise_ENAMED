
library( tidyverse )
library( stargazer )
library( geobr )
library( sf )

path_int <-
  "./data/inter/"

df_final <-
  readRDS( paste0( path_int, "data_enamed_final.rds"))



# Testa Modelo


# Acertos (Perc)

mod_1 <-
  lm( formula = acerto_med ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies ,
      data    = df_final )

summary( mod_1 )

mod_2 <-
  lm( formula = acerto_med ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + idade_curso + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies ,
      data    = df_final )

summary( mod_2 )


mod_3 <-
  lm( formula = acerto_med ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + idade_curso + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies + nota_fies + nota_sisu ,
      data    = df_final )

summary( mod_3 )

# Modelos com Dummies de tempo



mod_1_dt <-
  lm( formula = acerto_med ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + d_10_ys + d_20_ys + + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies ,
      data    = df_final )

summary( mod_1_dt )

mod_2_dt <-
  lm( formula = acerto_med ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + d_10_ys + d_20_ys + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies,
      data    = df_final )

summary( mod_2_dt )

mod_3_dt <-
  lm( formula = acerto_med ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + d_10_ys + d_20_ys + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies + nota_fies + nota_sisu ,
      data    = df_final )

summary( mod_3_dt )


stargazer::stargazer( mod_1_dt, mod_2_dt, mod_3_dt )

stargazer::stargazer( mod_1, mod_2, mod_3, mod_1_dt, mod_2_dt, mod_3_dt )

# Selecionando Somente Universidades Pariculares (Exc. Fed e Est)


mod_1_priv <-
  lm( formula = acerto_med ~ d_munic + p_sfl + comum + norte + nordeste + sul + coeste + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies ,
      data    = filter( df_final, d_pub_fed == 0 & d_pub_est == 0 ) )

summary( mod_1_priv )

mod_2_priv <-
  lm( formula = acerto_med ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + idade_curso + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies ,
      data    = filter( df_final, d_pub_fed == 0 & d_pub_est == 0 ) )

summary( mod_2_priv )


mod_3_priv <-
  lm( formula = acerto_med ~ d_munic + p_sfl + comum + norte + nordeste + sul + coeste + idade_curso + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies + nota_fies ,
      data    = filter( df_final, d_pub_fed == 0 & d_pub_est == 0 ) )

summary( mod_3_priv )

# Regressão VALOR DESEMP

mod_val_fies_1 <-
  lm( formula = acerto_med ~  p_sfl + comum + norte + nordeste + sul + coeste + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies + val_curso,
      data    = df_final )


mod_val_fies_2 <-
  lm( formula = acerto_med ~ p_sfl + comum + norte + nordeste + sul + coeste + idade_curso + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies + val_curso,
      data    = df_final )

mod_val_fies_3 <-
  lm( formula = acerto_med ~ p_sfl + comum + norte + nordeste + sul + coeste + idade_curso + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies + nota_fies + val_curso,
      data    = df_final )

summary( mod_val_fies_1 )
summary( mod_val_fies_2 )
summary( mod_val_fies_3 )

stargazer::stargazer( mod_val_fies_1, mod_val_fies_2, mod_val_fies_3 )


plot( df_final$val_curso, df_final$acerto_med )

val_acert_plot <-
  ggplot( df_final )+
  geom_point( aes( x = val_curso/1e3, y = acerto_med, size = n, col =  ), alpha = 0.2 )+
  labs( x = "Valor Curso (milhares)", y = "Acertos Médios", size = "Alunos" )+
  theme_bw()+
  theme( legend.position = "bottom",
         axis.text = element_text( size = 10 ),
         axis.title = element_text( size = 10 ))



stargazer::stargazer( mod_1_priv, mod_2_priv,  mod_3_priv )

plot( filter( df_final, d_pub_fed == 0 & d_pub_est == 0 )$idade_curso,
      filter( df_final, d_pub_fed == 0 & d_pub_est == 0 )$acerto_med )

plot( filter( df_final, d_pub_fed == 0 & d_pub_est == 0 & nota_fies !=0 )$nota_fies,
      filter( df_final, d_pub_fed == 0 & d_pub_est == 0 & nota_fies !=0 )$acerto_med )

# Mesmo entre as privadas tem uma forte heterogeneidade no resultado
# é errado dizer que Privado = Pior, Privado = maior prob de ser pior


mod_2_priv_dt <-
  lm( formula = acerto_med ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + d_20_ys + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies ,
      data    = filter( df_final, d_pub_fed == 0 & d_pub_est == 0 ) )

summary( mod_2_priv_dt )


mod_3_priv <-
  lm( formula = acerto_med ~ d_munic + p_sfl + comum + norte + nordeste + sul + coeste + idade_curso + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies + nota_fies ,
      data    = filter( df_final, d_pub_fed == 0 & d_pub_est == 0 ) )

summary( mod_3_priv )



# Usa Proficiencia TRRI


mod_1_2 <-
  lm( formula = prof_med ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_10_a_30 + mais_de_30 + perc_fem + idade_med + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies ,
      data    = df_final )

summary( mod_1_2 )

mod_2_2 <-
  lm( formula = prof_med ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + idade_curso + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_10_a_30 + mais_de_30 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies ,
      data    = df_final )

summary( mod_2_2 )


mod_3_2 <-
  lm( formula = prof_med ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + idade_curso + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_10_a_30 + mais_de_30 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies + nota_fies + nota_sisu ,
      data    = df_final )

summary( mod_3_2 )

# Modelos Especificos com Apenas Universidades Privadas (Exc. Federal e Estadual )


mod_1_priv <-
  lm( formula = acerto_med ~ d_munic + p_sfl + comum + norte + nordeste + sul + coeste + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies ,
      data    = filter( df_final, d_pub_fed != 1 & d_pub_est != 1 ) )

summary( mod_1_priv )

mod_2_priv <-
  lm( formula = acerto_med ~ d_munic + p_sfl + comum + norte + nordeste + sul + coeste + idade_curso + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies ,
      data    = filter( df_final, d_pub_fed != 1 & d_pub_est != 1 ) )

summary( mod_2_priv )


mod_3_priv <-
  lm( formula = acerto_med ~ d_munic + p_sfl + comum + norte + nordeste + sul + coeste + idade_curso + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies + nota_fies ,
      data    = filter( df_final, d_pub_fed != 1 & d_pub_est != 1 ) )

summary( mod_3_priv )

# Interações


mod_it_1 <-
  lm( formula = acerto_med ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies ,
      data    = mutate(df_final, CO_CATEGAD_CAT = as.character( CO_CATEGAD ) ) )

summary( mod_it_1 )

mod_it_2 <-
  lm( formula = acerto_med ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + idade_curso + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies ,
      data    = df_final )

summary( mod_2 )


mod_3 <-
  lm( formula = acerto_med ~ d_pub_fed + d_pub_est + d_munic + p_sfl + comum + norte + nordeste + sul + coeste + idade_curso + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies + nota_fies + nota_sisu ,
      data    = df_final )

summary( mod_3 )

# Modelos com Dummies de tempo

df_final_cat <-
  mutate(df_final, 
         CO_CATEGAD_CAT = as.character( CO_CATEGAD ),
         CO_REG         = case_when( sul      == 1 ~ "sul",
                                     sudeste  == 1 ~ "sud",
                                     coeste   == 1 ~ "coeste",
                                     norte    == 1 ~ "norte",
                                     nordeste == 1 ~ "nordeste"))


mod_1_it_dt <-
  lm( formula = acerto_med ~ CO_REG*d_20_ys + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies ,
      data    = df_final_cat )

summary( mod_1_it_dt )

mod_2_it_dt <-
  lm( formula = acerto_med ~ CO_REG*d_20_ys + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies,
      data    = df_final_cat )

summary( mod_2_it_dt )

mod_3_it_dt <-
  lm( formula = acerto_med ~ CO_REG*d_20_ys + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies + nota_fies + nota_sisu ,
      data    = df_final_cat )

summary( mod_3_it_dt )

stargazer::stargazer( mod_1_it_dt, mod_2_it_dt, mod_3_it_dt)

# Iterações com Regiao


mod_1_it_dt_2 <-
  lm( formula = acerto_med ~ CO_CATEGAD_CAT*d_20_ys + norte + nordeste + sul + coeste + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies ,
      data    = df_final_cat )

summary( mod_1_it_dt_2 )

mod_2_it_dt_2 <-
  lm( formula = acerto_med ~ CO_CATEGAD_CAT*d_20_ys + norte + nordeste + sul + coeste + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies,
      data    = df_final_cat )

summary( mod_2_it_dt_2 )

mod_3_it_dt_2 <-
  lm( formula = acerto_med ~ CO_CATEGAD_CAT*d_20_ys + norte + nordeste + sul + coeste + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies + nota_fies + nota_sisu ,
      data    = df_final_cat )

summary( mod_3_it_dt_2 )

stargazer::stargazer( mod_1_it_dt_2, mod_2_it_dt_2, mod_3_it_dt_2)

# Dummies especificas Tipo ADM X Regiao


mod_1_it_dt_3 <-
  lm( formula = acerto_med ~ CO_CATEGAD_CAT*CO_REG + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies ,
      data    = df_final_cat )

summary( mod_1_it_dt_3 )

mod_2_it_dt_3 <-
  lm( formula = acerto_med ~ CO_CATEGAD_CAT*CO_REG + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies,
      data    = df_final_cat )

summary( mod_2_it_dt_3 )

mod_3_it_dt_3 <-
  lm( formula = acerto_med ~ CO_CATEGAD_CAT*CO_REG + ate_1_5_sm + de_15_a_3 + de_3_a_45 + de_45_a_6 + de_6_a_10 + perc_fem + idade_med + perc_n_trab + ate_ef_mae + es_com_mae + ate_ef_pai + es_com_pai + satisf_cur+ satisf_ies + nota_fies + nota_sisu ,
      data    = df_final_cat )

summary( mod_3_it_dt_3 )

stargazer::stargazer( mod_1_it_dt_3, mod_2_it_dt_3, mod_3_it_dt_3)

# Resultados


stargazer( mod_1, mod_2, mod_3 )

# Plots de Correlações Interessantes

# Distribuição das notas de Alunos cursos

my_colors <- c( "1. Federal"            = "#E62A09",
                "2. Estadual"           = "#E69109",
                "3. Municipal"          = "#09AB47",
                "4. Privada fins lucra" = "#E5E81A",
                "5. Privada sem fins"   = "#5051A3",
                "6. Especial"           = "#30ACB3",
                "7. Comunitaria"        = "#6B0A6E")

df_vars <- 
  df_final %>%
  mutate( cod_categ = case_when( CO_CATEGAD == 1 ~ "1. Federal",
                                 CO_CATEGAD == 2 ~ "2. Estadual",
                                 CO_CATEGAD == 3 ~ "3. Municipal",
                                 CO_CATEGAD == 4 ~ "4. Privada fins lucra",
                                 CO_CATEGAD == 5 ~ "5. Privada sem fins",
                                 CO_CATEGAD == 7 ~ "6. Especial",
                                 CO_CATEGAD == 8 ~ "7. Comunitaria"))

df_final_long <-
  df_vars %>%
  select( CO_CURSO, cod_categ, n , acerto_med, idade_curso, idade_med, satisf_cur, satisf_ies )%>%
  pivot_longer( cols      = !c("CO_CURSO","cod_categ","n","acerto_med"),
                names_to  = "vars",
                values_to = "values")%>%
  mutate( var_desc = case_when( vars == "idade_curso" ~ "Idade Curso",
                                vars == "idade_med"   ~ "Idade Media",
                                vars == "satisf_cur"  ~ "Satisf. Curso",
                                vars == "satisf_ies"  ~ "Satisf. IES"))

plot_covars <-
  df_final_long %>%
  ggplot( aes( x = values, y = acerto_med, size = n, col = cod_categ  ) )+
  geom_point( alpha = 0.4 )+
  #scale_color_manual(values = my_colors)+
  theme_bw()+
  labs( x = "Idade Media",
        y = "Acertos Medios",
        size = "", color = "")+
  theme( axis.text = element_text( size = 10 ),
         axis.title = element_text( size = 10 ),
         legend.text = element_text( size = 8 ),
         legend.position = "bottom")+
  facet_wrap( ~var_desc, scales = "free" )

path_out_pic <-
  "./pics/"

ggsave( filename = paste0( path_out_pic, "val_acert_plot.pdf"),
        plot     = val_acert_plot, units = "px",
        width    = 1200, height = 1100 )

ggsave( filename = paste0( path_out_pic, "plot_covars.pdf"),
        plot     = plot_covars, units = "px",
        width    = 1900, height = 1400 )

# BoxPlot Tipo Curso (Pub Fed, Pub Est, ... )

curso_categad <-
  df_enamed[[1]] %>% 
  select( CO_CURSO, CO_CATEGAD ) %>%
  group_by( CO_CURSO, CO_CATEGAD )%>%
  summarise()

dta_prof <-
  left_join( df_enamed[[17]], curso_categad, by = "CO_CURSO" ) %>%
  select( CO_CURSO, CO_CATEGAD, PROFICIENCIA, NT_GER, PER_ACERTO_ENARE )%>%
  mutate( `Tipo Curso` = 
            case_when( CO_CATEGAD == 1 ~ "Pub.\nFederal",
                       CO_CATEGAD == 2 ~ "Pub.\nEstadual",
                       CO_CATEGAD == 3 ~ "Municipal",
                       CO_CATEGAD == 4 ~ "Priv.\nsem fins",
                       CO_CATEGAD == 5 ~ "Priv.\ncom fins",
                       CO_CATEGAD == 7 ~ "Especial",
                       CO_CATEGAD == 8 ~ "Comunitaria"))

dta_prof %>%
  ggplot( aes( x = `Tipo Curso`, y = PER_ACERTO_ENARE, group = `Tipo Curso`  ))+
  geom_boxplot()+
  theme_bw()+
  labs( y = "Percentual Acertos" )+
  theme( axis.text  = element_text( size = 10 ),
         axis.title = element_text( size = 10 ) )

df_final%>%
  ggplot( aes( x = es_com_mae, y = acerto_med ))+
  geom_point()

df_final%>%
  ggplot( aes( x = es_com_pai, y = acerto_med, size = n ))+
  geom_point( alpha = 0.4 )

df_final%>%
  ggplot( aes( x = perc_n_trab, y = acerto_med, size = n, 
               color = as.character( CO_CATEGAD ),
               fill = as.character( CO_CATEGAD ) ))+
  geom_point( alpha = 0.4 )


# Analise do perfil de Renda das IES

df_renda_long <-
  df_final %>%
  select( CO_CURSO, CO_CATEGAD, ate_1_5_sm, de_15_a_3, de_3_a_45, de_45_a_6,  
          de_6_a_10, de_10_a_30, mais_de_30 )%>%
  mutate( mais_de_10 = de_10_a_30 + mais_de_30,
          `Tipo Curso` = 
            case_when( CO_CATEGAD == 1 ~ "1.Pub.\nFederal",
                       CO_CATEGAD == 2 ~ "2.Pub.\nEstadual",
                       CO_CATEGAD == 3 ~ "3.Municipal",
                       CO_CATEGAD == 4 ~ "4.Priv.\nsem fins",
                       CO_CATEGAD == 5 ~ "5.Priv.\ncom fins",
                       CO_CATEGAD == 7 ~ "6.Especial",
                       CO_CATEGAD == 8 ~ "7.Comunitaria"))%>%
  select( -c( de_10_a_30, mais_de_30 ))%>%
  pivot_longer( cols      = !c("CO_CURSO","CO_CATEGAD","Tipo Curso"),
                names_to  = "faixa_renda",
                values_to = "perc" )%>%
  mutate( desc_faixa_renda = 
            case_when( faixa_renda == "ate_1_5_sm" ~ "Até 1.5 SM", 
                       faixa_renda == "de_15_a_3"  ~ "de 1.5 a 3", 
                       faixa_renda == "de_3_a_45"  ~ "de 3 a 4.5",
                       faixa_renda == "de_45_a_6"  ~ "de 4.5 a 6",
                       faixa_renda == "de_6_a_10"  ~ "de 6 a 10",
                       faixa_renda == "mais_de_10" ~ "mais de 10") )

plot_dist_renda <- ggplot( df_renda_long )+
  geom_boxplot( aes( x = as.character( CO_CATEGAD ), y = perc, 
                     color = `Tipo Curso`, fill = `Tipo Curso` ),
                alpha = 0.4 )+
  theme_bw()+
  labs( x = "Categoria Adm", y = "Percentual", fill = "", color = "" )+
  facet_wrap( ~desc_faixa_renda )+
  theme( legend.text = element_text( size = 8 ),
         axis.title  = element_text( size = 10 ),
         legend.position = "bottom" )



ggsave( filename = paste0( path_out_pic, "plot_dist_renda.pdf" ),
        plot     = plot_dist_renda,
        units = "px", width = 1300, height = 1100 ) 

# Pais com Ensino Superior

df_educ_long <-
  df_final %>%
  select( CO_CURSO, CO_CATEGAD, es_com_mae, es_com_pai )%>%
  mutate( `Tipo Curso` = 
            case_when( CO_CATEGAD == 1 ~ "1.Pub.\nFederal",
                       CO_CATEGAD == 2 ~ "2.Pub.\nEstadual",
                       CO_CATEGAD == 3 ~ "3.Municipal",
                       CO_CATEGAD == 4 ~ "4.Priv.\nsem fins",
                       CO_CATEGAD == 5 ~ "5.Priv.\ncom fins",
                       CO_CATEGAD == 7 ~ "6.Especial",
                       CO_CATEGAD == 8 ~ "7.Comunitaria"))%>%
  pivot_longer( cols      = !c("CO_CURSO","CO_CATEGAD","Tipo Curso"),
                names_to  = "var",
                values_to = "perc" )%>%
  mutate( var_educ_desc = case_when( var == "es_com_mae" ~ "Mãe com ES Comp",
                                     var == "es_com_pai" ~ "Pai com ES Comp"))

plot_dist_educ <- ggplot( df_educ_long )+
  geom_boxplot( aes( x = as.character( CO_CATEGAD ), y = perc, 
                     color = `Tipo Curso`, fill = `Tipo Curso` ),
                alpha = 0.4 )+
  theme_bw()+
  labs( x = "Categoria Adm", y = "Percentual", fill = "", color = "" )+
  facet_wrap( ~var_educ_desc )+
  theme( legend.text = element_text( size = 8 ),
         axis.title  = element_text( size = 10 ),
         legend.position = "bottom" )

ggsave( filename = paste0( path_out_pic, "plot_educ_curso.pdf" ),
        plot     = plot_dist_educ,
        units = "px", width = 1300, height = 1100 ) 


# Proficiencia dos cursos de Acordo com a Nota TECNICA

reg_curso_df <-
  df_enamed[[1]]%>%
  group_by( CO_CURSO, CO_IES, CO_CATEGAD, CO_MUNIC_CURSO, CO_UF_CURSO, CO_REGIAO_CURSO )%>%
  summarise()

df_prof_curso <- df_enamed[[17]] %>%
  select( CO_CURSO, PROFICIENCIA, NT_GER, PER_ACERTO_ENARE )%>%
  filter( !is.na( PROFICIENCIA ))%>%
  mutate( d_prof = if_else( NT_GER > 60, 1, 0))%>%
  group_by( CO_CURSO )%>%
  summarise( perc_prof = mean( d_prof ),
             n         = n())%>%
  mutate( class_prof = case_when( perc_prof < 0.4                      ~ "1",
                                  perc_prof >= 0.4  & perc_prof < 0.6  ~ "2",
                                  perc_prof >= 0.6  & perc_prof < 0.75 ~ "3",
                                  perc_prof >= 0.75 & perc_prof < 0.9  ~ "4",
                                  perc_prof >= 0.9                     ~ "5") )%>%
  left_join(.,reg_curso_df, by = "CO_CURSO")


# Distribuição Regional

mapa_br_tot <-
  geobr::read_country()

mapa_br_mun <-
  geobr::read_municipality( year = 2024 )


muni_centroids <-
  cbind( select( mapa_br_mun, code_muni ),
         st_coordinates( st_centroid( mapa_br_mun  ) ) )

df_muni_cursos <-
  df_prof_curso %>%
  group_by( CO_MUNIC_CURSO, class_prof, CO_CURSO )%>%
  summarise()%>%
  ungroup()%>%
  group_by( CO_MUNIC_CURSO, class_prof )%>%
  summarise( n = n( ))

cruz_df_prof_curso <-
  left_join( df_muni_cursos, muni_centroids, by = c("CO_MUNIC_CURSO" = "code_muni") )

regiao_conceito <-
  ggplot()+
  geom_sf( data = mapa_br_tot )+
  geom_point( data = cruz_df_prof_curso, aes( x = X, y = Y, size = n ),
              alpha = 0.4 )+
  theme_bw()+
  labs( x = "Latitude", y = "Longitude", size = "Cursos")+
  facet_wrap( ~class_prof )+
  theme( axis.title      = element_text( size = 9 ),
         axis.text       = element_text( size = 8 ),
         legend.text     = element_text( size = 10 ),
         legend.title    = element_text( size = 10 ),
         legend.position = "bottom")


ggsave( plot = regiao_conceito,
        filename = paste0( path_out_pic, "dist_reg_notas.pdf"),
        units = "px", width = 1400, height = 1200 )

## Por Regiao

left_joinreg_curso_df %>%
  group_by( CO_REGIAO_CURSO )%>%
  summarise( n = n() )


# Analise

df_prof_curso %>%
  group_by( CO_CATEGAD, class_prof )%>%
  summarise( n = n() )%>%
  ungroup()%>%
  group_by( CO_CATEGAD )%>%
  mutate( perc = n/sum(n) )%>%
  pivot_wider( id_cols   = "CO_CATEGAD",
               names_from  = "class_prof",
               values_from = "perc" )

df_prof_curso %>%
  group_by( class_prof )%>%
  summarise( n = n() )%>%
  ungroup()%>%
  #group_by( CO_CATEGAD )%>%
  mutate( perc = n/sum(n) )

# Heterogeneidade No Desempenho


dta_prof_curso <-
  mutate( df_prof_curso, 
          n_prof   = n * perc_prof,
          n_n_prof = n - n_prof,
          perc_n_prof = 1 - perc_prof ) %>% 
  select( CO_CURSO, class_prof, n_prof, n_n_prof, perc_prof, perc_n_prof ) %>% 
  pivot_longer( cols = !c("CO_CURSO","class_prof"),
                names_to = "var",
                values_to = "val")

# Apenas Numeros
plot_conceito_num <-
  dta_prof_curso %>%
  filter( var %in% c("n_prof","n_n_prof"))%>%
  mutate( var = if_else( var == "n_prof", "Proficiente", "Não Proficiente"))%>%
  ggplot( aes( x = class_prof, y = val, fill = var ))+
  geom_bar( position = "stack", stat = "identity")+
  labs( x = "Conceito", y = "Número de Estudantes", fill = "")+
  theme_bw()+
  theme( legend.position = "bottom" )+
  scale_fill_manual( values = c( "Proficiente" = "#CFCFCF", 
                                 "Não Proficiente"     = "#878787" ))


ggsave( plot = plot_conceito_num,
        filename = paste0( path_out_pic, "plot_conceito_num.pdf"),
        units = "px", width = 1200, height = 1000 )


# Mostrando em percentual
plot_conceito_perc <-
  dta_prof_curso %>%
  filter( var %in% c("n_prof","n_n_prof"))%>%
  mutate( var = if_else( var == "n_prof", "Proficiente", "Não Proficiente"))%>%
  group_by( class_prof, var )%>%
  summarise( val = sum( val ))%>%
  ungroup()%>%
  group_by( class_prof )%>%
  mutate(perc = val/sum(val) )%>%
  ggplot( aes( x = class_prof, y = perc, fill = var ))+
  geom_bar( position = "stack", stat = "identity")+
  geom_text( data = ,
             aes( label = paste0( round( perc*100, 2), "%" ) ), 
             position = position_stack(vjust = 0.5) )+
  labs( x = "Conceito", y = NULL, fill = NULL )+
  theme_bw()+
  theme( axis.text.y     = element_blank(),
         legend.position = "bottom" )+
  scale_fill_manual( values = c( "Proficiente" = "#CFCFCF", 
                                 "Não Proficiente"     = "#878787" ))

ggsave( plot = plot_conceito_perc,
        filename = paste0( path_out_pic, "plot_conceito_perc.pdf"),
        units = "px", width = 1200, height = 1000 )
