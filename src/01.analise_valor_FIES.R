
# Analise de Custos FIES
library( tidyverse )
library( data.table )
# PEGANDO BASE DE INSCRIÇÕES - O QUE EFETIVAMENTE OCORREU

path_insc <-
  "./data/raw/FIES_SISU/Inscricao/"

read_n_filt <-
  function( .file ){
  return(
    filter(
      fread( paste0( path_insc, .file ),
             sep = ";", dec = ",", encoding = "Latin-1" )
      ,`Situação Inscrição Fies`  == "CONTRATADA"
    )
  )
}

# Listagem dos arquivos
files_fies_insc <-
  list.files( path_insc )

files_fies_insc <-
  files_fies_insc[ grepl("inscricao", files_fies_insc)]

# Relacao Ano Semestre
names_list_insc <-
  map_chr( str_split( str_remove( files_fies_insc, ".csv|.CSV"), "_", 6), `[[`, 6 )

# Leitura e Tratamento dos ARQUIVOS

bases_contrat <-
  setNames( map( files_fies_insc, read_n_filt ), names_list_insc )


# Cruzamento com os dados de FIES
sel_vars <-
  c("Ano do processo seletivo", "Semestre do processo seletivo", "Nome da IES",
    "Código do curso", "Turno", "Percentual de financiamento", "Qtde semestre financiado" )

base_insc <-
  bind_rows( map( bases_contrat, ~{select(.x, all_of( sel_vars))}) )

# Leitura dos dados de oferta

path_ofert <-
  "./data/raw/FIES_SISU/Oferta/"

# Leitura e empIlhamento

read_ofert <-
  function(.file){
  return(
    fread( paste0( path_ofert, .file ), encoding = "Latin-1")
  )
}

files_fies_ofert <-
  list.files( path_ofert )


num_sems <-
  bind_rows( map( files_fies_ofert, read_ofert ) )%>%
  select(  Ano, Semestre, `Código do Curso`, `Nome do Curso`, Turno, contains("Semestre Bruto") )%>%
  mutate( across( .cols = contains( "Semestre Bruto"),
                  .fns  = ~as.numeric(str_replace_all( str_replace_all( str_replace_all( .,"\\.",""), ",", ".\\"),"-","0") ) ) )%>%
  pivot_longer( cols = !c('Ano', 'Semestre', 'Código do Curso', 'Nome do Curso', 'Turno' ),
                names_to = "sem",
                values_to = "val")%>%
  mutate( dummy_sem = if_else( val == 0,0,1) )%>%
  group_by( Ano, Semestre, `Código do Curso`, `Nome do Curso`, Turno )%>%
  summarise( sems = sum( dummy_sem ))


files_ofert <-
  bind_rows( map( files_fies_ofert, read_ofert ) )%>%
  select( Ano, Semestre, `Código do Curso`, `Nome do Curso`, Turno, `Valor bruto do curso`)%>%
  mutate( across( .cols = "Valor bruto do curso",
                  .fns  = ~as.numeric(str_replace_all(str_replace_all(.,"\\.",""),",","\\.") ) ) )%>%
  filter( !is.na( Ano ))%>%
  left_join( num_sems,
             by = c('Ano', 'Semestre', 'Código do Curso', 'Nome do Curso', 'Turno'))

base_val_cursos_fies <-
  files_ofert %>%
  filter( `Nome do Curso` == "MEDICINA")%>% 
  mutate( cod_anosem = paste( Ano, Semestre, sep = "_" ))%>%
  select( -c( Ano, Semestre, Turno, `Nome do Curso`, sems ))%>%
  group_by( `Código do Curso`)%>%
  summarise( val = mean(`Valor bruto do curso`, na.rm = TRUE))

base_val_cursos_fies_wide <-
  files_ofert %>%
  filter( `Nome do Curso` == "MEDICINA")%>% 
  mutate( cod_anosem = paste( Ano, Semestre, sep = "_" ))%>%
  select( -c( Ano, Semestre, Turno, `Nome do Curso`, sems ))%>%
  pivot_wider( id_cols = "Código do Curso",
               names_from  = "cod_anosem",
               values_from = "Valor bruto do curso",
               names_prefix = "val_")%>%
  left_join( base_val_cursos_fies )

base_mist <-
  files_ofert %>%
  filter( `Nome do Curso` == "MEDICINA")%>% 
  mutate( cod_anosem = paste( Ano, Semestre, sep = "_" ))%>%
  select( -c( Ano, Semestre, Turno, `Nome do Curso`, sems ))%>%
  left_join( base_val_cursos_fies, by = c("Código do Curso"))


grid_val_plot <-
  ggplot( data = base_mist )+
  geom_point( aes( x = val/1e3, y = `Valor bruto do curso`/1e3 ),alpha = 0.2)+
  geom_abline( col = "red", linetype = 2 )+
  labs( x = "Valor Médio", y = "Valor Semestre")+
  facet_wrap( ~cod_anosem )+
  theme_bw()+
  theme( axis.text = element_text( size = 10 ),
         axis.title = element_text( size = 10 ))

path_out_pic <-
  "./pics/"


# Salva Imagem comparando mensalidades por semestre e mensalidades médias 
ggsave( filename = paste0( path_out_pic,"grid_val_plot.pdf" ),
        plot     = grid_val_plot, units = "px",
        width    = 1600, height = 1400 )


join_table <-
  left_join( base_insc,
             files_ofert,
             by = c( "Ano do processo seletivo"      = "Ano",
                     "Semestre do processo seletivo" = "Semestre",
                     "Código do curso"               = "Código do Curso",
                     "Turno"                         = "Turno"))

# Valor final do FIES Financiado (Cruzando Inscritos e )

base_time <-
  expand.grid( ano = 2019:2021,
             sem = 1:2)%>%
  arrange( ano,sem )%>%
  mutate( time = row_number() )

base_val_fin_fies <- 
  join_table %>%
  mutate( val_final_fin = `Valor bruto do curso`*(`Percentual de financiamento`/100)*(`Qtde semestre financiado`/sems) )%>%
  group_by( `Ano do processo seletivo`, `Semestre do processo seletivo`, `Código do curso`,
            `Nome do Curso`)%>%
  summarise( val_final_fin = sum( val_final_fin ) )%>%
  rename( ano       = `Ano do processo seletivo`,
          sem       = `Semestre do processo seletivo`,
          cod_curso = `Código do curso`,
          nom_curso = `Nome do Curso`)%>%
  mutate( dummy_med = if_else( nom_curso == "MEDICINA", "Medicina","Outros") )%>%
  left_join( base_time, c("ano","sem"))


# Local de Salvar as figuras

posi_gastos <-
  base_val_fin_fies %>%
  group_by( time, nom_curso, dummy_med )%>%
  summarise( val_final_fin = sum( val_final_fin, na.rm = TRUE ))%>%
  arrange( time, desc(val_final_fin) )%>%
  ungroup()%>%
  group_by( time )%>%
  mutate( pos = row_number() )%>%
  filter( pos <= 30 )%>%
  ggplot( aes( x = time, y = pos, col = dummy_med, group = nom_curso ))+
  geom_line(size = 1.1)+
  ylim( c(20,1))+
  labs( x = "Periodo", y = "Posição Gastos", color = "" )+
  theme_bw()+
  theme( legend.position = "bottom",
         axis.text   = element_text( size = 10 ),
         axis.title  = element_text( size = 10 ),
         legend.text = element_text( size = 10 ))+
  scale_color_manual(values = c("black", "grey"))+
  #scale_y_continuous(breaks=c(20,15,10,5,1), labels = c("1", "5","10","15","20"))+
  scale_x_continuous(breaks=c(1:6),
                     labels=c("1 tri\n2019", "2 tri\n2019",
                              "1 tri\n2020", "2 tri\n2020",
                              "1 tri\n2021", "2 tri\n2021"))


ggsave( filename = paste0( path_out_pic, "plot_pos_gasto_fies.pdf"),
        plot     = posi_gastos,
        units    = "px", width = 1200, height = 1000 )
  
base_val_fin_fies %>%
  group_by( time )%>%
  summarise( val_final_fin = sum( val_final_fin, na.rm = TRUE ))

resumo_valores <-
  base_val_fin_fies %>%
  group_by( time, dummy_med )%>%
  summarise( val_final_fin = sum( val_final_fin, na.rm = TRUE ))%>%
  ungroup()%>%
  group_by( time )%>%
  mutate( perc = val_final_fin/sum(val_final_fin) )

ggplot( data = resumo_valores,
        aes( x = time, y = perc, fill = dummy_med ))+
  geom_area()

base_medicina_insc <-
  filter( base_val_fin_fies, nom_curso == "MEDICINA" )

# Base com conceitos dos cursos ENAMED

path_files <- "./data/raw/ENAMED/DADOS/Enade/"

files <- list.files( path_files )

df_enamed <-
  map( paste0( path_files, files ), read.csv, sep = ";")


df_prof_curso <- 
  df_enamed[[17]] %>%
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
                                  perc_prof >= 0.9                     ~ "5") )

base_cruz_med <-
  left_join( base_medicina_insc, df_prof_curso, by = c("cod_curso"="CO_CURSO") )

base_perc_fin <-
  base_cruz_med %>%
  group_by( time, class_prof )%>%
  summarise( val_final_fin = sum( val_final_fin, na.rm = TRUE ))%>%
  ungroup()%>%
  group_by( time )%>%
  mutate( perc = val_final_fin/sum( val_final_fin, na.rm = TRUE ),
          class_prof = if_else( is.na(class_prof),"NA",class_prof))




plot_perc_val_disp <-
  base_perc_fin %>%
  ggplot( aes( x = time, y = perc, fill = class_prof ) )+
  geom_area()+
  geom_hline( yintercept = 0.5, col = "red", linetype = 2 )+
  theme_bw()+
  theme( legend.position = "bottom",
         axis.text = element_text( size = 10 ),
         axis.title = element_text( size = 10 ))+
  labs( x = NULL, y = "Percentual", fill = "Classe\nProficiência" )+
  scale_x_continuous(breaks=c(1:6),
                     labels=c("1 tri\n2019", "2 tri\n2019",
                              "1 tri\n2020", "2 tri\n2020",
                              "1 tri\n2021", "2 tri\n2021"))+
  scale_fill_discrete( palette = c("#C7C7C7","#878787","#525252","#2E2E2E","#121212","#9E7B7B"))

plot_val_tot_disp <-
  base_perc_fin %>%
  ggplot( aes( x = time, y = val_final_fin/1e6, fill = class_prof ) )+
  geom_area()+
  theme_bw()+
  theme( legend.position = "bottom",
         axis.text = element_text( size = 10 ),
         axis.title = element_text( size = 10 ))+
  labs( x = NULL, y = "Milhões (R$)", fill = "Classe\nProficiência" )+
  scale_x_continuous(breaks=c(1:6),
                     labels=c( "1 tri\n2019", "2 tri\n2019",
                               "1 tri\n2020", "2 tri\n2020",
                               "1 tri\n2021", "2 tri\n2021"))+
  scale_fill_discrete( palette = c("#C7C7C7","#878787","#525252","#2E2E2E","#121212","#9E7B7B"))


ggsave( paste0(path_out_pic, "valor_med_dip_fies.pdf"),
        plot = plot_val_tot_disp, units = "px",
        width = 1200, height = 1000 )

ggsave( paste0(path_out_pic, "valor_perc_dip_fies.pdf"),
        plot = plot_perc_val_disp, units = "px",
        width = 1200, height = 1000 )

# Grafico Mostrando Percentuais
base_tot <-
  bind_rows(
  base_val_fin_fies %>%
  group_by( time )%>%
  summarise( val_final_fin = sum( val_final_fin, na.rm = TRUE ))%>%
  mutate( desc = "1. Total" ),
  # Medidcina
  base_perc_fin %>%
  group_by( time )%>%
  summarise( val_final_fin = sum( val_final_fin ))%>%
  mutate( desc = "2. Medicina"),
  # Medicina Conceitos 1 e 2
  base_perc_fin %>%
  filter( class_prof %in% c("1","2"))%>%
  group_by( time )%>%
  summarise( val_final_fin = sum( val_final_fin ))%>%
  mutate( desc = "3. Med. Conceito 1 & 2") )

base_perc <- base_tot %>%
  pivot_wider( id_cols = "time",
               names_from = "desc",
               values_from = "val_final_fin")%>%
  mutate( `2. Medicina` = `2. Medicina`/`1. Total`,
          `3. Med. Conceito 1 & 2` = `3. Med. Conceito 1 & 2`/`1. Total`,
          `1. Total` = 1 )%>%
  pivot_longer( cols = !"time",
                names_to = "desc",
                values_to = "perc")

base_comp <-
  left_join( base_tot, base_perc )

ggplot( base_comp, aes(x = time, y = val_final_fin/1e6, fill = desc ) )+
  geom_bar( position="dodge", stat="identity")+
  geom_text(
    aes(x = time, y = val_final_fin/1e6, label = paste0(round(perc*100,2),"%"), 
        group = desc), 
    hjust = +0.5, size = 2.5,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  ) +
  theme_bw()+
  theme( legend.position = "bottom" )

# Custo / Eficiência

base_calc_prof <-
  base_cruz_med %>%
  mutate( n_prof = n*perc_prof,
          # Valor Medio
          val_med      = val_final_fin/n ,
          val_med_prof = val_final_fin/n_prof,
          time_per     = paste0( ano, "/", sem )) 

plot_gasto <-
  base_calc_prof %>%
  filter( !is.na( n_prof ) )%>%
  ggplot(  )+
  geom_boxplot( aes( y = val_med/1e6, x = class_prof, group = class_prof ) )+
  facet_wrap( ~time_per )+
  labs( x = "Conceito", y = "Val. Med. aluno (m)")+
  theme( axis.text = element_text( size = 10 ),
         axis.title = element_text( size = 10 ))+
  theme_bw()

plot_gasto_eff <-
  base_calc_prof %>%
  filter( !is.na( n_prof ) )%>%
  ggplot(  )+
  geom_boxplot( aes( y = val_med_prof/1e6, x = class_prof, group = class_prof ) )+
  facet_wrap( ~time_per )+
  labs( x = "Conceito", y = "Val. Med. aluno prof. (m)")+
  theme( axis.text = element_text( size = 10 ),
         axis.title = element_text( size = 10 ))+
  theme_bw()

ggsave( paste0("D:/Analise_ENAMED/pics/gasto.pdf"),
        plot = plot_gasto, units = "px",
        width = 1200, height = 1000 )

ggsave( paste0("D:/Analise_ENAMED/pics/gasto_eff.pdf"),
        plot = plot_gasto_eff, units = "px",
        width = 1200, height = 1000 )

# Construção da Tabela

# Vl med por aluno
base_calc_prof %>%
  group_by( time_per, class_prof )%>%
  summarise( med_val_prof = mean(val_med,na.rm = TRUE )) %>%
  pivot_wider( id_cols = "time_per",
               names_from = "class_prof",
               values_from = "med_val_prof")%>%
  xtable::xtable()

# Vl med por aluno prof
base_calc_prof %>%
  group_by( time_per, class_prof )%>%
  summarise( med_val_prof = mean(val_med_prof,na.rm = TRUE )) %>%
  pivot_wider( id_cols = "time_per",
               names_from = "class_prof",
               values_from = "med_val_prof")%>%
  mutate( var5 = `2`/`5`-1,
          var4 = `2`/`4`-1,
          var3 = `2`/`3`-1)

resumo_prof <-
  df_prof_curso %>% 
  mutate( n_prof = n*perc_prof ) %>%
  group_by( class_prof )%>%
  summarise( cursos  = n(),
             n       = sum(n),
             n_prof  = sum( n_prof ))


base_calc_prof %>%
  group_by( class_prof )%>%
  summarise( val_gasto_tot = sum(val_final_fin,na.rm = TRUE ),
             val_gasto_med = mean(val_final_fin,na.rm = TRUE ))%>%
  filter( !is.na(class_prof))%>%
  left_join( resumo_prof, by = c("class_prof"))%>%
  select( class_prof, cursos, val_gasto_tot, n_prof )%>%
  mutate( custo_aluno = val_gasto_tot/n_prof )

# Salva dado de Valor Curso

path_out_dta <-
  "./data/inter/"

saveRDS( base_val_cursos_fies, paste0(path_out_dta,"val_cursos_fies.rds") )
