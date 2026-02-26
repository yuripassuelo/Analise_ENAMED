
library( tidyverse )
library( ggpubr )
library( cowplot )
library( data.table )
# Notas de Corte

path_dados_oferta <-
  "./data/raw/FIES_SISU/Oferta/"

path_dados_inscricao <-
  "./data/raw/FIES_SISU/Inscricao/"

path_inter <-
  "./data/inter/"


notas_fies <-
  read.csv( file = paste0( path_inter, "result_notas_fies.csv"), sep = ",")

# Pega FIES 2020/1
dta_fies <-
  fread( paste0( path_dados_oferta, "fies_oferta_2020_1.csv"),
         encoding = "Latin-1")

filt_fies <-
  filter( dta_fies, `Nome do Curso` == "MEDICINA")%>%
  select( `Código e-MEC da IES`, `Código do Curso`, `Nota de Corte Grupo Preferência`) %>%
  rename( CO_IES       = `Código e-MEC da IES`,
          CO_IES_CURSO = `Código do Curso`,
          NU_NOTACORTE = `Nota de Corte Grupo Preferência` )%>%
  mutate( tipo = "FIES" )

# Pega SISU 2020/1
dta_sisu <-
  fread( paste0( path_dados_inscricao, "ListagemChamadaRegular_2020-1.csv") )


filt_sisu <- 
  filter( dta_sisu, NO_CURSO == "MEDICINA" & DS_MOD_CONCORRENCIA == "Ampla concorrência" )%>%
  group_by( CO_IES, CO_IES_CURSO, NU_NOTACORTE )%>%
  summarise( )%>%
  mutate( NU_NOTACORTE = as.numeric( str_replace( NU_NOTACORTE, ",", ".")),
          tipo         = "SISU" )


dta_comb <- bind_rows( filt_fies, filt_sisu )

#write.csv( dta_comb, paste0( path_fies, "notas_corte.csv"), row.names = FALSE )
saveRDS( dta_comb, paste0( path_inter, "notas_corte.rds") )


# Plots
dta_comb %>%
  ggplot( aes( x = NU_NOTACORTE, col = tipo, fill = tipo ) )+
  geom_histogram( alpha = 0.2 )+
  geom_density( )+
  theme_bw()+
  theme( legend.position = "bottom" )
  
# Plot completo
  
# 1. histogram plot
phist <- gghistogram(
  dta_comb, x = "NU_NOTACORTE", 
  add = "mean", rug = TRUE,
  fill = "tipo", palette = c("#00AFBB", "#E7B800"),
  xlab = "Nota de Corte", ylab = "Contagem"
)

# 2. density plot com eixo y na direita

pdensity <- ggdensity(
  dta_comb, x = "NU_NOTACORTE", 
  color= "tipo", palette = c("#00AFBB", "#E7B800"),
  alpha = 0, xlab = "Nota de Corte", ylab = "Densidade"
) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), position = "right")  +
  theme_half_open(11, rel_small = 1) +
  rremove("x.axis")+
  rremove("xlab") +
  rremove("x.text") +
  rremove("x.ticks") +
  rremove("legend")

# 3. Alinha os plots
aligned_plots <- align_plots(phist, pdensity, align="hv", axis="tblr")

plot_nota_corte <-
  ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])+
  labs( x = "Nota de corte")

path_out <-
  "./pics/"

ggsave( plot = plot_nota_corte, filename = paste0( path_out, "plot_nota_corte.pdf"),
        units = "px", width = 1200, height = 1000 )
