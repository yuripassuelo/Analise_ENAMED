
# Analise ENAMED

Diretório que contém procedimentos códigos e dados utilizados na analise do ENAMED 2025, para baixar o diretório e todo seu conteúdo utilize o comando `git clone`

```{bash}
git clone https://github.com/yuripassuelo/Analise_ENAMED.git [<local_directory_name>] 
```

# Estrutura

```{bash}
├───data
│   ├───inter
│   └───raw
├───pdf
├───pics
└───src
```

Na pasta `\src` estão os scripts de processamento dos dados e analises, scripts com prefixo `00` são scripts que fazem processamento dos dados enquanto `01` são relativos s analises e modelos.

A pasta `data`  apresenta duas subpastas, a pasta `raw` contém os dados brutos das fontes do FIES, ENAMED, e-mec e CENSUP, enquanto a pasta `inter` apresenta os dados processados e finais disponíveis para a modelagem

A pasta `pdf` contém o working paper do projeto e por último a pasta `pics` contém todas as imagens geradas no artigo.


# Versão do R e pacotes

```{R}
R version 4.4.2 (2024-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26200)

Matrix products: default


locale:
[1] LC_COLLATE=Portuguese_Brazil.utf8  LC_CTYPE=Portuguese_Brazil.utf8   
[3] LC_MONETARY=Portuguese_Brazil.utf8 LC_NUMERIC=C                      
[5] LC_TIME=Portuguese_Brazil.utf8    

time zone: America/Sao_Paulo
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] data.table_1.16.2 factoextra_1.0.7  cluster_2.1.6     sf_1.0-19        
 [5] geobr_1.9.1       stargazer_5.2.3   lubridate_1.9.3   forcats_1.0.0    
 [9] stringr_1.5.1     dplyr_1.1.4       purrr_1.0.2       readr_2.1.5      
[13] tidyr_1.3.1       tibble_3.2.1      ggplot2_4.0.0     tidyverse_2.0.0  

loaded via a namespace (and not attached):
 [1] gtable_0.3.6       compiler_4.4.2     Rcpp_1.0.13-1     
 [4] tidyselect_1.2.1   dichromat_2.0-0.1  scales_1.4.0      
 [7] R6_2.5.1           generics_0.1.3     classInt_0.4-11   
[10] curl_6.0.1         ggrepel_0.9.6      units_0.8-5       
[13] DBI_1.2.3          pillar_1.10.1      RColorBrewer_1.1-3
[16] tzdb_0.4.0         rlang_1.1.4        stringi_1.8.4     
[19] S7_0.2.0           timechange_0.3.0   cli_3.6.3         
[22] withr_3.0.2        magrittr_2.0.3     class_7.3-22      
[25] grid_4.4.2         rstudioapi_0.17.1  hms_1.1.3         
[28] lifecycle_1.0.4    vctrs_0.6.5        KernSmooth_2.23-24
[31] proxy_0.4-27       glue_1.8.0         farver_2.1.2      
[34] e1071_1.7-16       tools_4.4.2        pkgconfig_2.0.3   
```