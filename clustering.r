library(specmine)

MTBLS654_intra=read_dataset_csv(filename.data="concentracao_metabolitos.csv",
                                filename.meta="metadados.csv", type="concentrations", description =
                                  "Concentrations data taken from MetaboLights study MTBLS654", label.x = "Names", label.values =
                                  "Concentrations", format="col")

######################
# Tal como para a redução de dimensionalidade, a análise de clustering foi feita utilizando as funções do specmine.
# Para a análise de clustering hierárquico, foram usados diferentes métodos de agregamento e medidas de distância para a construção dos clusters e dendogramas.

clust_spe_hc_samples_comp=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "complete")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_comp, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: amostras, método de agregamento: complete")

clust_spe_hc_samples_ward=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "ward")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_ward, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: amostras, método de agregamento: ward")

clust_spe_hc_samples_sin=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "single")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_sin, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: amostras, método de agregamento: single")

# Da análise dos dendogramas de clustering hierárquico em função do tipo de amostra, é possível observar que amostras do mesmo tipo e com tempos de fermentação semelhantes têm tendência a agrupar nos mesmos clusters.
# Usando a distância de Spearman e método de agregamento complete, a separação por tipo de amostra é evidente, sendo que se verifica que amostras do mesmo tipo agrupam-se em clusters de tamanho médio, e que no geral esses clusters contêm amostras com tempos de fermentação próximos (p. ex. verifica-se a existência de um cluster com 8 amostras MK de 0 a 8 dias de fermentação, um com 5 amostras SK de 0 a 5 dias de fermentação e um com 4 amostras SK de 16 a 20 dias de fermentação).
# Usando métodos de agregamento diferentes, como p. ex. single ou ward, continua a verificar-se a formação de clusters por tipo de amostra, no entanto com o método ward as amostras do mesmo tipo agrupam-se em clusters de tamanho mais pequeno, e com o método single a formação de clusters com amostras com tempos de fermentação próximos não é tão evidente.


clust_pear_hc_samples_comp=clustering(MTBLS654_intra, method = "hc", distance = "pearson", type = "samples", num.clusters = 4, clustMethod = "complete")
dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_samples_comp, "Fermentation_days", title = "Dist. Pearson, tipo: amostras, método de agregamento: complete")

clust_euc_hc_samples_comp=clustering(MTBLS654_intra, method = "hc", distance = "euclidean", type = "samples", num.clusters = 4, clustMethod = "complete")
dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_samples_comp, "Fermentation_days", title = "Dist. Euclidiana, tipo: amostras, método de agregamento: complete")

# Analisando o dendograma de clustering hierárquico em função do número de dias de fermentação, usando a distância de Pearson e o método de agregamento complete, há uma clara separação das amostras de tempo de 0 a 12, de 16 a 20 e de 30 a 40 dias.
# Também se observa que amostras do mesmo tipo com tempos de fermentação próximos têm tendência a agrupar nos mesmos clusters. No caso de amostras de kimchi com jeotgal, verifica-se um padrão ligeiramente diferente relativamente às amostras sem jeotgal, as quais têm tendência a agrupar com amostras sem jeotgal com tempos de fermentação ligeiramente diferentes, refletindo assim as diferenças nas concentrações de metabolitos observadas anteriormente.
# Com a distância euclidiana, o agrupamento de amostras por tempo de fermentação não se torna tão evidente como no clustering anterior, embora continue a haver uma separação entre os tempos de fermentação mais elevados (25 a 40 dias) e os restantes.

# Da análise de clustering k-means, verificou-se que qualquer que seja a medida de distância ou o método de agregamento usado, os clusters resultantes são sempre os mesmos.  
# Para 4 centros de cluster, os resultados obtidos foram os seguintes:
clust_euc_km_samples_avg=clustering(MTBLS654_intra, method = "kmeans", distance = "euclidean", type = "samples", num.clusters = 4, clustMethod = "average")
kmeans_plot(MTBLS654_intra, clust_euc_km_samples_avg)
kmeans_result_df(clust_euc_km_samples_avg)
#os clusteres formados não parecem denotar separações significativas das amostras, seja por tipo de amostra, seja por tempo de fermentação. Portanto, o clustering hierárquico para este contexto parece fornecer mais informações pretinentes em relação à diferenciação de concentrações dos metabolitos nas diferentes amostras. 



