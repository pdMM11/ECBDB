library(specmine)

MTBLS654_intra=read_dataset_csv(filename.data="concentracao_metabolitos.csv",
                                filename.meta="metadados.csv", type="concentrations", description =
                                  "Concentrations data taken from MetaboLights study MTBLS654", label.x = "Names", label.values =
                                  "Concentrations", format="col")

######################
# Tal como para a redu��o de dimensionalidade, a an�lise de clustering foi feita utilizando as fun��es do specmine.
# Para a an�lise de clustering hier�rquico, foram usados diferentes m�todos de agregamento e medidas de dist�ncia para a constru��o dos clusters e dendogramas.

clust_spe_hc_samples_comp=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "complete")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_comp, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: amostras, m�todo de agregamento: complete")

clust_spe_hc_samples_ward=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "ward")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_ward, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: amostras, m�todo de agregamento: ward")

clust_spe_hc_samples_sin=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "single")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_sin, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: amostras, m�todo de agregamento: single")

# Da an�lise dos dendogramas de clustering hier�rquico em fun��o do tipo de amostra, � poss�vel observar que amostras do mesmo tipo e com tempos de fermenta��o semelhantes t�m tend�ncia a agrupar nos mesmos clusters.
# Usando a dist�ncia de Spearman e m�todo de agregamento complete, a separa��o por tipo de amostra � evidente, sendo que se verifica que amostras do mesmo tipo agrupam-se em clusters de tamanho m�dio, e que no geral esses clusters cont�m amostras com tempos de fermenta��o pr�ximos (p. ex. verifica-se a exist�ncia de um cluster com 8 amostras MK de 0 a 8 dias de fermenta��o, um com 5 amostras SK de 0 a 5 dias de fermenta��o e um com 4 amostras SK de 16 a 20 dias de fermenta��o).
# Usando m�todos de agregamento diferentes, como p. ex. single ou ward, continua a verificar-se a forma��o de clusters por tipo de amostra, no entanto com o m�todo ward as amostras do mesmo tipo agrupam-se em clusters de tamanho mais pequeno, e com o m�todo single a forma��o de clusters com amostras com tempos de fermenta��o pr�ximos n�o � t�o evidente.


clust_pear_hc_samples_comp=clustering(MTBLS654_intra, method = "hc", distance = "pearson", type = "samples", num.clusters = 4, clustMethod = "complete")
dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_samples_comp, "Fermentation_days", title = "Dist. Pearson, tipo: amostras, m�todo de agregamento: complete")

clust_euc_hc_samples_comp=clustering(MTBLS654_intra, method = "hc", distance = "euclidean", type = "samples", num.clusters = 4, clustMethod = "complete")
dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_samples_comp, "Fermentation_days", title = "Dist. Euclidiana, tipo: amostras, m�todo de agregamento: complete")

# Analisando o dendograma de clustering hier�rquico em fun��o do n�mero de dias de fermenta��o, usando a dist�ncia de Pearson e o m�todo de agregamento complete, h� uma clara separa��o das amostras de tempo de 0 a 12, de 16 a 20 e de 30 a 40 dias.
# Tamb�m se observa que amostras do mesmo tipo com tempos de fermenta��o pr�ximos t�m tend�ncia a agrupar nos mesmos clusters. No caso de amostras de kimchi com jeotgal, verifica-se um padr�o ligeiramente diferente relativamente �s amostras sem jeotgal, as quais t�m tend�ncia a agrupar com amostras sem jeotgal com tempos de fermenta��o ligeiramente diferentes, refletindo assim as diferen�as nas concentra��es de metabolitos observadas anteriormente.
# Com a dist�ncia euclidiana, o agrupamento de amostras por tempo de fermenta��o n�o se torna t�o evidente como no clustering anterior, embora continue a haver uma separa��o entre os tempos de fermenta��o mais elevados (25 a 40 dias) e os restantes.

# Da an�lise de clustering k-means, verificou-se que qualquer que seja a medida de dist�ncia ou o m�todo de agregamento usado, os clusters resultantes s�o sempre os mesmos.  
# Para 4 centros de cluster, os resultados obtidos foram os seguintes:
clust_euc_km_samples_avg=clustering(MTBLS654_intra, method = "kmeans", distance = "euclidean", type = "samples", num.clusters = 4, clustMethod = "average")
kmeans_plot(MTBLS654_intra, clust_euc_km_samples_avg)
kmeans_result_df(clust_euc_km_samples_avg)
#os clusteres formados n�o parecem denotar separa��es significativas das amostras, seja por tipo de amostra, seja por tempo de fermenta��o. Portanto, o clustering hier�rquico para este contexto parece fornecer mais informa��es pretinentes em rela��o � diferencia��o de concentra��es dos metabolitos nas diferentes amostras. 



