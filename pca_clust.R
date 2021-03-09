pca_normal=pca_analysis_dataset(MTBLS654_intra)
pca_robusto=pca_robust(MTBLS654_intra)

pca_importance(pca_normal)
pca_importance(pca_robusto)

pca_screeplot(pca_normal)
pca_screeplot(pca_robusto)

pca_scoresplot2D(MTBLS654_intra, pca_normal, "Factor.Value.Kimchi.", ellipses = TRUE, pallette = "Paired", labels = FALSE)
pca_scoresplot2D(MTBLS654_intra, pca_normal, "Fermentation_days", ellipses = TRUE, pallette = "Paired", labels = FALSE)
pca_scoresplot2D(MTBLS654_intra, pca_robusto, "Factor.Value.Kimchi.", ellipses = TRUE, pallette = "Paired", labels = FALSE)
pca_scoresplot2D(MTBLS654_intra, pca_robusto, "Fermentation_days", ellipses = TRUE, pallette = "Paired", labels = FALSE)

pca_scoresplot3D(MTBLS654_intra, pca_normal, "Factor.Value.Kimchi.")
pca_scoresplot3D(MTBLS654_intra, pca_normal, "Fermentation_days")
pca_scoresplot3D(MTBLS654_intra, pca_robusto, "Factor.Value.Kimchi.")
pca_scoresplot3D(MTBLS654_intra, pca_robusto, "Fermentation_days")

pca_scoresplot3D_rgl(MTBLS654_intra, pca_normal, "Factor.Value.Kimchi.")
pca_scoresplot3D_rgl(MTBLS654_intra, pca_normal, "Fermentation_days")
pca_scoresplot3D_rgl(MTBLS654_intra, pca_robusto, "Factor.Value.Kimchi.")
pca_scoresplot3D_rgl(MTBLS654_intra, pca_robusto, "Fermentation_days")

pca_biplot(MTBLS654_intra, pca_normal, colors="Factor.Value.Kimchi.")
pca_biplot(MTBLS654_intra, pca_normal, colors="Fermentation_days")
pca_biplot(MTBLS654_intra, pca_robusto, colors="Factor.Value.Kimchi.")
pca_biplot(MTBLS654_intra, pca_robusto, colors="Fermentation_days")

pca_biplot3D(MTBLS654_intra, pca_normal, "Factor.Value.Kimchi.")
pca_biplot3D(MTBLS654_intra, pca_normal, "Fermentation_days")
pca_biplot3D(MTBLS654_intra, pca_robusto, "Factor.Value.Kimchi.")
pca_biplot3D(MTBLS654_intra, pca_robusto, "Fermentation_days")

pca_pairs_plot(MTBLS654_intra, pca_normal, "Factor.Value.Kimchi.")
pca_pairs_plot(MTBLS654_intra, pca_normal, "Fermentation_days")
pca_pairs_plot(MTBLS654_intra, pca_robusto, "Factor.Value.Kimchi.")
pca_pairs_plot(MTBLS654_intra, pca_robusto, "Fermentation_days")

pca_kmeans_plot2D(MTBLS654_intra, pca_normal,num.clusters = 4)
pca_kmeans_plot2D(MTBLS654_intra, pca_robusto,num.clusters = 4)

pca_kmeans_plot3D(MTBLS654_intra, pca_normal,num.clusters = 4)
pca_kmeans_plot3D(MTBLS654_intra, pca_robusto,num.clusters = 4)

pca_pairs_kmeans_plot(MTBLS654_intra, pca_normal, num.clusters = 4)
pca_pairs_kmeans_plot(MTBLS654_intra, pca_robusto, num.clusters = 4)

######################

#todos os clusters Hierarquical clustering, para as samples, com metodo de aglomeração completo
clust_euc_hc_samples_comp=clustering(MTBLS654_intra, method = "hc", distance = "euclidean", type = "samples", num.clusters = 4, clustMethod = "complete")
clust_man_hc_samples_comp=clustering(MTBLS654_intra, method = "hc", distance = "manhattan", type = "samples", num.clusters = 4, clustMethod = "complete")
clust_pear_hc_samples_comp=clustering(MTBLS654_intra, method = "hc", distance = "pearson", type = "samples", num.clusters = 4, clustMethod = "complete")
clust_spe_hc_samples_comp=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "complete")

#todos os clusters Hierarquical clustering, para as variáveis, com metodo de aglomeração completo
clust_euc_hc_var_comp=clustering(MTBLS654_intra, method = "hc", distance = "euclidean", type = "variables", num.clusters = 4, clustMethod = "complete")
clust_man_hc_var_comp=clustering(MTBLS654_intra, method = "hc", distance = "manhattan", type = "variables", num.clusters = 4, clustMethod = "complete")
clust_pear_hc_var_comp=clustering(MTBLS654_intra, method = "hc", distance = "pearson", type = "variables", num.clusters = 4, clustMethod = "complete")
clust_spe_hc_var_comp=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "variables", num.clusters = 4, clustMethod = "complete")

#todos os clusters Hierarquical clustering, para as samples, com metodo de aglomeração ward
clust_euc_hc_samples_ward=clustering(MTBLS654_intra, method = "hc", distance = "euclidean", type = "samples", num.clusters = 4, clustMethod = "ward")
clust_man_hc_samples_ward=clustering(MTBLS654_intra, method = "hc", distance = "manhattan", type = "samples", num.clusters = 4, clustMethod = "ward")
clust_pear_hc_samples_ward=clustering(MTBLS654_intra, method = "hc", distance = "pearson", type = "samples", num.clusters = 4, clustMethod = "ward")
clust_spe_hc_samples_ward=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "ward")

#todos os clusters Hierarquical clustering, para as variáveis, com metodo de aglomeração ward
clust_euc_hc_var_ward=clustering(MTBLS654_intra, method = "hc", distance = "euclidean", type = "variables", num.clusters = 4, clustMethod = "ward")
clust_man_hc_var_ward=clustering(MTBLS654_intra, method = "hc", distance = "manhattan", type = "variables", num.clusters = 4, clustMethod = "ward")
clust_pear_hc_var_ward=clustering(MTBLS654_intra, method = "hc", distance = "pearson", type = "variables", num.clusters = 4, clustMethod = "ward")
clust_spe_hc_var_ward=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "variables", num.clusters = 4, clustMethod = "ward")

#todos os clusters Hierarquical clustering, para as samples, com metodo de aglomeração single
clust_euc_hc_samples_sin=clustering(MTBLS654_intra, method = "hc", distance = "euclidean", type = "samples", num.clusters = 4, clustMethod = "single")
clust_man_hc_samples_sin=clustering(MTBLS654_intra, method = "hc", distance = "manhattan", type = "samples", num.clusters = 4, clustMethod = "single")
clust_pear_hc_samples_sin=clustering(MTBLS654_intra, method = "hc", distance = "pearson", type = "samples", num.clusters = 4, clustMethod = "single")
clust_spe_hc_samples_sin=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "single")

#todos os clusters Hierarquical clustering, para as variáveis, com metodo de aglomeração single
clust_euc_hc_var_sin=clustering(MTBLS654_intra, method = "hc", distance = "euclidean", type = "variables", num.clusters = 4, clustMethod = "single")
clust_man_hc_var_sin=clustering(MTBLS654_intra, method = "hc", distance = "manhattan", type = "variables", num.clusters = 4, clustMethod = "single")
clust_pear_hc_var_sin=clustering(MTBLS654_intra, method = "hc", distance = "pearson", type = "variables", num.clusters = 4, clustMethod = "single")
clust_spe_hc_var_sin=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "variables", num.clusters = 4, clustMethod = "single")

#todos os clusters Hierarquical clustering, para as samples, com metodo de aglomeração average
clust_euc_hc_samples_avg=clustering(MTBLS654_intra, method = "hc", distance = "euclidean", type = "samples", num.clusters = 4, clustMethod = "average")
clust_man_hc_samples_avg=clustering(MTBLS654_intra, method = "hc", distance = "manhattan", type = "samples", num.clusters = 4, clustMethod = "average")
clust_pear_hc_samples_avg=clustering(MTBLS654_intra, method = "hc", distance = "pearson", type = "samples", num.clusters = 4, clustMethod = "average")
clust_spe_hc_samples_avg=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "average")

#todos os clusters Hierarquical clustering, para as variáveis, com metodo de aglomeração average
clust_euc_hc_var_avg=clustering(MTBLS654_intra, method = "hc", distance = "euclidean", type = "variables", num.clusters = 4, clustMethod = "average")
clust_man_hc_var_avg=clustering(MTBLS654_intra, method = "hc", distance = "manhattan", type = "variables", num.clusters = 4, clustMethod = "average")
clust_pear_hc_var_avg=clustering(MTBLS654_intra, method = "hc", distance = "pearson", type = "variables", num.clusters = 4, clustMethod = "average")
clust_spe_hc_var_avg=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "variables", num.clusters = 4, clustMethod = "average")

#todos os clusters Hierarquical clustering, para as samples, com metodo de aglomeração mcquitty
clust_euc_hc_samples_mcq=clustering(MTBLS654_intra, method = "hc", distance = "euclidean", type = "samples", num.clusters = 4, clustMethod = "mcquitty")
clust_man_hc_samples_mcq=clustering(MTBLS654_intra, method = "hc", distance = "manhattan", type = "samples", num.clusters = 4, clustMethod = "mcquitty")
clust_pear_hc_samples_mcq=clustering(MTBLS654_intra, method = "hc", distance = "pearson", type = "samples", num.clusters = 4, clustMethod = "mcquitty")
clust_spe_hc_samples_mcq=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "mcquitty")

#todos os clusters Hierarquical clustering, para as variáveis, com metodo de aglomeração mcquitty
clust_euc_hc_var_mcq=clustering(MTBLS654_intra, method = "hc", distance = "euclidean", type = "variables", num.clusters = 4, clustMethod = "mcquitty")
clust_man_hc_var_mcq=clustering(MTBLS654_intra, method = "hc", distance = "manhattan", type = "variables", num.clusters = 4, clustMethod = "mcquitty")
clust_pear_hc_var_mcq=clustering(MTBLS654_intra, method = "hc", distance = "pearson", type = "variables", num.clusters = 4, clustMethod = "mcquitty")
clust_spe_hc_var_mcq=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "variables", num.clusters = 4, clustMethod = "mcquitty")

#todos os clusters Hierarquical clustering, para as samples, com metodo de aglomeração median
clust_euc_hc_samples_me=clustering(MTBLS654_intra, method = "hc", distance = "euclidean", type = "samples", num.clusters = 4, clustMethod = "median")
clust_man_hc_samples_me=clustering(MTBLS654_intra, method = "hc", distance = "manhattan", type = "samples", num.clusters = 4, clustMethod = "median")
clust_pear_hc_samples_me=clustering(MTBLS654_intra, method = "hc", distance = "pearson", type = "samples", num.clusters = 4, clustMethod = "median")
clust_spe_hc_samples_me=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "median")

#todos os clusters Hierarquical clustering, para as variáveis, com metodo de aglomeração median
clust_euc_hc_var_me=clustering(MTBLS654_intra, method = "hc", distance = "euclidean", type = "variables", num.clusters = 4, clustMethod = "median")
clust_man_hc_var_me=clustering(MTBLS654_intra, method = "hc", distance = "manhattan", type = "variables", num.clusters = 4, clustMethod = "median")
clust_pear_hc_var_me=clustering(MTBLS654_intra, method = "hc", distance = "pearson", type = "variables", num.clusters = 4, clustMethod = "median")
clust_spe_hc_var_me=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "variables", num.clusters = 4, clustMethod = "median")

#todos os clusters Hierarquical clustering, para as samples, com metodo de aglomeração centroid
clust_euc_hc_samples_cen=clustering(MTBLS654_intra, method = "hc", distance = "euclidean", type = "samples", num.clusters = 4, clustMethod = "centroid")
clust_man_hc_samples_cen=clustering(MTBLS654_intra, method = "hc", distance = "manhattan", type = "samples", num.clusters = 4, clustMethod = "centroid")
clust_pear_hc_samples_cen=clustering(MTBLS654_intra, method = "hc", distance = "pearson", type = "samples", num.clusters = 4, clustMethod = "centroid")
clust_spe_hc_samples_cen=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "centroid")

#todos os clusters Hierarquical clustering, para as variáveis, com metodo de aglomeração centroid
clust_euc_hc_var_cen=clustering(MTBLS654_intra, method = "hc", distance = "euclidean", type = "variables", num.clusters = 4, clustMethod = "centroid")
clust_man_hc_var_cen=clustering(MTBLS654_intra, method = "hc", distance = "manhattan", type = "variables", num.clusters = 4, clustMethod = "centroid")
clust_pear_hc_var_cen=clustering(MTBLS654_intra, method = "hc", distance = "pearson", type = "variables", num.clusters = 4, clustMethod = "centroid")
clust_spe_hc_var_cen=clustering(MTBLS654_intra, method = "hc", distance = "spearman", type = "variables", num.clusters = 4, clustMethod = "centroid")

#############################

#todos os clusters Kmeans, para as samples, com metodo de aglomeração completo
clust_euc_km_samples_comp=clustering(MTBLS654_intra, method = "kmeans", distance = "euclidean", type = "samples", num.clusters = 4, clustMethod = "complete")
clust_man_km_samples_comp=clustering(MTBLS654_intra, method = "kmeans", distance = "manhattan", type = "samples", num.clusters = 4, clustMethod = "complete")
clust_pear_km_samples_comp=clustering(MTBLS654_intra, method = "kmeans", distance = "pearson", type = "samples", num.clusters = 4, clustMethod = "complete")
clust_spe_km_samples_comp=clustering(MTBLS654_intra, method = "kmeans", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "complete")

#todos os clusters Kmeans, para as variáveis, com metodo de aglomeração completo
clust_euc_km_var_comp=clustering(MTBLS654_intra, method = "kmeans", distance = "euclidean", type = "variables", num.clusters = 4, clustMethod = "complete")
clust_man_km_var_comp=clustering(MTBLS654_intra, method = "kmeans", distance = "manhattan", type = "variables", num.clusters = 4, clustMethod = "complete")
clust_pear_km_var_comp=clustering(MTBLS654_intra, method = "kmeans", distance = "pearson", type = "variables", num.clusters = 4, clustMethod = "complete")
clust_spe_km_var_comp=clustering(MTBLS654_intra, method = "kmeans", distance = "spearman", type = "variables", num.clusters = 4, clustMethod = "complete")

#todos os clusters Kmeans, para as samples, com metodo de aglomeração ward
clust_euc_km_samples_ward=clustering(MTBLS654_intra, method = "kmeans", distance = "euclidean", type = "samples", num.clusters = 4, clustMethod = "ward")
clust_man_km_samples_ward=clustering(MTBLS654_intra, method = "kmeans", distance = "manhattan", type = "samples", num.clusters = 4, clustMethod = "ward")
clust_pear_km_samples_ward=clustering(MTBLS654_intra, method = "kmeans", distance = "pearson", type = "samples", num.clusters = 4, clustMethod = "ward")
clust_spe_km_samples_ward=clustering(MTBLS654_intra, method = "kmeans", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "ward")

#todos os clusters Kmeans, para as variáveis, com metodo de aglomeração ward
clust_euc_km_var_ward=clustering(MTBLS654_intra, method = "kmeans", distance = "euclidean", type = "variables", num.clusters = 4, clustMethod = "ward")
clust_man_km_var_ward=clustering(MTBLS654_intra, method = "kmeans", distance = "manhattan", type = "variables", num.clusters = 4, clustMethod = "ward")
clust_pear_km_var_ward=clustering(MTBLS654_intra, method = "kmeans", distance = "pearson", type = "variables", num.clusters = 4, clustMethod = "ward")
clust_spe_km_var_ward=clustering(MTBLS654_intra, method = "kmeans", distance = "spearman", type = "variables", num.clusters = 4, clustMethod = "ward")

#todos os clusters Kmeans, para as samples, com metodo de aglomeração single
clust_euc_km_samples_sin=clustering(MTBLS654_intra, method = "kmeans", distance = "euclidean", type = "samples", num.clusters = 4, clustMethod = "single")
clust_man_km_samples_sin=clustering(MTBLS654_intra, method = "kmeans", distance = "manhattan", type = "samples", num.clusters = 4, clustMethod = "single")
clust_pear_km_samples_sin=clustering(MTBLS654_intra, method = "kmeans", distance = "pearson", type = "samples", num.clusters = 4, clustMethod = "single")
clust_spe_km_samples_sin=clustering(MTBLS654_intra, method = "kmeans", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "single")

#todos os clusters Kmeans, para as variáveis, com metodo de aglomeração single
clust_euc_km_var_sin=clustering(MTBLS654_intra, method = "kmeans", distance = "euclidean", type = "variables", num.clusters = 4, clustMethod = "single")
clust_man_km_var_sin=clustering(MTBLS654_intra, method = "kmeans", distance = "manhattan", type = "variables", num.clusters = 4, clustMethod = "single")
clust_pear_km_var_sin=clustering(MTBLS654_intra, method = "kmeans", distance = "pearson", type = "variables", num.clusters = 4, clustMethod = "single")
clust_spe_km_var_sin=clustering(MTBLS654_intra, method = "kmeans", distance = "spearman", type = "variables", num.clusters = 4, clustMethod = "single")

#todos os clusters Kmeans, para as samples, com metodo de aglomeração average
clust_euc_km_samples_avg=clustering(MTBLS654_intra, method = "kmeans", distance = "euclidean", type = "samples", num.clusters = 4, clustMethod = "average")
clust_man_km_samples_avg=clustering(MTBLS654_intra, method = "kmeans", distance = "manhattan", type = "samples", num.clusters = 4, clustMethod = "average")
clust_pear_km_samples_avg=clustering(MTBLS654_intra, method = "kmeans", distance = "pearson", type = "samples", num.clusters = 4, clustMethod = "average")
clust_spe_km_samples_avg=clustering(MTBLS654_intra, method = "kmeans", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "average")

#todos os clusters Kmeans, para as variáveis, com metodo de aglomeração average
clust_euc_km_var_avg=clustering(MTBLS654_intra, method = "kmeans", distance = "euclidean", type = "variables", num.clusters = 4, clustMethod = "average")
clust_man_km_var_avg=clustering(MTBLS654_intra, method = "kmeans", distance = "manhattan", type = "variables", num.clusters = 4, clustMethod = "average")
clust_pear_km_var_avg=clustering(MTBLS654_intra, method = "kmeans", distance = "pearson", type = "variables", num.clusters = 4, clustMethod = "average")
clust_spe_km_var_avg=clustering(MTBLS654_intra, method = "kmeans", distance = "spearman", type = "variables", num.clusters = 4, clustMethod = "average")

#todos os clusters Kmeans, para as samples, com metodo de aglomeração mcquitty
clust_euc_km_samples_mcq=clustering(MTBLS654_intra, method = "kmeans", distance = "euclidean", type = "samples", num.clusters = 4, clustMethod = "mcquitty")
clust_man_km_samples_mcq=clustering(MTBLS654_intra, method = "kmeans", distance = "manhattan", type = "samples", num.clusters = 4, clustMethod = "mcquitty")
clust_pear_km_samples_mcq=clustering(MTBLS654_intra, method = "kmeans", distance = "pearson", type = "samples", num.clusters = 4, clustMethod = "mcquitty")
clust_spe_km_samples_mcq=clustering(MTBLS654_intra, method = "kmeans", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "mcquitty")

#todos os clusters Kmeans, para as variáveis, com metodo de aglomeração mcquitty
clust_euc_km_var_mcq=clustering(MTBLS654_intra, method = "kmeans", distance = "euclidean", type = "variables", num.clusters = 4, clustMethod = "mcquitty")
clust_man_km_var_mcq=clustering(MTBLS654_intra, method = "kmeans", distance = "manhattan", type = "variables", num.clusters = 4, clustMethod = "mcquitty")
clust_pear_km_var_mcq=clustering(MTBLS654_intra, method = "kmeans", distance = "pearson", type = "variables", num.clusters = 4, clustMethod = "mcquitty")
clust_spe_km_var_mcq=clustering(MTBLS654_intra, method = "kmeans", distance = "spearman", type = "variables", num.clusters = 4, clustMethod = "mcquitty")

#todos os clusters Kmeans, para as samples, com metodo de aglomeração median
clust_euc_km_samples_me=clustering(MTBLS654_intra, method = "kmeans", distance = "euclidean", type = "samples", num.clusters = 4, clustMethod = "median")
clust_man_km_samples_me=clustering(MTBLS654_intra, method = "kmeans", distance = "manhattan", type = "samples", num.clusters = 4, clustMethod = "median")
clust_pear_km_samples_me=clustering(MTBLS654_intra, method = "kmeans", distance = "pearson", type = "samples", num.clusters = 4, clustMethod = "median")
clust_spe_km_samples_me=clustering(MTBLS654_intra, method = "kmeans", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "median")

#todos os clusters Kmeans, para as variáveis, com metodo de aglomeração median
clust_euc_km_var_me=clustering(MTBLS654_intra, method = "kmeans", distance = "euclidean", type = "variables", num.clusters = 4, clustMethod = "median")
clust_man_km_var_me=clustering(MTBLS654_intra, method = "kmeans", distance = "manhattan", type = "variables", num.clusters = 4, clustMethod = "median")
clust_pear_km_var_me=clustering(MTBLS654_intra, method = "kmeans", distance = "pearson", type = "variables", num.clusters = 4, clustMethod = "median")
clust_spe_km_var_me=clustering(MTBLS654_intra, method = "kmeans", distance = "spearman", type = "variables", num.clusters = 4, clustMethod = "median")

#todos os clusters Kmeans, para as samples, com metodo de aglomeração centroid
clust_euc_km_samples_cen=clustering(MTBLS654_intra, method = "kmeans", distance = "euclidean", type = "samples", num.clusters = 4, clustMethod = "centroid")
clust_man_km_samples_cen=clustering(MTBLS654_intra, method = "kmeans", distance = "manhattan", type = "samples", num.clusters = 4, clustMethod = "centroid")
clust_pear_km_samples_cen=clustering(MTBLS654_intra, method = "kmeans", distance = "pearson", type = "samples", num.clusters = 4, clustMethod = "centroid")
clust_spe_km_samples_cen=clustering(MTBLS654_intra, method = "kmeans", distance = "spearman", type = "samples", num.clusters = 4, clustMethod = "centroid")

#todos os clusters Kmeans, para as variáveis, com metodo de aglomeração centroid
clust_euc_km_var_cen=clustering(MTBLS654_intra, method = "kmeans", distance = "euclidean", type = "variables", num.clusters = 4, clustMethod = "centroid")
clust_man_km_var_cen=clustering(MTBLS654_intra, method = "kmeans", distance = "manhattan", type = "variables", num.clusters = 4, clustMethod = "centroid")
clust_pear_km_var_cen=clustering(MTBLS654_intra, method = "kmeans", distance = "pearson", type = "variables", num.clusters = 4, clustMethod = "centroid")
clust_spe_km_var_cen=clustering(MTBLS654_intra, method = "kmeans", distance = "spearman", type = "variables", num.clusters = 4, clustMethod = "centroid")


#############################
#gráficos Hierarquical clustering
#em função de Factor.Value.Kimchi.
#sempre que e variável as legendas não funcionam, nao entendo porque, tentei várias coisas 

##
dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_samples_comp, "Factor.Value.Kimchi.", title = "Dist. Eucladiana, tipo: amostras, método de agregamento: complete")
dendrogram_plot_col(MTBLS654_intra, clust_man_hc_samples_comp, "Factor.Value.Kimchi.", title = "Dist. Mannathan, tipo: amostras, método de agregamento: complete")
dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_samples_comp, "Factor.Value.Kimchi.", title = "Dist. Pearson, tipo: amostras, método de agregamento: complete")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_comp, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: amostras, método de agregamento: complete")

## 
# dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_var_comp, "Factor.Value.Kimchi.", title = "Dist. Eucladiana, tipo: variáveis, método de agregamento: complete")
# dendrogram_plot_col(MTBLS654_intra, clust_man_hc_var_comp, "Factor.Value.Kimchi.", title = "Dist. Mannathan, tipo: variáveis, método de agregamento: complete")
# dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_var_comp, "Factor.Value.Kimchi.", title = "Dist. Pearson, tipo: variáveis, método de agregamento: complete")
# dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_var_comp, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: variáveis, método de agregamento: complete")


##
dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_samples_ward, "Factor.Value.Kimchi.", title = "Dist. Eucladiana, tipo: amostras, método de agregamento: ward")
dendrogram_plot_col(MTBLS654_intra, clust_man_hc_samples_ward, "Factor.Value.Kimchi.", title = "Dist. Mannathan, tipo: amostras, método de agregamento: ward")
dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_samples_ward, "Factor.Value.Kimchi.", title = "Dist. Pearson, tipo: amostras, método de agregamento: ward")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_ward, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: amostras, método de agregamento: ward")

## 
# dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_var_ward, "Factor.Value.Kimchi.", title = "Dist. Eucladiana, tipo: variáveis, método de agregamento: ward")
# dendrogram_plot_col(MTBLS654_intra, clust_man_hc_var_ward, "Factor.Value.Kimchi.", title = "Dist. Mannathan, tipo: variáveis, método de agregamento: ward")
# dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_var_ward, "Factor.Value.Kimchi.", title = "Dist. Pearson, tipo: variáveis, método de agregamento: ward")
# dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_var_ward, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: variáveis, método de agregamento: ward")



##
dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_samples_sin, "Factor.Value.Kimchi.", title = "Dist. Eucladiana, tipo: amostras, método de agregamento: single")
dendrogram_plot_col(MTBLS654_intra, clust_man_hc_samples_sin, "Factor.Value.Kimchi.", title = "Dist. Mannathan, tipo: amostras, método de agregamento: single")
dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_samples_sin, "Factor.Value.Kimchi.", title = "Dist. Pearson, tipo: amostras, método de agregamento: single")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_sin, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: amostras, método de agregamento: single")

## 
# dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_var_sin, "Factor.Value.Kimchi.", title = "Dist. Eucladiana, tipo: variáveis, método de agregamento: single")
# dendrogram_plot_col(MTBLS654_intra, clust_man_hc_var_sin, "Factor.Value.Kimchi.", title = "Dist. Mannathan, tipo: variáveis, método de agregamento: single")
# dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_var_sin, "Factor.Value.Kimchi.", title = "Dist. Pearson, tipo: variáveis, método de agregamento: single")
# dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_var_sin, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: variáveis, método de agregamento: single")



##
dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_samples_avg, "Factor.Value.Kimchi.", title = "Dist. Eucladiana, tipo: amostras, método de agregamento: average")
dendrogram_plot_col(MTBLS654_intra, clust_man_hc_samples_avg, "Factor.Value.Kimchi.", title = "Dist. Mannathan, tipo: amostras, método de agregamento: average")
dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_samples_avg, "Factor.Value.Kimchi.", title = "Dist. Pearson, tipo: amostras, método de agregamento: average")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_avg, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: amostras, método de agregamento: average")

## 
# dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_var_avg, "Factor.Value.Kimchi.", title = "Dist. Eucladiana, tipo: variáveis, método de agregamento: average")
# dendrogram_plot_col(MTBLS654_intra, clust_man_hc_var_avg, "Factor.Value.Kimchi.", title = "Dist. Mannathan, tipo: variáveis, método de agregamento: average")
# dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_var_avg, "Factor.Value.Kimchi.", title = "Dist. Pearson, tipo: variáveis, método de agregamento: average")
# dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_var_avg, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: variáveis, método de agregamento: average")



##
dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_samples_mcq, "Factor.Value.Kimchi.", title = "Dist. Eucladiana, tipo: amostras, método de agregamento: mcquitty")
dendrogram_plot_col(MTBLS654_intra, clust_man_hc_samples_mcq, "Factor.Value.Kimchi.", title = "Dist. Mannathan, tipo: amostras, método de agregamento: mcquitty")
dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_samples_mcq, "Factor.Value.Kimchi.", title = "Dist. Pearson, tipo: amostras, método de agregamento: mcquitty")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_mcq, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: amostras, método de agregamento: mcquitty")

## 
# dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_var_mcq, "Factor.Value.Kimchi.", title = "Dist. Eucladiana, tipo: variáveis, método de agregamento: mcquitty")
# dendrogram_plot_col(MTBLS654_intra, clust_man_hc_var_mcq, "Factor.Value.Kimchi.", title = "Dist. Mannathan, tipo: variáveis, método de agregamento: mcquitty")
# dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_var_mcq, "Factor.Value.Kimchi.", title = "Dist. Pearson, tipo: variáveis, método de agregamento: mcquitty")
# dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_var_mcq, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: variáveis, método de agregamento: mcquitty")


##
dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_samples_me, "Factor.Value.Kimchi.", title = "Dist. Eucladiana, tipo: amostras, método de agregamento: median")
dendrogram_plot_col(MTBLS654_intra, clust_man_hc_samples_me, "Factor.Value.Kimchi.", title = "Dist. Mannathan, tipo: amostras, método de agregamento: median")
dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_samples_me, "Factor.Value.Kimchi.", title = "Dist. Pearson, tipo: amostras, método de agregamento: median")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_me, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: amostras, método de agregamento: median")

## 
# dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_var_me, "Factor.Value.Kimchi.", title = "Dist. Eucladiana, tipo: variáveis, método de agregamento: median")
# dendrogram_plot_col(MTBLS654_intra, clust_man_hc_var_me, "Factor.Value.Kimchi.", title = "Dist. Mannathan, tipo: variáveis, método de agregamento: median")
# dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_var_me, "Factor.Value.Kimchi.", title = "Dist. Pearson, tipo: variáveis, método de agregamento: median")
# dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_var_me, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: variáveis, método de agregamento: median")



##
dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_samples_cen, "Factor.Value.Kimchi.", title = "Dist. Eucladiana, tipo: amostras, método de agregamento: centroid")
dendrogram_plot_col(MTBLS654_intra, clust_man_hc_samples_cen, "Factor.Value.Kimchi.", title = "Dist. Mannathan, tipo: amostras, método de agregamento: centroid")
dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_samples_cen, "Factor.Value.Kimchi.", title = "Dist. Pearson, tipo: amostras, método de agregamento: centroid")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_cen, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: amostras, método de agregamento: centroid")


## 
# dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_var_cen, "Factor.Value.Kimchi.", title = "Dist. Eucladiana, tipo: variáveis, método de agregamento: centroid")
# dendrogram_plot_col(MTBLS654_intra, clust_man_hc_var_cen, "Factor.Value.Kimchi.", title = "Dist. Mannathan, tipo: variáveis, método de agregamento: centroid")
# dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_var_cen, "Factor.Value.Kimchi.", title = "Dist. Pearson, tipo: variáveis, método de agregamento: centroid")
# dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_var_cen, "Factor.Value.Kimchi.", title = "Dist. Spearman, tipo: variáveis, método de agregamento: centroid")



###############
# em função de Fermentation_days

##
dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_samples_comp, "Fermentation_days", title = "Dist. Eucladiana, tipo: amostras, método de agregamento: complete")
dendrogram_plot_col(MTBLS654_intra, clust_man_hc_samples_comp, "Fermentation_days", title = "Dist. Mannathan, tipo: amostras, método de agregamento: complete")
dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_samples_comp, "Fermentation_days", title = "Dist. Pearson, tipo: amostras, método de agregamento: complete")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_comp, "Fermentation_days", title = "Dist. Spearman, tipo: amostras, método de agregamento: complete")

## 
# dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_var_comp, "Fermentation_days", title = "Dist. Eucladiana, tipo: variáveis, método de agregamento: complete")
# dendrogram_plot_col(MTBLS654_intra, clust_man_hc_var_comp, "Fermentation_days", title = "Dist. Mannathan, tipo: variáveis, método de agregamento: complete")
# dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_var_comp, "Fermentation_days", title = "Dist. Pearson, tipo: variáveis, método de agregamento: complete")
# dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_var_comp, "Fermentation_days", title = "Dist. Spearman, tipo: variáveis, método de agregamento: complete")


##
dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_samples_ward, "Fermentation_days", title = "Dist. Eucladiana, tipo: amostras, método de agregamento: ward")
dendrogram_plot_col(MTBLS654_intra, clust_man_hc_samples_ward, "Fermentation_days", title = "Dist. Mannathan, tipo: amostras, método de agregamento: ward")
dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_samples_ward, "Fermentation_days", title = "Dist. Pearson, tipo: amostras, método de agregamento: ward")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_ward, "Fermentation_days", title = "Dist. Spearman, tipo: amostras, método de agregamento: ward")

## 
# dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_var_ward, "Fermentation_days", title = "Dist. Eucladiana, tipo: variáveis, método de agregamento: ward")
# dendrogram_plot_col(MTBLS654_intra, clust_man_hc_var_ward, "Fermentation_days", title = "Dist. Mannathan, tipo: variáveis, método de agregamento: ward")
# dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_var_ward, "Fermentation_days", title = "Dist. Pearson, tipo: variáveis, método de agregamento: ward")
# dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_var_ward, "Fermentation_days", title = "Dist. Spearman, tipo: variáveis, método de agregamento: ward")



##
dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_samples_sin, "Fermentation_days", title = "Dist. Eucladiana, tipo: amostras, método de agregamento: single")
dendrogram_plot_col(MTBLS654_intra, clust_man_hc_samples_sin, "Fermentation_days", title = "Dist. Mannathan, tipo: amostras, método de agregamento: single")
dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_samples_sin, "Fermentation_days", title = "Dist. Pearson, tipo: amostras, método de agregamento: single")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_sin, "Fermentation_days", title = "Dist. Spearman, tipo: amostras, método de agregamento: single")

## 
# dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_var_sin, "Fermentation_days", title = "Dist. Eucladiana, tipo: variáveis, método de agregamento: single")
# dendrogram_plot_col(MTBLS654_intra, clust_man_hc_var_sin, "Fermentation_days", title = "Dist. Mannathan, tipo: variáveis, método de agregamento: single")
# dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_var_sin, "Fermentation_days", title = "Dist. Pearson, tipo: variáveis, método de agregamento: single")
# dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_var_sin, "Fermentation_days", title = "Dist. Spearman, tipo: variáveis, método de agregamento: single")



##
dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_samples_avg, "Fermentation_days", title = "Dist. Eucladiana, tipo: amostras, método de agregamento: average")
dendrogram_plot_col(MTBLS654_intra, clust_man_hc_samples_avg, "Fermentation_days", title = "Dist. Mannathan, tipo: amostras, método de agregamento: average")
dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_samples_avg, "Fermentation_days", title = "Dist. Pearson, tipo: amostras, método de agregamento: average")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_avg, "Fermentation_days", title = "Dist. Spearman, tipo: amostras, método de agregamento: average")

## 
# dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_var_avg, "Fermentation_days", title = "Dist. Eucladiana, tipo: variáveis, método de agregamento: average")
# dendrogram_plot_col(MTBLS654_intra, clust_man_hc_var_avg, "Fermentation_days", title = "Dist. Mannathan, tipo: variáveis, método de agregamento: average")
# dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_var_avg, "Fermentation_days", title = "Dist. Pearson, tipo: variáveis, método de agregamento: average")
# dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_var_avg, "Fermentation_days", title = "Dist. Spearman, tipo: variáveis, método de agregamento: average")



##
dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_samples_mcq, "Fermentation_days", title = "Dist. Eucladiana, tipo: amostras, método de agregamento: mcquitty")
dendrogram_plot_col(MTBLS654_intra, clust_man_hc_samples_mcq, "Fermentation_days", title = "Dist. Mannathan, tipo: amostras, método de agregamento: mcquitty")
dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_samples_mcq, "Fermentation_days", title = "Dist. Pearson, tipo: amostras, método de agregamento: mcquitty")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_mcq, "Fermentation_days", title = "Dist. Spearman, tipo: amostras, método de agregamento: mcquitty")

## 
# dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_var_mcq, "Fermentation_days", title = "Dist. Eucladiana, tipo: variáveis, método de agregamento: mcquitty")
# dendrogram_plot_col(MTBLS654_intra, clust_man_hc_var_mcq, "Fermentation_days", title = "Dist. Mannathan, tipo: variáveis, método de agregamento: mcquitty")
# dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_var_mcq, "Fermentation_days", title = "Dist. Pearson, tipo: variáveis, método de agregamento: mcquitty")
# dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_var_mcq, "Fermentation_days", title = "Dist. Spearman, tipo: variáveis, método de agregamento: mcquitty")


##
dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_samples_me, "Fermentation_days", title = "Dist. Eucladiana, tipo: amostras, método de agregamento: median")
dendrogram_plot_col(MTBLS654_intra, clust_man_hc_samples_me, "Fermentation_days", title = "Dist. Mannathan, tipo: amostras, método de agregamento: median")
dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_samples_me, "Fermentation_days", title = "Dist. Pearson, tipo: amostras, método de agregamento: median")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_me, "Fermentation_days", title = "Dist. Spearman, tipo: amostras, método de agregamento: median")

## 
# dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_var_me, "Fermentation_days", title = "Dist. Eucladiana, tipo: variáveis, método de agregamento: median")
# dendrogram_plot_col(MTBLS654_intra, clust_man_hc_var_me, "Fermentation_days", title = "Dist. Mannathan, tipo: variáveis, método de agregamento: median")
# dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_var_me, "Fermentation_days", title = "Dist. Pearson, tipo: variáveis, método de agregamento: median")
# dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_var_me, "Fermentation_days", title = "Dist. Spearman, tipo: variáveis, método de agregamento: median")



##
dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_samples_cen, "Fermentation_days", title = "Dist. Eucladiana, tipo: amostras, método de agregamento: centroid")
dendrogram_plot_col(MTBLS654_intra, clust_man_hc_samples_cen, "Fermentation_days", title = "Dist. Mannathan, tipo: amostras, método de agregamento: centroid")
dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_samples_cen, "Fermentation_days", title = "Dist. Pearson, tipo: amostras, método de agregamento: centroid")
dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_samples_cen, "Fermentation_days", title = "Dist. Spearman, tipo: amostras, método de agregamento: centroid")


## 
# dendrogram_plot_col(MTBLS654_intra, clust_euc_hc_var_cen, "Fermentation_days", title = "Dist. Eucladiana, tipo: variáveis, método de agregamento: centroid")
# dendrogram_plot_col(MTBLS654_intra, clust_man_hc_var_cen, "Fermentation_days", title = "Dist. Mannathan, tipo: variáveis, método de agregamento: centroid")
# dendrogram_plot_col(MTBLS654_intra, clust_pear_hc_var_cen, "Fermentation_days", title = "Dist. Pearson, tipo: variáveis, método de agregamento: centroid")
# dendrogram_plot_col(MTBLS654_intra, clust_spe_hc_var_cen, "Fermentation_days", title = "Dist. Spearman, tipo: variáveis, método de agregamento: centroid")



###########################
#clust_euc_km_samples_comp
kmeans_plot(MTBLS654_intra, clust_euc_km_samples_comp)
kmeans_result_df(clust_euc_km_samples_comp)

#clust_man_km_samples_comp
kmeans_plot(MTBLS654_intra, clust_man_km_samples_comp)
kmeans_result_df(clust_man_km_samples_comp)

#clust_pear_km_samples_comp
kmeans_plot(MTBLS654_intra, clust_pear_km_samples_comp)
kmeans_result_df(clust_pear_km_samples_comp)

#clust_spe_km_samples_comp
kmeans_plot(MTBLS654_intra, clust_spe_km_samples_comp)
kmeans_result_df(clust_spe_km_samples_comp)



#clust_euc_km_var_comp
kmeans_plot(MTBLS654_intra, clust_euc_km_var_comp)
kmeans_result_df(clust_euc_km_var_comp)

#clust_man_km_var_comp
kmeans_plot(MTBLS654_intra, clust_man_km_var_comp)
kmeans_result_df(clust_man_km_var_comp)

#clust_pear_km_var_comp
kmeans_plot(MTBLS654_intra, clust_pear_km_var_comp)
kmeans_result_df(clust_pear_km_var_comp)

#clust_spe_km_var_comp
kmeans_plot(MTBLS654_intra, clust_spe_km_var_comp)
kmeans_result_df(clust_spe_km_var_comp)




#clust_euc_km_samples_ward
kmeans_plot(MTBLS654_intra, clust_euc_km_samples_ward)
kmeans_result_df(clust_euc_km_samples_ward)

#clust_man_km_samples_ward
kmeans_plot(MTBLS654_intra, clust_man_km_samples_ward)
kmeans_result_df(clust_man_km_samples_ward)

#clust_pear_km_samples_ward
kmeans_plot(MTBLS654_intra, clust_pear_km_samples_ward)
kmeans_result_df(clust_pear_km_samples_ward)

#clust_spe_km_samples_ward
kmeans_plot(MTBLS654_intra, clust_spe_km_samples_ward)
kmeans_result_df(clust_spe_km_samples_ward)



#clust_euc_km_var_ward
kmeans_plot(MTBLS654_intra, clust_euc_km_var_ward)
kmeans_result_df(clust_euc_km_var_ward)

#clust_man_km_var_ward
kmeans_plot(MTBLS654_intra, clust_man_km_var_ward)
kmeans_result_df(clust_man_km_var_ward)

#clust_pear_km_var_ward
kmeans_plot(MTBLS654_intra, clust_pear_km_var_ward)
kmeans_result_df(clust_pear_km_var_ward)

#clust_spe_km_var_ward
kmeans_plot(MTBLS654_intra, clust_spe_km_var_ward)
kmeans_result_df(clust_spe_km_var_ward)


#clust_euc_km_samples_sin
kmeans_plot(MTBLS654_intra, clust_euc_km_samples_sin)
kmeans_result_df(clust_euc_km_samples_sin)

#clust_man_km_samples_sin
kmeans_plot(MTBLS654_intra, clust_man_km_samples_sin)
kmeans_result_df(clust_man_km_samples_sin)

#clust_pear_km_samples_sin
kmeans_plot(MTBLS654_intra, clust_pear_km_samples_sin)
kmeans_result_df(clust_pear_km_samples_sin)

#clust_spe_km_samples_sin
kmeans_plot(MTBLS654_intra, clust_spe_km_samples_sin)
kmeans_result_df(clust_spe_km_samples_sin)



#clust_euc_km_var_sin
kmeans_plot(MTBLS654_intra, clust_euc_km_var_sin)
kmeans_result_df(clust_euc_km_var_sin)

#clust_man_km_var_sin
kmeans_plot(MTBLS654_intra, clust_man_km_var_sin)
kmeans_result_df(clust_man_km_var_sin)

#clust_pear_km_var_sin
kmeans_plot(MTBLS654_intra, clust_pear_km_var_sin)
kmeans_result_df(clust_pear_km_var_sin)

#clust_spe_km_var_sin
kmeans_plot(MTBLS654_intra, clust_spe_km_var_sin)
kmeans_result_df(clust_spe_km_var_sin)


#clust_euc_km_samples_avg
kmeans_plot(MTBLS654_intra, clust_euc_km_samples_avg)
kmeans_result_df(clust_euc_km_samples_avg)

#clust_man_km_samples_avg
kmeans_plot(MTBLS654_intra, clust_man_km_samples_avg)
kmeans_result_df(clust_man_km_samples_avg)

#clust_pear_km_samples_avg
kmeans_plot(MTBLS654_intra, clust_pear_km_samples_avg)
kmeans_result_df(clust_pear_km_samples_avg)

#clust_spe_km_samples_avg
kmeans_plot(MTBLS654_intra, clust_spe_km_samples_avg)
kmeans_result_df(clust_spe_km_samples_avg)



#clust_euc_km_var_avg
kmeans_plot(MTBLS654_intra, clust_euc_km_var_avg)
kmeans_result_df(clust_euc_km_var_avg)

#clust_man_km_var_avg
kmeans_plot(MTBLS654_intra, clust_man_km_var_avg)
kmeans_result_df(clust_man_km_var_avg)

#clust_pear_km_var_avg
kmeans_plot(MTBLS654_intra, clust_pear_km_var_avg)
kmeans_result_df(clust_pear_km_var_avg)

#clust_spe_km_var_avg
kmeans_plot(MTBLS654_intra, clust_spe_km_var_avg)
kmeans_result_df(clust_spe_km_var_avg)


#clust_euc_km_samples_mcq
kmeans_plot(MTBLS654_intra, clust_euc_km_samples_mcq)
kmeans_result_df(clust_euc_km_samples_mcq)

#clust_man_km_samples_mcq
kmeans_plot(MTBLS654_intra, clust_man_km_samples_mcq)
kmeans_result_df(clust_man_km_samples_mcq)

#clust_pear_km_samples_mcq
kmeans_plot(MTBLS654_intra, clust_pear_km_samples_mcq)
kmeans_result_df(clust_pear_km_samples_mcq)

#clust_spe_km_samples_mcq
kmeans_plot(MTBLS654_intra, clust_spe_km_samples_mcq)
kmeans_result_df(clust_spe_km_samples_mcq)



#clust_euc_km_var_mcq
kmeans_plot(MTBLS654_intra, clust_euc_km_var_mcq)
kmeans_result_df(clust_euc_km_var_mcq)

#clust_man_km_var_mcq
kmeans_plot(MTBLS654_intra, clust_man_km_var_mcq)
kmeans_result_df(clust_man_km_var_mcq)

#clust_pear_km_var_mcq
kmeans_plot(MTBLS654_intra, clust_pear_km_var_mcq)
kmeans_result_df(clust_pear_km_var_mcq)

#clust_spe_km_var_mcq
kmeans_plot(MTBLS654_intra, clust_spe_km_var_mcq)
kmeans_result_df(clust_spe_km_var_mcq)


#clust_euc_km_samples_me
kmeans_plot(MTBLS654_intra, clust_euc_km_samples_me)
kmeans_result_df(clust_euc_km_samples_me)

#clust_man_km_samples_me
kmeans_plot(MTBLS654_intra, clust_man_km_samples_me)
kmeans_result_df(clust_man_km_samples_me)

#clust_pear_km_samples_me
kmeans_plot(MTBLS654_intra, clust_pear_km_samples_me)
kmeans_result_df(clust_pear_km_samples_me)

#clust_spe_km_samples_me
kmeans_plot(MTBLS654_intra, clust_spe_km_samples_me)
kmeans_result_df(clust_spe_km_samples_me)



#clust_euc_km_var_me
kmeans_plot(MTBLS654_intra, clust_euc_km_var_me)
kmeans_result_df(clust_euc_km_var_me)

#clust_man_km_var_me
kmeans_plot(MTBLS654_intra, clust_man_km_var_me)
kmeans_result_df(clust_man_km_var_me)

#clust_pear_km_var_me
kmeans_plot(MTBLS654_intra, clust_pear_km_var_me)
kmeans_result_df(clust_pear_km_var_me)

#clust_spe_km_var_me
kmeans_plot(MTBLS654_intra, clust_spe_km_var_me)
kmeans_result_df(clust_spe_km_var_me)


#clust_euc_km_samples_cen
kmeans_plot(MTBLS654_intra, clust_euc_km_samples_cen)
kmeans_result_df(clust_euc_km_samples_cen)

#clust_man_km_samples_cen
kmeans_plot(MTBLS654_intra, clust_man_km_samples_cen)
kmeans_result_df(clust_man_km_samples_cen)

#clust_pear_km_samples_cen
kmeans_plot(MTBLS654_intra, clust_pear_km_samples_cen)
kmeans_result_df(clust_pear_km_samples_cen)

#clust_spe_km_samples_cen
kmeans_plot(MTBLS654_intra, clust_spe_km_samples_cen)
kmeans_result_df(clust_spe_km_samples_cen)



#clust_euc_km_var_cen
kmeans_plot(MTBLS654_intra, clust_euc_km_var_cen)
kmeans_result_df(clust_euc_km_var_cen)

#clust_man_km_var_cen
kmeans_plot(MTBLS654_intra, clust_man_km_var_cen)
kmeans_result_df(clust_man_km_var_cen)

#clust_pear_km_var_cen
kmeans_plot(MTBLS654_intra, clust_pear_km_var_cen)
kmeans_result_df(clust_pear_km_var_cen)

#clust_spe_km_var_cen
kmeans_plot(MTBLS654_intra, clust_spe_km_var_cen)
kmeans_result_df(clust_spe_km_var_cen)
