# MP

data_clean.ipynb

outage_summary.ipynb

pca_outages.ipynb outputs:
- outages_tract_loadings.csv: this shows what each PC represents (and what a high/low score for the PC means)
- outages_cluster_profiles.csv: this is the statistical outage summary for each cluster (ie max/min/median/etc values for outage metrics)
- outages_tracts_with_pca_and_clusters.geojson: this is the geojson data for a map that shows the PC scores and cluster for each tract
- five_maps_pcs_and_cluster.png: shows five maps together, with the PC score scale for each as well as what cluster each tract falls into
- outlier_tracts_static_map_polygons_only_outages_pca.png: highlights the top 50 outlier tracts
- cluster_outage_weather_summary.csv: for each cluster, the summary statistics for several socioeconomic and weather metrics
- tracts_with_clusters_outage_weather.csv: has the outage and weather metrics for every single tract, as well as its cluster and PC scores
- top50_standout_tracts.csv: top 50 outliers (that are currently mapped)
- cluster_weather_socioeconomics_heatmap.png: cluster-level z-scores (above/below average)

pca_socioeconomics.ipynb
- socioeconomics pca loadings.csv: this shows what each PC represents (and what a high/low score for the PC means)
- socioeconomics_cluster_profiles.csv: this is the statistical socioeconomics summary for each cluster (ie mean values for socioeconomic metrics, but NOTE these are z-scores, not the original values)
- socioeconomic_tracts_with_pca_and_clusters.geojson: this is the geojson data for a map that shows the PC scores and cluster for each tract
- socioeconomic_tracts_with_pca_and_clusters.geojson: this is the geojson data for a map that shows the PC scores and cluster for each tract
- maps_pcs_and_cluster.png: shows tract maps with the PC score scale for each as well as what cluster each tract falls into
- cluster_outage_weather_summary.csv: for each cluster, the summary statistics for several outage and weather metrics
- tracts_with_clusters_outage_weather.csv: has the outage and weather metrics for every single tract, as well as its cluster and PC scores
- top50_standout_tracts.csv: top 50 outliers
- cluster_weather_outage_heatmap.png: cluster-level z-scores (above/below average)



