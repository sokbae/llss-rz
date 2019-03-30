Replication codes for "Desperate times call for desperate measures: government spending multipliers in hard times" by Lee, Liao, Seo, and Shin (2019)



rz-thr-test.R: Using the sup-Wald test in Hansen (2000), it conducts inference for multiple regimes. Main outputs are
	(1) rz-thr-test.out
	(2) graph-thr-het-500.pdf
	(3) graph-thr-het-458.pdf

rz-threshold.R: Estimates the threshold regression model. Main outputs are
	(1) recession-dgp.pdf
	(2) recessions-unemp.pdf
	(3) obj-values.pdf
	(4) rz_dat_updated.csv

jordagk_llss_newsy.do: Stata do file. We modify the original code from Ramey and Zubairy (2018) to apply the estimated threshold point 11.97. The IV is military news. Main outputs are
	(1) jordagkirs_results-newsy.log
	(2) junkmultse-newsy.csv

jordagk_llss_bp.do: Stata do file. We modify the original code from Ramey and Zubairy (2018) to apply the estimated threshold point 11.97. The IV is BP. Main outputs are
	(1) jordagkirs_results-bp.log
	(2) junkmultse-bp.csv

jordagk_llss_twoinstruments.do: Stata do file. We modify the original code from Ramey and Zubairy (2018) to apply the estimated threshold point 11.97. The IVs are military news and BP. Main outputs are
	(1) jordagkirs_results-twoinstruments.log
	(2) junkmultse-twoinstruments.csv

Note that Table 1 was constructed using junkmultse-xxx.csv files.

figure-cum-mul.R: generates the cumulative multiplier graphs. One from the original results in Ramey and Zubairy (2018) and the other from the estimated threshold value at 11.97. The IV is military news. 


