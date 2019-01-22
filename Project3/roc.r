library('pROC')

prob_knn <- predict(knn, data_test, type='prob')

r <- roc(data_test$days_in_hospital == '1+ days', prob_knn[,'1+ days'])
png('./plots/knn-roc.png', width=4, height=3.25, units='in', res=1200, pointsize=4)
r <- roc(data_test$days_in_hospital == '1+ days', prob_knn[,'1+ days'])
par(cex.lab=3, cex.axis=2, cex.main=1, cex.sub=1.5)
plot(r, mar=c(7,7,4,7)+0.1, mgp=c(5, 1, 0))
dev.off()