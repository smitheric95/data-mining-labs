library(lattice)

p_svmr <- predict(svmr, data_test)
c_svmr <- confusionMatrix(p_svmr, data_test$days_in_hospital, positive='1+ days')

svm_tab <- prop.table(c_svmr$table, 2)

myPanel <- function(x, y, z, ...) {
    panel.levelplot(x,y,z,...)
    panel.text(x, y, round(z,4))
}

png('./plots/svm-heat.png', width=3.75, height=3.25, units='in', res=1200, pointsize=4)
levelplot(svm_tab, panel=myPanel, at=(0:100/100),
          aspect='fill', col.regions=rev(gray(0:100/100)))
dev.off()
