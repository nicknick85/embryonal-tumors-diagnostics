library(survival)
library(ggfortify)

png(file = "s2_time.png", width=10000, height=5000, res=1200)
autoplot(survfit(Surv(dur, status) ~ yn, data = cbind(data.frame(yn = ifelse(dat$s2,"Yes","No")),dat)[dat$s0 != 1,])) +
labs(x="Time", y="Percent of undiagnosed patients", color = "Nausea/Vomiting", fill = "Nausea/Vomiting")
dev.off()

png(file = "s15_time.png", width=10000, height=5000, res=1200)
autoplot(survfit(Surv(dur, status) ~ yn, data = cbind(data.frame(yn = ifelse(dat$s15,"Yes","No")),dat)[dat$s0 != 1,])) +
labs(x="Time", y="Percent of undiagnosed patients", color = "Paresis/Paralysis", fill = "Paresis/Paralysis")
dev.off()

####################################################################

diag.mplot <- function(tab, name)
{
	dimnames(tab) <- list(Diagnosis = c("MB", "ATRT", "ETMR", "NB", "Other", "PB"), Symptom = c("No", "Yes"));

	mosaicplot(tab, main = name, color = TRUE);
}

png(file = "s1.png", width=10000, height=5000, res=1200)
par(mar=c(5,4,4,2)-1)
diag.mplot(table(dat$diag, dat$s1), "Headache frequency")
par(mar=c(5,4,4,2)+0.1)
dev.off()

png(file = "s3.png", width=10000, height=5000, res=1200)
par(mar=c(5,4,4,2)-1)
diag.mplot(table(dat$diag, dat$s3), "Bulging fontanelle frequency")
par(mar=c(5,4,4,2)+0.1)
dev.off()

png(file = "s11.png", width=10000, height=5000, res=1200)
par(mar=c(5,4,4,2)-1)
diag.mplot(table(dat$diag, dat$s11), "Convulsions frequency")
par(mar=c(5,4,4,2)+0.1)
dev.off()

########################################################################################
png(file = "age_diag.png", width=10000, height=5000, res=1200)
ggboxplot(dat, y = "age", x = "diag", add = "jitter", xlab = "Diagnosis", ylab = "Age in months", fill = "light blue", legend = "none") + 
scale_x_discrete(labels = c("MB", "ATRT", "ETMR", "NB", "Other", "PB"))
dev.off()

png(file = "age_diag_merge.png", width=7000, height=5000, res=1200)
ggboxplot(dat, y = "age", x = "diag_merge", add = "jitter", xlab = "Diagnosis", ylab = "Age in months", fill = "light blue", legend = "none") + 
scale_x_discrete(labels = c("MB", "ATRT", "Other"))
dev.off()

########################################################################################
png(file = "age_s0.png", width=5000, height=5000, res=1200)
ggboxplot(dat[!is.na(dat$s0),], y = "age", x = "s0", add = "jitter", xlab = "Any symptoms", ylab = "Age in months", fill = "light blue", legend = "none") +
scale_x_discrete(labels = c("Yes", "No"))
dev.off()
