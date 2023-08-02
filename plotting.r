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