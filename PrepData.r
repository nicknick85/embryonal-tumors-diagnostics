occ.n <- function(n, ss)
{
	s <- paste("([[:space:]]", n, "[[:space:]])|(^", n, "[[:space:]])|([[:space:]]", n, "$)|(^", n, "$)", sep = "")	
	res <- grepl(s, ss)
	res[is.na(ss)] <- NA

	as.numeric(res)
}

prep.ss <- function(ss, nn, name)
{
	res <- c()
	names <- c()	

	for (n in nn)
	{
		res <- cbind(res, occ.n(n, ss))
		names <- c(names, paste(name, n, sep = ""))
	}

	res <- as.data.frame(res)
	names(res) <- names

	res
}

dat$l6 <- as.numeric(dat$l6 | dat$l1 | dat$l2 | dat$l5 | dat$l13 | dat$l12)
dat$l3 <- as.numeric(dat$l3 | dat$l6 | dat$l1 | dat$l2 | dat$l5 | dat$l13 | dat$l12 | dat$l14 | dat$l10)
dat$l8 <- as.numeric(dat$l8 | dat$l10 | dat$l14)
dat$l2 <- as.numeric(dat$l2 | dat$l13 | dat$l12)
dat$l16 <- as.numeric(dat$l16 | dat$l18)
dat$l23 <- as.numeric(dat$l23 | dat$l18 | dat$l20)
dat$l15 <- as.numeric(dat$l15 | dat$l20)