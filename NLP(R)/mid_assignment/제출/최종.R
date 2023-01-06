A <- scan(file='./Q1/Q1_01.txt', what='char', quote=NULL)
B <- scan(file='./Q1/Q1_02.txt', what='char', quote=NULL)
A <- gsub("^[[:punct:]]|[[:punct:]]$", "", tolower(A))
inter <- intersect(A, B)
Freq <- table(A[A %in% inter])
Freq.data <- data.frame(¾îÈÖ=names(Freq), ºóµµ=as.vector(Freq))
write.table(Freq.data, file='Q1_out.txt', quote=F, sep = '\t', col.names=NA)

dir.create("./Q2_out")
for (i in list.files(path='./Q2', pattern='[.]txt$'))
{TEXTS <- scan(file= paste0("./Q2/", i, sep=""), what = "char", quote=NULL)
a <- TEXTS[grep('</w>$|</c>$|^[*]">|BETZ["]', TEXTS)]
b <- gsub("^.+?>", "", a)
c <- gsub("[<].+$", "", b)
d <- paste0(toupper(substring(i, 1, 1)), substring(i, 2))
write(c, file=paste0("./Q2_out/", d), sep="\n")
}
