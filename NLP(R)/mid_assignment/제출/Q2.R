dir.create("./Q2_out")
for (i in list.files(path='./Q2', pattern='[.]txt$'))
{TEXTS <- scan(file= paste0("./Q2/", i, sep=""), what = "char", quote=NULL)
a <- TEXTS[grep('</w>$|</c>$|^[*]">|BETZ["]', TEXTS)]
b <- gsub("^.+?>", "", a)
c <- gsub("[<].+$", "", b)
d <- paste0(toupper(substring(i, 1, 1)), substring(i, 2))
write(c, file=paste0("./Q2_out/", d), sep="\n")
}
