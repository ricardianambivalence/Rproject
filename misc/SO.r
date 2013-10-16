require(gtools)
M1 <- list(A=1, B=2, C=3)
M2 <- list(A=4, B=5, C=6)

perms <- t(permutations(3, 2, 1:3))

comboList <- list()
for (i in 1:ncol(perms)) {
    nameString <- paste0(names(M2)[perms[1,i]], names(M1)[perms[2,i]])
    comboList[[nameString]] <- mapply("/", M2[[perms[,i][1]]], M1[[perms[,i][2]]])
}
