mat = read.csv("http://north1ws.anu.edu.au/~kevin/results/BVZ0018-GC04L~fullres-orig_gaps.csv", header=T)
m = as.matrix(mat[,2:ncol(mat)])
hist(m)
rownames(m) = mat$X
image(t(m),col=c("#000000FF", rainbow(10)))
?image
?heat.colors
warnings()
text(mat$X)
