
rpartTree = function(Data, lossMatrix){
  #build tree
tree = rpart(preseizure ~., method="class", data=Data, parms = list(loss = lossMatrix))
printcp(tree)
plotcp(tree)
par(mfrow = c(1,2), xpd = NA)
plot(tree, uniform = T, compress = T, margin = 0.1, branch = 0.3)
text(tree,use.n = TRUE, cex= 0.7)

#prune tree
#tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
#prunedTree<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
#printcp(prunedTree)
#plot pruned Tree
#plot(prunedTree, uniform=TRUE,
#     main="Pruned Classification Tree")
#text(prunedTree, use.n=TRUE, all=TRUE, cex=0.7)

return(tree)
}