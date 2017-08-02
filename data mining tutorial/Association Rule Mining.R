load("~/Documents/Learning/R/data mining tutorial/titanic.raw.rdata")

head(titanic.raw)
attach(titanic.raw)
# install library "Matrix" & "arules"
## Tools -> install packages -> search "Matrix"/"arules"
library(arules)

# find association rules with default settings
rules <- apriori(titanic.raw)
inspect(rules)
## for any A -> B
## Support 支持度: P(A ∩ B)
## Confidence 置信度: P(B|A)
## Lift 提升度: Lift(A->B) = Confidence(A->B)/Support(B)
### 描述的是相对于不用规则，使用规则可以提高多少。有用的规则的提升度大于1

#We then set rhs=c("Survived=No", "Survived=Yes") in appearance to make sure that only "Survived=No" and "Survived=Yes" will appear in the rhs of rules.
# rules with rhs containing "Survived" only
rules <- apriori(titanic.raw, parameter = list(minlen=2, supp=0.005, conf=0.8), appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"), control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

#Pruning Redundant Rules
#In the above result, rule 2 provides no extra knowledge in addition to rule 1, since rules 1 tells us that all 2nd-class children survived. 
#Generally speaking, when a rule (such as rule 2) is a super rule of another rule (such as rule 1) and the former has the same or a lower lift, 
#the former rule (rule 2) is considered to be redundant. Below we prune redundant rules. 

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)


# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

detach(titanic.raw)