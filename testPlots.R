ggbarTest(obj)
ggbarTest(obj, showText = TRUE)
ggbarTest(obj, showText = TRUE, ylims=c(80, 100))


ggplotTrend(obj)
ggplotTrend(obj, showText  = TRUE)
ggplotTrend(obj, showText  = TRUE, ylims=c(90, 100))

ggClassBar(obj)
ggClassBar(obj , showText = TRUE)




ggPermHist(permObj)
ggPermHist(permObj, density=TRUE)
ggPermHist(permObj, density=TRUE, percentiles = TRUE, mean = TRUE)
ggPermHist(permObj, density=TRUE, percentiles = TRUE, median = TRUE)



ggEnsHist(obj)
ggEnsHist(obj, density = TRUE)
ggEnsHist(obj, density = TRUE, percentiles=TRUE)
ggEnsHist(obj, density = TRUE, percentiles=TRUE, mean=TRUE)
ggEnsHist(obj, density = TRUE, percentiles=TRUE, median=TRUE)


fiveNumSummary(permObj)
fiveNumSummary(permObj)$median      
fiveNumSummary(permObj)$minimum
fiveNumSummary(permObj)$maximum
fiveNumSummary(permObj)$upperQ
fiveNumSummary(permObj)$lowerQ