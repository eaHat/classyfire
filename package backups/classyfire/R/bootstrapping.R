# Private functions for bootstrapping


.bootIndx <- function(data, indices) 
{
  return(indices) 
} 


# .bootControl <- function(trainClass, indices)
# {
#   classLevels = levels(as.factor(as.matrix(trainClass)))
#   classLength = length(classLevels)
#   
#   bootTrainClass = factor(trainClass[indices], levels = classLevels)
#   btc = table(bootTrainClass)
#   
#   for (i in 1:classLength)
#   {
#     bootClass = as.numeric(btc[i])
#     
#     if (bootClass == 0)
#     {
#       indices = sample(length(bootTrainClass), replace=TRUE)  
#       .bootControl(trainClass = trainClass, indices = indices)
#     }
#   }
#   
#   return(indices)
# }