

#' Cards
#' 
#' The data used here refer to 16 cards (images) on which 30 children performed
#' a hierarchical sorting task.
#' 
#' 
#' @name cards
#' @docType data
#' @format A data frame with 16 rows (the number of cards) and 81 columns (the
#' total number of levels provided by all children). For each child, we have
#' several qualitative variables corresponding to nested partitions: a
#' partition corresponds to a level provided by the child. The columns are
#' grouped by child.
#' @source Applied mathematics department, AGROCAMPUS OUEST
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' data(cards)
#' ## Example of FAHST
#' group.cards<-c(2,3,3,2,2,4,2,3,2,1,3,2,3,3,3,2,3,3,2,3,3,3,3,3,3,3,3,3,3,3)
#' res.fahst<-fahst(cards,group=group.cards)
#' }
#' 
NULL





#' Chocolates data
#' 
#' The data used here refer to six varieties of chocolates sold in France.\cr
#' 
#' - For the sensory description: each chocolate was evaluated twice by 29
#' panelists according to 14 sensory descriptors;\cr - For the hedonic data:
#' each chocolate was evaluated on a structured scale from 0 to 10, by 222
#' consumers, according to their liking (0) or disliking (10);\cr - For the
#' sensory panels description: each chocolate was evaluated by 7 panels
#' according to 14 sensory descriptors.
#' 
#' 
#' @name chocolates
#' @docType data
#' @format
#' 
#' There are three data frames: - sensochoc: a data frame with 348 rows and 19
#' columns: 5 qualitative variables (Panelist, Session, Form, Rank, Product)
#' and 14 sensory descriptors;\cr - hedochoc: a data frame with 6 rows and 222
#' columns: each row corresponds to a chocolate and each column to the hedonic
#' scores given by one of the 222 consumers participating in the study;\cr -
#' sensopanels: a data frame with 6 rows and 98 columns: each row corresponds
#' to a chocolate and each column to the mean over the panelists of a given
#' panel according to a sensory descriptor.
#' @source Applied mathematics department, AGROCAMPUS OUEST
#' @keywords datasets
#' @examples
#' 
#' data(chocolates)
#' decat(sensochoc, formul = "~Product+Panelist", firstvar = 5, graph = FALSE)
#' 
NULL





#' Cocktail data
#' 
#' The data used here refer to 16 cocktails.
#' 
#' There are 3 files corresponding to the composition of the cocktails; the
#' sensory description of the cocktails; the hedonic scores.
#' 
#' - For the composition of the cocktails: The mango, banana, orange and lemon
#' concentration are known;\cr - For the sensory description: each cocktail was
#' evaluated by 12 panelists according to 13 sensory descriptors (only the
#' average of each cocktail are given). - For the hedonic data: each cocktail
#' was evaluated on a structured scale from 0 to 10, by 100 consumers,
#' according to their disliking (0) or liking (10).
#' 
#' 
#' @name cocktail
#' @docType data
#' @format There are three data frames: - compo.cocktail: a data frame with 16
#' rows and 4 columns: the composition of each cocktail is given for the 4
#' ingredients;\cr - senso.cocktail: a data frame with 16 rows and 13 columns:
#' each cocktail was evaluated by 12 panelists according to 13 sensory
#' descriptors;\cr hedo.cocktail: a data frame with 16 rows and 100 columns:
#' each cocktail was evaluated on a structured scale from 0 to 10, by 100
#' consumers, according to their disliking (0) or liking (10).
#' @source Applied Mathematics Department, Agrocampus Rennes
#' @keywords datasets
#' @examples
#' 
#' data(cocktail)
#' 
NULL





#' Composition of the cocktails data
#' 
#' The data used here refer to the composition of 16 cocktails, i.e. the mango,
#' banana, orange and lemon concentration.
#' 
#' 
#' @name compo.cocktail
#' @docType data
#' @format A data frame with 16 rows and 4 columns: the composition of each
#' cocktail is given for the 4 ingredients.
#' @source Applied mathematics department, AGROCAMPUS OUEST
#' @keywords datasets
#' @examples
#' 
#' data(cocktail)
#' 
NULL





#' Cream Ideal Data
#' 
#' The data used here refer to the sensory description of 9 dessert chocolate
#' creams.\cr
#' 
#' Each cream was evaluated once by 86 French consumers and described on 13
#' attributes according to the Ideal Profile Method.\cr Both perceived and
#' ideal intensities were asked. In addition, the overall liking is asked.\cr
#' 
#' 
#' @name cream_id
#' @docType data
#' @format
#' 
#' A data frame made of 86*9=774 rows and 2 qualitative variables (panelist and
#' product), 13*2 attributes (perceived and ideal intensities) and overall
#' liking.
#' @source Agrocampus Ouest, Melodie Sanchez, Sarah Sanchez
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' data(cream_id)
#' decat(cream_id, formul = "~product+user", firstvar = 3, graph = FALSE)
#' 
#' ###IdMapConsumer function
#' data(cream_signa)
#' res.idmap <- IdMapConsumer(craem_id, cream_signa, col.p=2, col.j=1, col.lik=29, 
#' num.col.var.signa=c(1:12), conf.level=0.90, id.recogn="id_", color = FALSE, simusigni = 500) 
#' 
#' }
#' 
NULL





#' Data description of the consumers who made the Ideal for the cream
#' 
#' The data used here refer to the sensory description of 9 dessert chocolate
#' creams.\cr
#' 
#' Each cream was evaluated once by 86 French consumers and described on 13
#' attributes according to the Ideal Profile Method.\cr Both perceived and
#' ideal intensities were asked. In addition, the overall liking is asked.\cr
#' 
#' 
#' @name cream_signa
#' @docType data
#' @format
#' 
#' A data frame made of 86*9=774 rows and 2 qualitative variables (panelist and
#' product), 13*2 attributes (perceived and ideal intensities) and overall
#' liking.
#' @source Agrocampus Ouest, Melodie Sanchez, Sarah Sanchez
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' data(cream_signa)
#' data(cream_signa)
#' res.idmap <- IdMapConsumer(cream_id, cream_signa, col.p=2, col.j=1, col.lik=29, 
#' num.col.var.signa=c(1:12), conf.level=0.90, id.recogn="id_", color = FALSE, simusigni = 500) 
#' 
#' }
#' 
NULL





#' Cocktails hedonic scores
#' 
#' The data used here refer to 16 cocktails. Each cocktail was evaluated on a
#' structured scale from 0 to 10, by 100 consumers, according to their liking
#' (0) or disliking (10).
#' 
#' 
#' @name hedo.cocktail
#' @docType data
#' @format A data frame with 16 rows and 100 columns: each row corresponds to a
#' cocktail and each column to the hedonic scores given by one of the 100
#' consumers participating in the study.
#' @source Applied mathematics department, AGROCAMPUS OUEST
#' @keywords datasets
#' @examples
#' 
#' data(cocktail)
#' 
NULL





#' Chocolates hedonic scores
#' 
#' The data used here refer to six varieties of chocolates sold in France. Each
#' chocolate was evaluated on a structured scale from 0 to 10, by 222
#' consumers, according to their liking (0) or disliking (10).
#' 
#' 
#' @name hedochoc
#' @docType data
#' @format A data frame with 6 rows and 222 columns: each row corresponds to a
#' chocolate and each column to the hedonic scores given by one of the 222
#' consumers participating in the study.
#' @source Agrocampus Rennes
#' @keywords datasets
#' @examples
#' 
#' data(chocolates)
#' 
NULL





#' An example of Napping data
#' 
#' The data used here refer to 10 different French wines evaluated by 11
#' panelists. They were asked to position the wines on a tableclothe of
#' dimension (60,40).
#' 
#' 
#' @name napping.don
#' @docType data
#' @format A data frame of dimension (10,22): each row represents a French
#' wine, each couple (Xi,Yi) represents the coordinates of the wines positioned
#' on a tableclothe for a given panelist.
#' @source Applied mathematics department, AGROCAMPUS OUEST
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' data(napping)
#' nappeplot(napping.don)
#' res <- pmfa(napping.don, napping.words)
#' res2 <- boot(napping.don,method="napping")
#' }
#' 
NULL





#' Napping data
#' 
#' The data used here refer to 10 different French wines evaluated by 11
#' panelists.\cr They were asked to position the wines on a tableclothe of
#' dimension (60,40). They were asked to describe each wine using their own
#' word list.
#' 
#' 
#' @name napping
#' @docType data
#' @format There are two data frames: - napping.don: A data frame of dimension
#' (10,22): each row represents a French wine, each couple (Xi,Yi) represents
#' the coordinates of the wines positioned on a tableclothe for a given
#' panelist;\cr - napping.words: A data frame of dimension (10,14): each row
#' represents a French wine, each column an attribute, each cell the number of
#' times a given attribute was quoted for a given wine.
#' @source Applied mathematics department, AGROCAMPUS OUEST
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' data(napping)
#' nappeplot(napping.don)
#' dev.new()
#' pmfa(napping.don, napping.words)
#' }
#' 
NULL





#' An example of "illustrative" variables to enhance results from Napping data
#' 
#' The data used here refer to 10 different French wines evaluated by 11
#' panelists. They were asked to describe each wine using their own word list.
#' 
#' 
#' @name napping.words
#' @docType data
#' @format A data frame of dimension (10,14): each row represents a French
#' wine, each column an attribute, each cell the number of times a given
#' attribute was quoted for a given wine
#' @source Applied mathematics department, AGROCAMPUS OUEST
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' data(napping)
#' nappeplot(napping.don)
#' dev.new()
#' pmfa(napping.don, napping.words)
#' }
#' 
NULL





#' Perfume data obtained by free choice profiling
#' 
#' The data used here refer to 12 luxury perfumes described by 6 experts.
#' 
#' 
#' @name perfume_fcp
#' @docType data
#' @format A data frame with 12 rows (the number of perfumes) and 47 columns.
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' data(perfume_fcp)
#' res <- fcp(perfume_fcp, group = c(12,7,7,7,6,8))
#' }
#' 
NULL





#' Perfume Ideal Data
#' 
#' The data used here refer to the sensory description of twelve perfumes (2
#' were duplicated).\cr
#' 
#' Each perfume was evaluated once by 103 Dutch consumers and described on 21
#' attributes according to the Ideal Profile Method.\cr Both perceived and
#' ideal intensities were asked. In addition, the overall liking is asked.\cr
#' 
#' 
#' @name perfume_ideal
#' @docType data
#' @format
#' 
#' A data frame made of 103*14=1442 rows and 2 qualitative variables (set and
#' product), 21*2 attributes (perceived and ideal intensities) and overall
#' liking.
#' @source OP&P Product Research, Utrecht, The Netherlands
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' data(perfume_ideal)
#' decat(perfume_ideal, formul = "~product+user", firstvar = 3, graph = FALSE)
#' }
#' 
NULL





#' Perfume
#' 
#' The data used here refer to 12 luxury perfumes categorized by 30 consumers.
#' 
#' 
#' @name perfume
#' @docType data
#' @format A data frame with 12 rows (the number of perfumes) and 30 columns
#' (the number of consumers): a cell corresponds either to the number of the
#' group to which the product belongs for the consumer, or, in the case of
#' "qualified" categorization, to the sequence of words associted with the
#' group to which the product belongs for the consumer.
#' @source Applied Mathematics Department, AGROCAMPUS OUEST Centre de Rennes
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' data(perfume)
#' ## Example of FAST
#' res.fast <- fast(perfume)
#' }
#' 
NULL





#' Sensory data for 16 cocktails
#' 
#' The data used here refer to the sensory description of 16 cocktails. Each
#' cocktail was evaluated by 12 panelists according to 13 sensory descriptors
#' (only the average of each cocktail are given).
#' 
#' 
#' @name senso.cocktail
#' @docType data
#' @format A data frame with 16 rows and 13 columns: each cocktail was
#' evaluated by 12 panelists according to 13 sensory descriptors.
#' @source Agrocampus Rennes
#' @keywords datasets
#' @examples
#' 
#' data(cocktail)
#' 
NULL





#' Sensory data for 6 chocolates
#' 
#' The data used here refer to the sensory description of six varieties of
#' chocolates sold in France: each chocolate was evaluated twice by 29
#' panelists according to 14 sensory descriptors.
#' 
#' 
#' @name sensochoc
#' @docType data
#' @format
#' 
#' A data frame with 348 rows and 19 columns: 5 qualitative variables
#' (Panelist, Session, Form, Rank, Product) and 14 sensory descriptors.
#' @source Applied mathematics department, AGROCAMPUS OUEST
#' @keywords datasets
#' @examples
#' 
#' data(chocolates)
#' decat(sensochoc, formul = "~Product+Panelist", firstvar = 5, graph = FALSE)
#' 
NULL





#' Sensory profiles given by 7 panels
#' 
#' The data used here refer to six varieties of chocolates sold in France. Each
#' chocolate was evaluated by 7 panels according to 14 sensory descriptors.
#' 
#' 
#' @name sensopanels
#' @docType data
#' @format A data frame with 6 rows and 98 columns: each row corresponds to a
#' chocolate and each column to the mean over the panelists of a given panel
#' according to a sensory descriptor.
#' @source Agrocampus Rennes
#' @keywords datasets
#' @examples
#' 
#' data(chocolates)
#' 
NULL





#' Smoothies
#' 
#' The data used here refer to 8 smoothies on which 24 consumers performed a
#' sorted napping task.
#' 
#' 
#' @name smoothies
#' @docType data
#' @format A data frame with 8 rows (the number of smoothies) and 72 columns
#' (the number of consumers * 3). For each consumer, we have the coordinates of
#' the products on the tablecloth associated with napping on the one hand and
#' the partitionning variable associated with categorization on the other hand.
#' The columns are grouped by consumer.
#' @source Applied mathematics department, AGROCAMPUS OUEST
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' data(smoothies)
#' ## Example of FASNT
#' res.fasnt<-fasnt(smoothies,first="nappe")
#' }
#' 
NULL





#' Videos data obtained with Holos
#' 
#' Example of Holos data, as obtained with the \code{\link{format_holos}}
#' function.
#' 
#' 
#' @name videos
#' @docType data
#' @format A list of 4 objects: \code{name.subjects}, a dataframe containing
#' the concordance between the names of the subjects as given in the Holos
#' experiment and their ID; \code{datadigit}, a list of S (S = number of
#' subjects) dataframes corresponding to the digit-tracking data;
#' \code{datafinal_coord}, a list of S (S = number of subjects) dataframes
#' corresponding to the final configurations data; and \code{datafinal_verb}, a
#' list of S (S = number of subjects) dataframes corresponding to the final
#' verbalization data.
#' @seealso \code{\link{format_holos} \link{analyse_holos}}
#' @examples
#' 
#' data(videos)
#' 
NULL



