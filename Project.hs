import System.Random
import System.IO.Unsafe
randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))

users = ["user1","user2","user3","user4"]
items = ["item1","item2","item3","item4","item5","item6"]
purchasesHistory = [("user1",[["item1","item2","item3"],["item1" ,"item2","item4"]]),("user2",[["item2","item5"],["item4","item5"]]),("user3",[["item3","item2"]]),("user4",[])]

-------------------- generates a tuple that has two elements----------------------------

createTuple x y = (x,y)

---------------------returns lists of all purchasesHistory items by user---------------------

getpurchasesHistory :: String ->[(String,[[String]])] ->[[String]]
getpurchasesHistory user (x:xs) | (fst x) == user = (snd x)
								| otherwise = getpurchasesHistory user xs 

------------------converts a list from a 2-D list to a 1-D list------------------------------

flatting :: [[a]] -> [a]
flatting [] = []
flatting (x:xs) = x ++ flatting(xs)

------------------returns a list of all purchasesHistory items bought by all users----------------------------

all_purchases :: [(a,[[a]])] -> [a]
all_purchases list = flatting(all_purchases_Helper list)

all_purchases_Helper :: [(a,[[a]])] -> [[a]]
all_purchases_Helper [] = []
all_purchases_Helper (x:xs) = snd(x) ++ all_purchases_Helper(xs)

------------------count the number occurance of an item in a list---------------------------

countOccur :: String -> [String] -> Int	
countOccur _ [] = 0
countOccur item (x:xs) | item==x = 1 + countOccur item xs
					   | otherwise = (countOccur item xs) 

---------------------takes a user and returns the frequency list of previously purchased items------------------- 

freqListItems:: String -> [(String, Int)]
freqListItems user = freqListItems_Helper (flatting list) list [] where list = getpurchasesHistory user purchasesHistory

freqListItems_Helper ::[String] -> [[String]] -> [String] -> [(String,Int)] 
freqListItems_Helper [] _ _ = []
freqListItems_Helper (x:xs) carts checklist = 
						if(elem x checklist == False)
						then
						(createTuple x (count_item_freqListItems carts x):freqListItems_Helper xs carts (x:checklist))
						else
						freqListItems_Helper xs carts checklist
 
------------------takes a list of carts and an item,and counts number of occurances of items with the given item---------- 

count_item_freqListItems :: [[String]] -> String ->  Int 												
count_item_freqListItems [] _ = 0
count_item_freqListItems (x:xs) item = if elem item x == True
										then
										(length(x) - countOccur item x) + count_item_freqListItems xs item			
										else
										count_item_freqListItems xs item

--------------------------------- counts number of common items between two lists -----------------------

count_Intersections  :: [String] -> [String] -> Int 
count_Intersections  [] _ = 0
count_Intersections  (x:xs) oldcart = if (elem x oldcart)
									then 1 + count_Intersections  xs oldcart
									else count_Intersections  xs oldcart

-----------------------------------------

count_item_freqListCart :: String -> [[String]] -> [String] -> Int
count_item_freqListCart _ [] _ = 0									
count_item_freqListCart item (x:xs) newcart= 
				if (elem item x)
				then 
					if (elem item newcart) 
					then 
						(count_Intersections  newcart x) - (countOccur item x) + (count_item_freqListCart item xs newcart)
					else 
						(count_Intersections  newcart x) + (count_item_freqListCart item xs newcart)
				else 
					count_item_freqListCart item xs newcart	
					
------------------------------------------------------------------

freqListCart :: String ->[String] -> [(String, Int)] 												
freqListCart user newcart = freqListCart_Helper	(flatting list) list newcart [] 
										where list = getpurchasesHistory user purchasesHistory

freqListCart_Helper :: [String] -> [[String]] -> [String] -> [String] -> [(String, Int)]
freqListCart_Helper [] _ _ _ = []
freqListCart_Helper (x:xs) oldcart newcart checklist = 
		if (elem x checklist == False && value>0)
		then 
			(createTuple x value : freqListCart_Helper xs oldcart newcart (x:checklist))
		else 
			freqListCart_Helper xs oldcart newcart checklist
		where value = count_item_freqListCart x oldcart newcart	

--------------------------------------------------------------------

freqListCartAndItems:: String -> [String] -> [(String, Int)]
freqListCartAndItems user newcart = freqListCartAndItems_Helper (flatting list) list newcart [] 
										where list = getpurchasesHistory user purchasesHistory
freqListCartAndItems_Helper :: [String] -> [[String]] -> [String] -> [String] -> [(String, Int)]  										
freqListCartAndItems_Helper [] _ _ _ = []										
freqListCartAndItems_Helper (x:xs) oldcart newcart checklist = 
		if(elem x checklist == False)
		then
			((createTuple x ((count_item_freqListCart x oldcart newcart)+count_item_freqListItems oldcart x)):freqListCartAndItems_Helper xs oldcart newcart (x:checklist))
		else
			freqListCartAndItems_Helper xs oldcart newcart checklist

---------------------------------------------------------------

tuplesToList :: [(String,Int)] -> [[String]]
tuplesToList [] = []
tuplesToList (x:xs) = tupleToList_Helper x : tuplesToList xs

tupleToList_Helper :: (String,Int) -> [String]
tupleToList_Helper (x,y) = if (y>0)
					then
						x:tupleToList_Helper(x,y-1)
					else
						[]

---------------- recommends an item to the user based on the previously purchased items only ---------------

recommendEmptyCart :: String -> String
recommendEmptyCart user = if(length list>0)
							then	
							list !! (randomZeroToX ((length list)-1))											
							else
							[]
							where list =  flatting(tuplesToList(freqListItems user))
							
--recommend an item to the user based on the items currently in the user’s cart and the previously purchased items-------

recommendBasedOnItemsInCart :: String -> [String] -> String
recommendBasedOnItemsInCart user newcart = if(length list>0)
							then	
							list !! (randomZeroToX ((length list)-1))											
							else
							[]
							where list =  flatting(tuplesToList(freqListCartAndItems user newcart))
			
--Initializes an empty frequency list that maps each item to a list that will contain all the items that were 
                         --bought with this item along with their frequencies-----------------------------------------------

createEmptyFreqList :: [a] -> [(a,[b])]
createEmptyFreqList [] = []
createEmptyFreqList (x:xs) = (x,[]):createEmptyFreqList xs

-----------------------------------------------------------------------------

countOccurTogether :: String -> String -> [[String]] -> Int
countOccurTogether _ _ [] = 0											
countOccurTogether item newitem (x:xs) = if (elem item x && elem newitem x)
									then countOccur newitem x + countOccurTogether item newitem xs
									else countOccurTogether item newitem xs

occurTogether :: String -> [String]-> [[String]] -> [(String,Int)]
occurTogether _ [] _ = []
occurTogether item (x:xs) purchased = 
								if(item /= x)
								then 
									if(numberofOccurs >0) 
									then 
										(x ,numberofOccurs):occurTogether item xs purchased
									else 
										occurTogether item xs purchased
								else 
									occurTogether item xs purchased
								where numberofOccurs = countOccurTogether item x purchased 	


getAllUsersStats :: [(String, [[String]])] -> [(String, [(String, [(String, Int)])])]
getAllUsersStats list = getAllUsersStats_Helper1 users purchasesHistory

getAllUsersStats_Helper1 :: [String] -> [(String, [[String]])] -> [(String, [(String, [(String, Int)])])]
getAllUsersStats_Helper1 [] _ = []
getAllUsersStats_Helper1 (x:xs) purchased = (x,getAllUsersStats_Helper2 items (getpurchasesHistory x purchased)) :getAllUsersStats_Helper1 xs purchased


getAllUsersStats_Helper2 :: [String]-> [[String]] -> [(String, [(String, Int)])]
getAllUsersStats_Helper2 [] _ = []								
getAllUsersStats_Helper2 (x:xs) purchased = (x,occurTogether x items purchased): getAllUsersStats_Helper2 xs purchased 								

-----------------------------------------------------------------------

concatUsers listU1 listU2 = concatUsers_Helper1 (concatUsers_Helper listU1 listU2)

concatUsers_Helper [] [] = []
concatUsers_Helper (x:xs) (y:ys) = if (length((snd x))>0 && length((snd y))>0)
							then 
								((fst(x),(((snd x) ++ (snd y)))) : concatUsers_Helper xs ys)
							else		
								concatUsers_Helper xs ys

concatUsers_Helper1 [] = []
concatUsers_Helper1 (x:xs) = ((fst(x),addTuples(snd(x))):concatUsers_Helper1 xs)

addTuples tuples = addTuplesHelper (flatting (tuplesToList tuples)) items
addTuplesHelper _ [] = []												
addTuplesHelper list (x:xs) = if(numberOfOccurence > 0)
								then (x,numberOfOccurence):addTuplesHelper list xs
								else addTuplesHelper list xs
							where  numberOfOccurence = countOccur x list 							

purchasesIntersection _ [] = []
purchasesIntersection user1 ((x,y):xs) = (concatUsers user1 y):purchasesIntersection user1 xs
						


getUserInfo user = getUserInfo_Helper user (getAllUsersStats purchasesHistory)
getUserInfo_Helper user (x:xs) = if(fst(x) == user)
								 then
									snd(x)
								 else
									getUserInfo_Helper user xs

getAllButUser user = getAllButUser_Helper user (getAllUsersStats purchasesHistory)
getAllButUser_Helper _ [] = []
getAllButUser_Helper user (x:xs) = if (fst(x)== user) 
								   then
										getAllButUser_Helper user xs
							       else
									    (x:(getAllButUser_Helper user xs))

getSecondOfAllTuples [] = []									
getSecondOfAllTuples (x:xs) = snd(x) ++	getSecondOfAllTuples (xs)								
freqListUsers user = addTuples(getSecondOfAllTuples(flatting(purchasesIntersection (getUserInfo user) (getAllButUser user))))									


recommendBasedOnUsers user = if(length list>0)
							then	
							list !! (randomZeroToX ((length list)-1))											
							else
							[]
							where list =  flatting(tuplesToList(freqListUsers user))

randomItem list = list !! (randomZeroToX ((length list)-1))

recommendForEmptyCarts user =if (recommendItem==[])
							 then
								randomItem items
						     else
								recommendItem
							where recommendItem = recommendEmptyCart user 

recommendForOnItemsInCart user cart =
							if(recommendItem==[])
							then	
								randomItem items
							else
								recommendItem								
							where recommendItem = recommendBasedOnItemsInCart user cart

recommendForAllUsers user = 
						if(recommendItem==[])
						then	
							randomItem items
						else
							recommendItem
						where recommendItem = recommendBasedOnUsers user
						
recommend user cart = if(length(cart)==0)
					  then		
						recommendForEmptyCarts user
					  else
						  if(randomZeroToX(1)==0)
						  then
							  recommendForAllUsers user
						  else
							  recommendForOnItemsInCart user cart