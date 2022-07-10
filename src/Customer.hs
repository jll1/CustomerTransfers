{-# LANGUAGE DeriveGeneric #-}
-- |This module creates a datatype Customer and contains the functions that manupulate customer information, such as creating customers and making transfers between customers.

module Customer where

import Control.Concurrent
import Control.Parallel
import System.Random
import Text.Printf

-- |Type synonym for name used in datatype Customer.
type Name = [Char]
-- |Type synonym for account used in datatype Customer.
type Account = Integer
-- |Type synonym for balance used in datatype Customer.
type Balance = Float

-- |Datatype for Customer in record syntax.
data Customer = Customer
  { name :: Name
  , account :: Account
  , balance :: Balance } deriving (Eq,Ord,Show)
 
-- |This function takes a list of names as String, a list of integers from 1-10, an MVar, a Channel, and executes IO functionality (creating and printing customers,starting transfer process, managing customer threads) and returns nothing.
createCustomers :: [[Char]] -- ^ receives list of names
                -> [Integer] -- ^ receives list of integers
                -> MVar () -- ^ receives MVar
                -> Chan () -- ^ receives channel
                -> IO () -- ^ returns IO action
createCustomers (x:xs)(y:ys) m e = do 
 let c = Customer x y 1000.00 : customers xs ys
 return c
 takeMVar m
 printcust c 
 print "Customers created."
 putMVar m ()
 print "Final Balances after 100 random transfers(10-50):"
 counttransfers c 0
 writeChan e()

-- |This function takes a list of names as string, a list of integers and recursively returns a list of customers using the values received.
customers :: [[Char]] -- ^ receives list of names
          -> [Integer] -- ^ receives list of integers
          -> [Customer] -- ^ returns list of customers
customers [][]=[]
customers []_=[]
customers _[]=[]
customers (x:xs)(y:ys) = Customer x y 1000.00 : customers xs ys

-- |This function takes a list of Customers, executes IO functionality (printing each customer recursively) and returns nothing.
printcust :: [Customer] -- ^ receives list of customers
          -> IO() -- ^ returns IO action
printcust [] = return ()
printcust (x:xs) = do 
 print x
 printcust xs

-- |This function takes a list of Customers, executes random transfers between two random customers and returns a list of Customers in IO format.
transfer :: [Customer] -- ^ receives list of customers
         -> IO [Customer] -- ^ returns list of IO customer
transfer (x:xs) = do 
 let l = length (x:xs)
 rc1 <- randomRIO (0,l-1) :: IO Int
 rc2 <- randomRIO (0,l-1) :: IO Int
 ramt <- randomRIO (10, 50) :: IO Float
 let rcust1 = (x:xs) !! rc1
     rcust2 = (x:xs) !! rc2
     cust1newbal = balance rcust1 - ramt
     cust2newbal = balance rcust2 + ramt
     c1bal = read (printf "%.2f" (cust1newbal)) :: Float
     c2bal = read (printf "%.2f" (cust2newbal)) :: Float
     c1 = updatebal rcust1 c1bal
     c2 = updatebal rcust2 c2bal
     zs = removecust rcust1 (x:xs)
 let xs = removecust rcust2 zs 
     newlist = nub' (c1:c2:xs)
 if cust1newbal < 0 then return (x:xs) else return newlist 

-- |This function takes a single Customer and a Balance, updates the customer with the balance and returns the updated customer.
updatebal :: Customer -- ^ receives customer
          -> Balance  -- ^ receives balance
          -> Customer -- ^ returns customer
updatebal x y = x { balance = y }

-- |This function takes a customer and a list of customers, and removes the customer from the list, it then returns the list of customers without the customer.
removecust :: Customer  -- ^ receives customer
           -> [Customer] -- ^ receives list of customers
           -> [Customer] -- ^ returns list of customers
removecust _ [] = []
removecust a (x:xs) 
 | a == x = removecust a xs
 | otherwise = x : removecust a xs
  
-- |This function takes a list Customers, removes duplicates/ older versions of customers and returns the list of customers without any duplicates.                
nub' :: [Customer] -- ^ receives list of customers
     -> [Customer] -- ^ returns list of customers
nub' [] = []
nub' [x] = [x]
nub' (x:xs:xss) 
 | name x == name xs  = nub' (x:xss)
 | otherwise = x : nub' (xs:xss)

-- |This function is recusively called, incrementing a counter each time to make 100 transfers, using the transfer function to update the list of customers and lastly printing the final list of customers, returning nothing.
counttransfers :: [Customer] -- ^ receives list of customers
               -> Integer -- ^ receives integer of transfers made so far
               -> IO() -- ^ returns IO action
counttransfers [] _ = return ()
counttransfers x y = do  
 let v = (1+y)
 if v < 101 then 
  do c <- transfer x
     counttransfers c v
 else printcust x
