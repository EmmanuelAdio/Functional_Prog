----------Emmanuel Adio's Functional Coursework---------

----Part 1----
{-

(a) List and explain (in your own words) 3 benefits that Functional Programming brings to
programmers;

Functional programming allows programmers to define functions which can be a very efficient way of programming, as it allows for modularity so the functions can be reused, and code 
can easily be read through for debugging. Functional programming also means that each defined function is isolated, and each function will give the same result every time and does not 
have any outside values affecting the results. Functional programming also is much more readable. One reason for this is that all the functions are separately defined, and values 
don't overlap one another, but also because there is no use of loops in functional programming (only recursions) which makes the code a lot easier to read/follow. and makes the 
code easier to compile, as it uses less memory.

(b)Explain in your own words what a (mathematical) function is and discuss to what extent
Haskell functions resemble mathematical functions;

A mathematical function is a function that takes a numerical value, or multiple numerical values, and can apply a mathematical operation to that value to return a 
different value, e.g. f(x) = x + 3 would be a mathematical function it takes a value, x, and returns a different value, so if x = 3 then f(x) would return 7. Haskell functions are 
like mathematical functions because they also take values (known as arguments) and return a different value. They also always output the same result depending on the value passed into 
them. But Haskell functions can also take strings, characters, and boolean values, while mathematical functions cannot as they can only take numerical values such as integers or float.
Both mathematical and Haskell functions can be used to define high-order functions and both only return sing values.

So to conclude, Haskell functions and mathematical functions are very similar in that they take in values and perform operations on these values to then return (or output) a result
depending on the values they took, both of theses functions also only return a single value,i.e one value passed into the functions cannot return multiple results. They are 
different because in Mathematical functions they can only take mathematical value such as real values or Integers.  

(c) Explain what a higher-order function is (use examples to support your answer).

A high order function is a function  that returns another function or can take another function as an argument, they are differnet from first-functions that only take and return values.
an example of high-order functions would be; a mapping function that applies a given functio to multiple element, or a list of elements, that were passed to the function to give a list of
elements with each element having been a result from the passsed function.

-}



----Part 2----
--(a)--
{-
This is a type declaration for a type called Horse that is a tuple, with the first element being a string representing the horse's name and the second 
element is an integer representing the horse's height.
-}
type Horse = (String, Int)

--(b)--
{-
This create_horse_list function takes a list of strings and a list of integers and then uses the zip function to return a list of tuples, with the first element of the tuple being a string 
and the second element being an integer. ( or just a list of the data type Horse specified in the previous exercise )
-}
create_horse_list :: [String] -> [Int] -> [Horse]
create_horse_list n h = zip n h

--(c)--
{-
This is the sort_horse_list which takes a list of the elements with the data type Horse (defined in the previous exercise) and returns a sorted, in ascending order, version of that Horse list
using pattern matching and list comprehension to perform a quick sort on the list. On the second element of the horse tuple. 
-}
sort_horse_list :: [Horse] -> [Horse] 
sort_horse_list [] = []
sort_horse_list ((n,h):hs) = sort_horse_list [(x,y)|(x,y)<-hs,y<=h]++[(n,h)]++sort_horse_list[(x,y)|(x,y)<-hs,y>h]

--(d)--
{-
This remove_smallest_horses function takes an integer for how many horses need to be removed from a list and takes the list of horses to return a list of the remaining horses. 
It recursively calls the function with the number of horses to remove being decremented each time, and the tail (the list without the first element) of the sorted list is passed. 
Terminating when the number of horses needed to be removed equals 0, and it just returns the potentially shortened list.
-}
remove_smallest_horses :: Int -> [Horse] -> [Horse]
remove_smallest_horses k l
   | k > 0 = remove_smallest_horses (k-1)  (tail (sort_horse_list l))
   |otherwise = l

--(e)--
{-
This remove_tall_horses removes the horses that are taller than 152cm from the horse list; it does this by the use of list comprehension, performing a boolean operation on
the second element of the horse tuple.
-}
remove_tall_horses :: [Horse] -> [Horse]
remove_tall_horses l = [(x,y) | (x,y) <- l,y<=152]



----Part 3----
--(a)--
{-
This is the duplicate function I used to multiply a string several times.

This function was made before i knew about the built in replicate function.
-}
duplicate :: String -> Int -> String
duplicate s 0 = ""
duplicate s n = s ++ duplicate s (n-1)

{-
This is the step function that takes three integers m n p and displays a pattern that has p number of incrementing iterations of a pattern of n width and m height concatinated with the
the reverse of this pattern, the main function steps concatenate the pattern and the reverse of the pattern function.

In the "where" section, we have the functions pattern, reversePattern, and aPattern:

The Pattern function creates each step in the step pattern by decrementing the number of steps given by the user, p, which is passed into the Pattern function as v. 
The n, which is the width of the pattern, multiplied by v, gives us the width of the step we are creating, then multiplied by m to give us the height of the steps we are creating.
Then we put the pattern steps into a list of strings. Each element is a string that represents a step in the pattern. Finally, the function returns this list.

The aPattern function concatenates a list of strings into a single string. This string represents the step pattern.

The reversePattern function reverses a list of strings and outputs it. This will be used to generate the reverse of the step pattern.
-}
steps :: Int -> Int-> Int -> String
steps m n p = aPattern (reversePattern (pattern p)) ++ aPattern (pattern p)
    where

    pattern :: Int -> [String]
    pattern v
        |v == 0 = [""]
        |otherwise =  duplicate ((duplicate "*" (n*v)) ++ "\n") m:pattern (v-1)
    
    reversePattern :: [String] -> [String]
    reversePattern [] = []
    reversePattern (l:ls) = reversePattern ls ++ [l]

    aPattern :: [String] -> String
    aPattern [] = ""
    aPattern (l:ls) = l ++ aPattern ls
   
--(b)--
{-
Our main function in this part is the flagpattern function that takes two integers from the user n and m and displays a flag pattern using these functions; n is the dimensions of the 
the first pattern to be displayed, and m is the number of decremented flag dimensions to be displayed.

In this part, we use the functions oddRow, evenRow, oddpattern, evenpattern and flagpattern:

flag pattern is the main function explained above.

oddRow is the function used to create a row in the odd pattern flag (a pattern with an odd number of dimensions). It takes the dimensions of the flag pattern we are making. 
(assuming the value given for dimensions is odd), and the row number (between 1 and the pattern dimensions). If the row number is one or d, then the row will be a line of "*"s
otherwise, a row will be made in the rowMade function. In the "where" section, we have:

	The value middle holds the number directly in between 1 and the dimensions of the odd pattern, ((d `div` 2) + 1) because it is an odd number of dimensions the middle is 
      calculated this way; for example, middle of 9 => 9 `div` 2 => 4 + 1 = 5 
	
	The positionnValue function is a function that takes two integers, the first one being the dimensions of the pattern being made, d, and the second being the index
	(or column number) of the digit, we are assigning a character too. The function then returns a character value (a character because this will be put into a list of characters). 
	So if the index is equal to 1 or the dimensions of the shape, the value of that position will be "*", If the index is less than or equal to the middle and the index is equal
	the row number, then the value is "*", and when the index is bigger than the middle and the sum of the index and the row number is equal to the dimensions of the pattern + 1
	then the value is "*", this rule flip when the row number is larger than the middle, e.g, the value = "*" when the index is > than the middle and n == i. otherwise the value = " "

	rowMade is a function that maps the index of the row values to the function positionvalue and collects a list of charecters that have been generated to represent a row.

evenRow is a function used to creat a row in the even patter flag (a pattern with an even number of dimensions) is it exactly the same as teh oddRow function explained above but the 
middle value saved uses a differnce calculation (d `div` 2) ad the middle of an even integer is just that integer divided by two. 

oddpattern function is a function that takes two integers, the first is the dimensions (assuming it is odd) of the pattern to be made, and the next is the starting row number of the 
pattern to be made. Using recursion, it concatenates the string given by the oddRow function with itself, with the second number incremented by one each time. It terminates when the 
second and first numbers are equal.

evenpattern is the same as the oddpattern function. but it only works if the dimension passed to it is even. It concatenates itself with the evenRow function instead.

POSSIBLE IMPROVEMENT = making the rowMade function just a variable that stores the mapping of the positionvalue function on the index values.
-}
oddRow :: Int -> Int -> String
oddRow d n
    | (n == 1) || (n == d) = (duplicate "*" d) ++ "\n"
    | otherwise = (rowMade n) ++ "\n"
    where
    middle = (d `div` 2) + 1

    positionValue :: Int -> Int -> Char
    positionValue d i
        |(i == d) || (i == 1) || (i <= middle && (i == n))  = '*'
        |(i > middle) && ((i+n) == (d+1)) = '*'
        |(n > middle) && ( (i > middle && (i==n) ) || ((i <= middle) && (i+n==d+1)) ) = '*'
        |otherwise = ' '

    rowMade :: Int -> [Char]
    rowMade _ = map (positionValue (d)) ([1..d])

evenRow :: Int -> Int -> String
evenRow d n
    | (n == 1) || (n == d) = (duplicate "*" d) ++ "\n"
    | otherwise = (rowMade n) ++ "\n"
    where
    middle = (d `div` 2)

    positionValue :: Int -> Int -> Char
    positionValue d i
        |(i == d) || (i == 1) || (i <= middle && (i == n))  = '*'
        |(i > middle) && ((i+n) == (d+1)) = '*'
        |(n > middle) && ( (i > middle && (i==n) ) || ((i <= middle) && (i+n==d+1)) ) = '*'
        |otherwise = ' '

    rowMade :: Int -> [Char]
    rowMade l = map (positionValue (d)) ([1..d])

oddpattern :: Int -> Int -> String
oddpattern d l
    |(d == l) = oddRow d l
    |otherwise = oddRow d l ++ oddpattern d (l+1)

evenpattern :: Int -> Int -> String
evenpattern d l
    |(d == l) = evenRow d l
    |otherwise = evenRow d l ++ evenpattern d (l+1)

flagpattern :: Int -> Int -> String
flagpattern n m
    |m == 0 = ""
    |n `mod` 2 == 0 = (evenpattern n 1) ++ flagpattern (n-1) (m-1)
    |otherwise = (oddpattern n 1) ++ flagpattern (n-1) (m-1)



----Part 4----
{-
compatibility is a function that takes two strings representing people's names and after crossing out similar charecters from one another and aligning them to a string with the 
word lahi repeated 20 times the function returns a string with the explanation of how each person feels about eachother.

In the where section we have:
	modifiedn which stores a list of all the charecters that are in the first string that are not in the second string
	modifiednn sores a list of all the charcters that are not in the first string but are in the second string

	Feelings stores the string lahi repeated 20 times using the duplicate function I defined in the previous parts.

	person1 stores a tuple of each character in from modiffiedn with a charecter from the feelings string, using the zip function in a list comprehension
	person2 stores a tuple of each character in from modiffiednn with a charecter from the feelings string, using the zip function in a list comprehension

	personfeels takes a list of tuples with both elements in the tuple being a charecter, it then takes the last element of the list and checks what the second value in that tuple is, 
	it then returns a specific string depending on what the character in the second element of the last tuple is, i.e,:
      if it = 'l' it will return " likes "
      if it = 'a' it will return " admires "
      if it = 'h' it will return " hate "
      if it = 'i' it will return " is indiffernt to  "
	and if the list is empty, so this is when the two strings passed to compatability are the same thing, then the function will return " is indiffernt to  "

This solution has a limitation in that if a a name with more than 80 charcters is entered then the function will give the wrong result.
-}
compatibility :: String -> String -> String
compatibility n nn = n ++ (personfeels (person1)) ++ nn ++ " and " ++ nn ++ (personfeels (person2)) ++ n
    where

    modifiedn = [x | x <- n,x `notElem` nn]
    modifiednn = [x | x <- nn, x `notElem` n]

    feelings = duplicate "lahi" 20 

    person1 = [(c,f) | (c,f) <- zip modifiedn feelings]
    person2 = [(c,f) | (c,f) <- zip modifiednn feelings]

    personfeels :: [(Char,Char)] -> String
    personfeels p
        |p == [] = " is indifferent to "
        |snd (last p) == 'l' = " likes "
        |snd (last p) == 'a' = " admires"
        |snd (last p) == 'h' = " hates "
        |otherwise = " is indifferent to "



----Part 5----

--(a)--
{-
The main function in this part is the split function which is a polymorphic function that takes a list of any type and a value of the same type and splits up
the list into sub list at the points on the list where the value appears. It does this by checking if the list is not empty, and if the list of values in the list up to 
the specified value is not empty it applies the takeWhile funtion to the list given until the value specified by the user is reached, then using recursion, appends the 
recalled split function with a list of everything up to the specified value in the list being dropped and the specified value being passed to it. It does this using the 
safe tail function that will not return an error if an empty list is passed to it and once the list passed too the function is empyt it stops.
-}
safetail :: [a] -> [a]
safetail [] = []
safetail (x:xs) = xs

split ::(Eq a) => [a] -> a -> [[a]]
split [] _ = []
split l v
   |takeWhile(/=v) l /= [] = takeWhile (/=v) l:split (safetail(dropWhile (/=v) l)) v
   |otherwise = [] ++ split (safetail(dropWhile (/=v) l)) v

--(b)--
{-
concatinate the rest of the string to the replaced word and add the takewhile values and the swapwords function with the dropwhile space function added to the rest.

concatinate the top and all the elements taken up to the space and the recalled swapwords function with w1 w2 and the the rest of the string with the spaces dropepd.
-}
swapwords :: String -> String -> String -> String
swapwords w1 w2 s
 | top == [ ] = [ ]
 | top == w1 = w2 ++ takeWhile(==' ') rest ++swapwords w1 w2 (dropWhile (==' ') rest)
 | otherwise = top ++ takeWhile(==' ') rest ++ swapwords w1 w2 (dropWhile (==' ') rest)
 where
 top = take (length w1) s
 rest = drop (length w1) s

