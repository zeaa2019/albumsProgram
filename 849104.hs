--
-- MATHFUN
--
-- 849104
--

--
-- Types
import Data.Char
import Data.List
import Text.Printf
import Data.List (sortBy)
import Data.Ord (comparing)

-- data Album = Album String String Int Int
                -- deriving (Eq,Ord,Show,Read)

data Album = Album {  albumName :: String,
        artistName :: String,
        year :: Int,
        sales :: Int
        } deriving (Eq,Ord,Show,Read)

testData :: [Album]
testData = [(Album "Greatest Hits" "Queen" 1981 6300000),
           (Album "Gold: Greatest Hits" "ABBA" 1992 5400000),
           (Album "Sgt. Pepper's Lonely Hearts Club Band" "The Beatles" 1967 5340000),
           (Album "21" "Adele" 2011 5110000),
           (Album "(What's the Story) Morning Glory?" "Oasis" 1995 4940000),
           (Album "Thriller" "Michael Jackson" 1982 4470000),
           (Album "The Dark Side of the Moon" "Pink Floyd" 1973 4470000),
           (Album "Brothers in Arms"  "Dire Straits" 1985 4350000),
           (Album "Bad" "Michael Jackson" 1987 4140000),
           (Album "Rumours" "Fleetwood Mac" 1977 4090000),
           (Album "Greatest Hits II" "Queen" 1991 3990000),
           (Album "Back to Black" "Amy Winehouse" 2006 3940000),
           (Album "The Immaculate Collection" "Madonna" 1990 3700000),
           (Album "25" "Adele" 2015 3500000),
           (Album "Stars" "Simply Red" 1991 3450000),
           (Album "Come On Over" "Shania Twain" 1998 3430000),
           (Album "x" "Ed Sheeran" 2014 3380000),
           (Album "Legend" "Bob Marley" 1984 3380000),
           (Album "Bat Out of Hell" "Meat Loaf" 1977 3370000),
           (Album "Back to Bedlam" "James Blunt" 2004 3360000),
           (Album "Urban Hymns" "The Verve" 1997 3340000),
           (Album "Bridge over Troubled Water" "Simon & Garfunkel" 1970 3260000),
           (Album "1" "The Beatles" 2000 3230000),
           (Album "Spirit" "Leona Lewis" 2007 3170000),
           (Album "Crazy Love" "Michael Bublé" 2009 3130000),
           (Album "No Angel" "Dido" 2000 3090000),
           (Album "White Ladder" "David Gray" 1998 3020000),
           (Album "The Fame" "Lady Gaga" 2009 2990000),
           (Album "Only by the Night" "Kings of Leon" 2008 2980000),
           (Album "A Rush of Blood to the Head" "Coldplay" 2002 2960000),
           (Album "Talk on Corners" "The Corrs" 1997 2960000),
           (Album "Spice" "Spice Girls" 1996 2960000),
           (Album "Life for Rent" "Dido" 2003 2900000),
           (Album "Beautiful World" "Take That" 2006 2880000),
           (Album "The Joshua Tree" "U2" 1987 2880000),
           (Album "Hopes and Fears" "Keane" 2004 2860000),
           (Album "The War of the Worlds" "Jeff Wayne" 1978 2800000),
           (Album "X&Y" "Coldplay" 2005 2790000),
           (Album "Jagged Little Pill" "Alanis Morissette" 1995 2780000),
           (Album "Tubular Bells" "Mike Oldfield" 1973 2760000),
           (Album "Scissor Sisters" "Scissor Sisters" 2004 2760000),
           (Album "...But Seriously" "Phil Collins" 1989 2750000),
           (Album "Tracy Chapman" "Tracy Chapman" 1988 2710000),
           (Album "Parachutes" "Coldplay" 2000 2710000),
           (Album "The Man Who" "Travis" 1999 2687500),
           (Album "Greatest Hits" "ABBA" 1975 2606000),
           (Album "I've Been Expecting You" "Robbie Williams" 1998 2586500),
           (Album "Come Away with Me" "Norah Jones" 2002 2556650),
           (Album "Graceland" "Paul Simon" 1986 2500000),
           (Album "Ladies & Gentlemen: The Best of" "George Michael" 1998 2500000)]



 removeAlbum :: [Album] -> [Album]
 removeAlbum listOfAlbums = init listOfAlbums

 getListOfArtists :: [Album] -> [String]
 getListOfArtists listOfAlbums = nub [artist | (Album albumName artist year sales) <- listOfAlbums]

 getArtists :: [Album] -> [String]
 getArtists listOfAlbums = [artist | (Album albumName artist year sales) <- listOfAlbums]

 countNumAlbums :: String -> Int
 countNumAlbums artist = length (filter (==artist) (getArtists testData))

 countNumAlbumsList :: [Album] -> [Int]
 countNumAlbumsList listOfAlbums = map countNumAlbums (getListOfArtists listOfAlbums)

 listOfArtists :: [Album] -> [String]
 listOfArtists listOfAlbums = nub [artist | (Album albumName artist year sales) <- listOfAlbums]

 deleteAlbum :: Album -> [Album] -> [Album]
 deleteAlbum albumToDelete listOfAlbums = delete albumToDelete listOfAlbums

 sortBySales :: [Album] -> [Album]
 sortBySales = reverse . sortBy (comparing sales)

 getAlbum :: String -> String -> [Album] -> Album
 getAlbum artistName nameOfAlbum listOfAlbums = head [(Album albumName artist year sales) | (Album albumName artist year sales) <- listOfAlbums, artistName == artist && nameOfAlbum == albumName]

 addAlbum :: Album -> [Album] -> [Album]
 addAlbum (Album albumName artist year sales) (x:xs) = (Album albumName artist year sales) : (x:xs)

 createNewAlbum :: String -> String -> Int -> Int -> Album
 createNewAlbum newAlbumName newAlbumArtist newAlbumYear newAlbumSales = (Album newAlbumName newAlbumArtist newAlbumYear newAlbumSales)

 getSales :: String -> [Album] -> Int
 getSales nameOfAlbum listOfAlbums = head [sales | (Album albumName artist year sales) <- listOfAlbums, nameOfAlbum == albumName]

 getYear :: String -> [Album] -> Int
 getYear nameOfAlbum listOfAlbums = head [year | (Album albumName artist year sales) <- listOfAlbums, nameOfAlbum == albumName]

 --Functionality Code
 albumsToString :: [Album] -> IO()
 albumsToString listOfAlbums = putStrLn (show listOfAlbums)

 top10 :: [Album] -> [Album]
 top10 listOfAlbums = take 10 listOfAlbums

 albumRelease :: [Album] -> Int -> Int -> [Album]
 albumRelease listOfAlbums fromY toY  = [(Album albumName artist year sales) | (Album albumName artist year sales) <- listOfAlbums, year >= fromY && year <= toY]

 albumTitles :: [Album] -> Char -> [Album]
 albumTitles listOfAlbums prefix = [(Album albumName artist year sales) | (Album albumName artist year sales) <- listOfAlbums, head albumName == prefix]

 albumTotalSales :: [Album] -> String -> Int
 albumTotalSales listOfAlbums artistName = foldr (+) 0 [sales | (Album albumName artist year sales) <- listOfAlbums, artist == artistName]

 getNumAlbumsArtist:: [Album] -> [(String, Int)]
 getNumAlbumsArtist listOfAlbums = zip (getListOfArtists listOfAlbums) (countNumAlbumsList (nub listOfAlbums))

 addRemoveAlbum :: Album -> [Album] -> [Album]
 addRemoveAlbum newAlbum listOfAlbums = sortBySales (addAlbum newAlbum (removeAlbum listOfAlbums))

 increaseSales :: String -> String -> Int -> [Album] -> [Album]
 increaseSales artistName nameOfAlbum additionalSales listOfAlbums = sortBySales (addAlbum (createNewAlbum nameOfAlbum artistName (getYear nameOfAlbum listOfAlbums) ((getSales nameOfAlbum listOfAlbums) + additionalSales)) (deleteAlbum (getAlbum artistName nameOfAlbum listOfAlbums) listOfAlbums))


-- Demo function to test basic functionality (without persistence - i.e.
-- testData doesn't change and nothing is saved/loaded to/from albums file).

--demo :: Int -> IO ()
--demo 1  = putStrLn (albumsToString testData)
--demo 2  = putStrLn (albumsToString (top10 testData))
--demo 3  = putStrLn ( all albums released between 2000 and 2008 inclusive )
--demo 4  = putStrLn ( all albums with titles beginning with "Th" )
--demo 5  = putStrLn ( total sales figure for "Queen" )
--demo 6  = putStrLn ( all artists with the number of times they appear in top 50 )
--demo 7  = putStrLn ( albums after removing 50th album and adding "Progress"
--                     by "Take That" from 2010 with 2700000 sales )
--demo 8  = putStrLn ( albums after increasing sales of "21" by "Adele" by 400000 )

--
--
-- Your user interface (and loading/saving) code goes here
--
--
