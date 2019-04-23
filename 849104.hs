--
-- MATHFUN
--
-- 849104
--


--Types
import Data.Char
import Data.List
import Data.List (sortBy)
import Data.Ord (comparing)
import Text.Printf

                
-- data Album = Album { albumName :: String,
                     -- artistName :: String,
                     -- year :: Int,
                     -- sales :: Int
                   -- } deriving (Eq,Ord,Show,Read)
                   
data Album = Album String String Int Int
            deriving (Eq, Ord, Show, Read)
   

testData :: [Album]
testData =[(Album  "Greatest Hits"                                 "Queen"             1981  6300000),
           (Album  "Gold: Greatest Hits"                           "ABBA"              1992  5400000), 
           (Album  "Sgt. Pepper's Lonely Hearts Club Band"         "The Beatles"       1967  5340000),
           (Album  "21"                                            "Adele"             2011  5110000),
           (Album  "(What's the Story) Morning Glory?"             "Oasis"             1995  4940000),
           (Album  "Thriller"                                      "Michael Jackson"   1982  4470000), 
           (Album  "The Dark Side of the Moon"                     "Pink Floyd"        1973  4470000),
           (Album  "Brothers in Arms"                              "Dire Straits"      1985  4350000), 
           (Album  "Bad"                                           "Michael Jackson"   1987  4140000),
           (Album  "Rumours"                                       "Fleetwood Mac"     1977  4090000), 
           (Album  "Greatest Hits II"                              "Queen"             1991  3990000),
           (Album  "Back to Black"                                 "Amy Winehouse"     2006  3940000),
           (Album  "The Immaculate Collection"                     "Madonna"           1990  3700000), 
           (Album  "25"                                            "Adele"             2015  3500000),
           (Album  "Stars"                                         "Simply Red"        1991  3450000), 
           (Album  "Come On Over"                                  "Shania Twain"      1998  3430000),
           (Album  "x"                                             "Ed Sheeran"        2014  3380000), 
           (Album  "Legend"                                        "Bob Marley"        1984  3380000),
           (Album  "Bat Out of Hell"                               "Meat Loaf"         1977  3370000),
           (Album  "Back to Bedlam"                                "James Blunt"       2004  3360000),
           (Album  "Urban Hymns"                                   "The Verve"         1997  3340000),
           (Album  "Bridge over Troubled Water"                    "Simon & Garfunkel" 1970  3260000),
           (Album  "1"                                             "The Beatles"       2000  3230000),
           (Album  "Spirit"                                        "Leona Lewis"       2007  3170000),
           (Album  "Crazy Love"                                    "Michael Buble"     2009  3130000),
           (Album  "No Angel"                                      "Dido"              2000  3090000),
           (Album  "White Ladder"                                  "David Gray"        1998  3020000),
           (Album  "The Fame"                                      "Lady Gaga"         2009  2990000),
           (Album  "Only by the Night"                             "Kings of Leon"     2008  2980000),
           (Album  "A Rush of Blood to the Head"                   "Coldplay"          2002  2960000),
           (Album  "Talk on Corners"                               "The Corrs"         1997  2960000),
           (Album  "Spice"                                         "Spice Girls"       1996  2960000),
           (Album  "Life for Rent"                                 "Dido"              2003  2900000),
           (Album  "Beautiful World"                               "Take That"         2006  2880000),
           (Album  "The Joshua Tree"                               "U2"                1987  2880000),
           (Album  "Hopes and Fears"                               "Keane"             2004  2860000),
           (Album  "The War of the Worlds"                         "Jeff Wayne"        1978  2800000),
           (Album  "X&Y"                                           "Coldplay"          2005  2790000),
           (Album  "Jagged Little Pill"                            "Alanis Morissette" 1995  2780000),
           (Album  "Tubular Bells"                                 "Mike Oldfield"     1973  2760000),
           (Album  "Scissor Sisters"                               "Scissor Sisters"   2004  2760000),
           (Album  "...But Seriously"                              "Phil Collins"      1989  2750000),
           (Album  "Tracy Chapman"                                 "Tracy Chapman"     1988  2710000),
           (Album  "Parachutes"                                    "Coldplay"          2000  2710000),
           (Album  "The Man Who"                                   "Travis"            1999  2687500),
           (Album  "Greatest Hits"                                 "ABBA"              1975  2606000),
           (Album  "I've Been Expecting You"                       "Robbie Williams"   1998  2586500),
           (Album  "Come Away with Me"                             "Norah Jones"       2002  2556650),
           (Album  "Graceland"                                     "Paul Simon"        1986  2500000),
           (Album  "Ladies & Gentlemen: The Best of"               "George Michael"    1998  2500000)]

-- Helper functions

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
         
sortF :: Album -> Album -> Ordering
sortF (Album albumNameA artistA yearA salesA) (Album albumNameB artistB yearB salesB) | salesA > salesB = LT
                                                                                      | otherwise       = GT

getAlbum :: String -> String -> [Album] -> Album
getAlbum artistName nameOfAlbum listOfAlbums = head [(Album albumName artist year sales) | (Album albumName artist year sales) <- listOfAlbums, artistName == artist && nameOfAlbum == albumName]

addAlbum :: Album -> [Album] -> [Album]
addAlbum newAlbum listOfAlbums = insertBy sortF newAlbum listOfAlbums

createNewAlbum :: String -> String -> Int -> Int -> Album
createNewAlbum newAlbumName newAlbumArtist newAlbumYear newAlbumSales = (Album newAlbumName newAlbumArtist newAlbumYear newAlbumSales)

getSales :: String -> [Album] -> Int
getSales nameOfAlbum listOfAlbums = head [sales | (Album albumName artist year sales) <- listOfAlbums, nameOfAlbum == albumName]

getYear :: String -> [Album] -> Int
getYear nameOfAlbum listOfAlbums = head [year | (Album albumName artist year sales) <- listOfAlbums, nameOfAlbum == albumName]

-- Functionality Code

albumsToString :: [Album] -> String
albumsToString []                                       = []
albumsToString ((Album albumName artist year sales):xs) = printf "%s " albumName ++ printf "%s " artist ++ printf "%d " year ++ printf "%d " sales ++ albumsToString xs

top10 :: [Album] -> [Album]
top10 listOfAlbums = take 10 listOfAlbums

albumRelease :: [Album] -> Int -> Int -> [Album]
albumRelease listOfAlbums fromY toY  = [(Album albumName artist year sales) | (Album albumName artist year sales) <- listOfAlbums, year >= fromY && year <= toY]

albumTitles :: [Album] -> String -> [Album]
albumTitles listOfAlbums prefix = [(Album albumName artist year sales) | (Album albumName artist year sales) <- listOfAlbums, take (length prefix) albumName == prefix]

albumTotalSales :: [Album] -> String -> Int
albumTotalSales listOfAlbums artistName = foldr (+) 0 [sales | (Album albumName artist year sales) <- listOfAlbums, artist == artistName]

getNumAlbumsArtist:: [Album] -> [(String, Int)]
getNumAlbumsArtist listOfAlbums = zip (getListOfArtists listOfAlbums) (countNumAlbumsList (nub listOfAlbums))

addRemoveAlbum :: Album -> [Album] -> [Album]
addRemoveAlbum newAlbum listOfAlbums = addAlbum newAlbum (removeAlbum listOfAlbums)

increaseSales :: String -> String -> Int -> [Album] -> [Album]
increaseSales artistName nameOfAlbum additionalSales listOfAlbums = addAlbum (createNewAlbum nameOfAlbum artistName (getYear nameOfAlbum listOfAlbums) ((getSales nameOfAlbum listOfAlbums) + additionalSales)) (deleteAlbum (getAlbum artistName nameOfAlbum listOfAlbums) listOfAlbums)


-- Demo function to test basic functionality (without persistence - i.e.
-- testData doesn't change and nothing is saved/loaded to/from albums file).

demo :: Int -> IO ()
demo 1  = putStrLn (albumsToString testData)
demo 2  = putStrLn (albumsToString (top10 testData))
demo 3  = putStrLn (albumsToString (albumRelease testData 2000 2008))
demo 4  = putStrLn (albumsToString (albumTitles testData "Th"))
demo 5  = print (albumTotalSales testData "Queen")
demo 6  = print (getNumAlbumsArtist testData)
demo 7  = putStrLn (albumsToString (addRemoveAlbum(Album "Progress" "Take That" 2010 2700000) testData))
demo 8  = putStrLn (albumsToString (increaseSales "Adele" "21" 400000 testData))

getAlbumYear :: [Album] -> IO()
getAlbumYear albums = do
              putStrLn("Enter the year from")
              fromY <- getLine
              let yearFrom = read fromY :: Int
              putStrLn("Enter the year to")
              toY <- getLine
              let yearTo = read toY :: Int
              putStrLn (albumsToString (albumRelease albums yearFrom yearTo))
              
getAlbumPrefix :: [Album] -> IO()
getAlbumPrefix albums = do
              putStrLn("Enter album name prefix")
              albumPrefix <- getLine
              putStrLn (albumsToString (albumTitles albums albumPrefix))
              
getArtistName :: [Album] -> IO()
getArtistName albums = do
               putStrLn("Enter artist name")
               artistName <- getLine
               print (albumTotalSales albums artistName)
               
getNewAlbum :: [Album] -> IO()
getNewAlbum albums = do
              putStrLn("Enter name of album")
              albumName <- getLine
              putStrLn("Enter name of artist")
              artistName <- getLine
              putStrLn("Enter year album was released")
              albumRelease <- getLine
              let year = read albumRelease :: Int
              putStrLn("Enter number of sales")
              numSales <- getLine
              let sales = read numSales :: Int
              let newAlbum = createNewAlbum albumName artistName year sales
              putStrLn (albumsToString(addRemoveAlbum newAlbum albums))
              
              
increaseSalesOfArtist :: [Album] -> IO()
increaseSalesOfArtist albums = do
              putStrLn("Enter name of album")
              albumName <- getLine
              putStrLn("Enter name of artist")
              artistName <- getLine
              putStrLn("Enter number of additional sales")
              numSales <- getLine
              let sales = read numSales :: Int
              putStrLn (albumsToString(increaseSales artistName albumName sales albums))
              
  
  
--
--
-- Your user interface (and loading/saving) code goes here

executeFunction :: String -> [Album] -> IO ()
executeFunction "1" albums = putStrLn (albumsToString albums)
executeFunction "2" albums = putStrLn (albumsToString (top10 albums))
executeFunction "3" albums = getAlbumYear albums
executeFunction "4" albums = getAlbumPrefix albums
executeFunction "5" albums = getArtistName albums
executeFunction "6" albums = print (getNumAlbumsArtist albums)
executeFunction "7" albums = getNewAlbum albums
executeFunction "8" albums = increaseSalesOfArtist albums
executeFunction  _ albums  = main 


main :: IO ()
main = do
   albums <- readFile "albums.txt"
   let listOfAlbums = read albums :: [Album]
   putStrLn (albumsToString listOfAlbums)
   putStrLn ("---------------------------------------")
   putStrLn ("---------------------------------------")
   putStrLn ("-------Album Database Program----------")
   putStrLn ("---------------------------------------")
   putStrLn ("---------------------------------------")
   putStrLn ("1 - Convert Albums to String")
   putStrLn ("2 - Top 10 Albums in descending album")
   putStrLn ("3 - Albums released beetween given years")
   putStrLn ("4 - Albums with given prefix")
   putStrLn ("5 - Total sales for artist")
   putStrLn ("6 - Pairs of Artist and Number of Albums")
   putStrLn ("7 - Remove lowest album and add new album")
   putStrLn ("8 - Increase sale figure of album")
   putStrLn ("----------------------------------------")
   putStrLn ("Enter no: ")
   choice <- getLine
   executeFunction choice listOfAlbums
   

