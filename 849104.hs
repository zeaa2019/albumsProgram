--
-- MATHFUN
--
-- 849104
--

--
-- Types
--data Album = AlbumDetails String String Int Int
                deriving (Eq,Ord,Show,Read)

testData :: [Album]
testData = [AlbumDetails "Greatest Hits" "Queen" 1981 6300000, AlbumDetails "Gold: Greatest Hits" "ABBA" 1992 5400000, AlbumDetails "Sgt. Pepper's Lonely Hearts Club Band" "The Beatles" 1967 5340000, AlbumDetails "21" "Adele" 2011 5110000, AlbumDetails "(What's the Story) Morning Glory?" "Oasis" 1995 4940000, AlbumDetails "Thriller" "Michael Jackson" 1982 4470000, AlbumDetails "The Dark Side of the Moon" "Pink Floyd" 1973 4470000, AlbumDetails "Brothers in Arms"  "Dire Straits" 1985 4350000 , AlbumDetails "Bad" "Michael Jackson" 1987 4140000, AlbumDetails "Rumours" "Fleetwood Mac" 1977 4090000, AlbumDetails "Greatest Hits II" "Queen" 1991 3990000, AlbumDetails "Back to Black" "Amy Winehouse" 2006 3940000, AlbumDetails "The Immaculate Collection" "Madonna" 1990 3700000, AlbumDetails "25" "Adele" 2015 3500000, AlbumDetails "Stars" "Simply Red" 1991 3450000, AlbumDetails "Come On Over" "Shania Twain" 1998 3430000, AlbumDetails "x" "Ed Sheeran" 2014 3380000, AlbumDetails "Legend" "Bob Marley" 1984 3380000, AlbumDetails "Bat Out of Hell" "Meat Loaf" 1977 3370000, AlbumDetails "Back to Bedlam" "James Blunt" 2004 3360000, AlbumDetails "Urban Hymns" "The Verve" 1997 3340000, AlbumDetails "Bridge over Troubled Water" "Simon & Garfunkel" 1970 3260000, AlbumDetails "1" "The Beatles" 2000 3230000, AlbumDetails "Spirit" "Leona Lewis" 2007 3170000, AlbumDetails "Crazy Love" "Michael Bublé" 2009 3130000, AlbumDetails "No Angel" "Dido" 2000 3090000, AlbumDetails "White Ladder" "David Gray" 1998 3020000, AlbumDetails "The Fame" "Lady Gaga" 2009 2990000, AlbumDetails "Only by the Night" "Kings of Leon" 2008 2980000, AlbumDetails "A Rush of Blood to the Head" "Coldplay" 2002 2960000, AlbumDetails "Talk on Corners" "The Corrs" 1997 2960000, AlbumDetails "Spice" "Spice Girls" 1996 2960000, AlbumDetails "Life for Rent" "Dido" 2003 2900000, AlbumDetails "Beautiful World" "Take That" 2006 2880000, AlbumDetails "The Joshua Tree" "U2" 1987 2880000, AlbumDetails "Hopes and Fears" "Keane" 2004 2860000, AlbumDetails "The War of the Worlds" "Jeff Wayne" 1978 2800000, AlbumDetails "X&Y" "Coldplay" 2005 2790000, AlbumDetails "Jagged Little Pill" "Alanis Morissette" 1995 2780000, AlbumDetails "Tubular Bells" "Mike Oldfield" 1973 2760000, AlbumDetails "Scissor Sisters" "Scissor Sisters" 2004 2760000, AlbumDetails "...But Seriously" "Phil Collins" 1989 2750000, AlbumDetails "Tracy Chapman" "Tracy Chapman" 1988 2710000, AlbumDetails "Parachutes" "Coldplay" 2000 2710000, AlbumDetails "The Man Who" "Travis" 1999 2687500, AlbumDetails "Greatest Hits" "ABBA" 1975 2606000, AlbumDetails "I've Been Expecting You" "Robbie Williams" 1998 2586500, AlbumDetails "Come Away with Me" "Norah Jones" 2002 2556650, AlbumDetails "Graceland" "Paul Simon" 1986 2500000, AlbumDetails "Ladies & Gentlemen: The Best of" "George Michael" 1998 2500000]

--
--
--  Your functional code goes here
--
--

albumsToString :: [Album] -> IO()
albumsToString listOfAlbums = putStrLn (show listOfAlbums)

top10 :: [Album] -> [Album]
top10 listOfAlbums = take 10 listOfAlbums

albumRelease :: [Album] -> Int -> Int -> [Album]
albumRelease listOfAlbums fromY toY  = [(AlbumDetails albumName artist year sales) | (AlbumDetails albumName artist year sales) <- listOfAlbums, year >= fromY && year <= toY]

albumTitles :: [Album] -> Char -> [Album]
albumTitles listOfAlbums prefix = [(AlbumDetails albumName artist year sales) | (AlbumDetails albumName artist year sales) <- listOfAlbums, head albumName == prefix]

albumTotalSales :: [Album] -> String -> Int
albumTotalSales listOfAlbums artistName = foldr (+) 0 [sales | (AlbumDetails albumName artist year sales) <- listOfAlbums, artist == artistName]



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
