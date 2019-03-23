{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( Profile (..),
    Score (..),
    profile,
    scores,
    readRank
  ) where

import Control.Monad
import Data.Either
import Data.Foldable
  ( fold
  )
import Data.Maybe
import Data.Text 
  ( Text,
    pack,
    replace,
    strip,
    unpack
  )
import Data.Text.Read
  ( Reader,
    decimal
  )
import Text.HTML.Scalpel
import Text.Read

-- url generation

baseUrl :: String
baseUrl = "https://scoresaber.com/u/"

type ProfileId = String
type PageNumber = Integer

profileUrl :: ProfileId -> PageNumber -> String
profileUrl profileId pageNumber =
  baseUrl ++ profileId ++ "&page=" ++ (show pageNumber)

-- profile

type Username = Text
type Rank = Integer
type PageCount = Integer

data Profile
  = Profile ProfileId Username Rank PageCount

-- score

type TrackName = Text
type DateTime = Text
type PP = Double

data Score
  = Score DateTime TrackName Rank PP 
    deriving Show

-- utils

infixl 9 @.
(@.) :: TagName -> String -> Selector
(@.) element className = element @: [hasClass className]

spack :: String -> Text
spack = (strip . pack)

fromRead :: Read a => a -> String -> a
fromRead otherwise = fromMaybe otherwise . readMaybe

readRank :: String -> Integer
readRank rankStr = rank
  where 
    text :: Text
    text = (strip . pack) rankStr

    uncomma :: Text
    uncomma = replace "," "" text

    unpound :: Text
    unpound = replace "#" "" uncomma

    rankE = decimal unpound
    (rank, _) = fromRight (0, "default") rankE

-- scalpel

profile :: ProfileId -> IO (Maybe Profile)
profile profileId = scrapeURL url profileData
  where
    url :: String
    url = profileUrl profileId 1

    profileData :: Scraper String Profile
    profileData = do
      (username, rank) <- profileHeader
      pageCount        <- pageCount
      return $ Profile profileId username rank pageCount

    profileHeader :: Scraper String (Username, Rank)
    profileHeader = chroot ("div" @. "box" // "div" @. "column") $ do
      username <- spack <$> text ("h5" @. "title" // "a")
      rank     <- readRank <$> text ("ul" // "li" // "a")
      return (username, rank)

    pageCount :: Scraper String PageCount
    pageCount = (read . last) <$> texts ("ul" @. "pagination-list" // "li" // "a")

scores :: Profile -> IO [Score]
scores (Profile profileId _ _ pageCount) = foldMap (pageScores profileId) [1..pageCount]

pageScores :: ProfileId -> PageNumber -> IO [Score]
pageScores profileId page = fmap fold scraped
  where
    url :: String
    url = profileUrl profileId page

    pageScores' :: Scraper String [Score]
    pageScores' = chroots ("table" @. "songs" // "tbody" // "tr") $ do 
      dateTime  <- spack <$> attr "title" ("span" @. "time")
      trackName <- spack <$> text ("a" // "span" @. "pp")
      rank      <- readRank <$> text ("th" @. "rank")
      pp        <- fromRead 0.0 <$> text ("span" @. "ppValue")
      return $ (Score dateTime trackName rank pp)

    scraped :: IO (Maybe [Score])
    scraped = scrapeURL url pageScores'