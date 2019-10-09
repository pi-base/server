module Data.PullRequest
  ( PullRequest(..)
  , PullRequestError(..)
  ) where

import Import

data PullRequest = PullRequest
  { url :: Text
  } deriving (Generic, Show, Eq)

data PullRequestError = PullRequestError
  { message :: Text
  } deriving (Generic, Show)
