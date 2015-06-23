module Parse.Format where

--import GrammarFormat
import Text.Parsec as P hiding(many, (<|>))
--import Control.Monad
--import Control.Applicative


data ParseFormat
	= ParseFormat {
		parseFormat_symbol ::
			Parsec String () () -> Parsec String () (Either String String),
		--parseFormat_terminal :: Parsec String () String,
		parseFormat_or :: [Parsec String () String],
		parseFormat_prodSign :: [Parsec String () String],
		parseFormat_whitespaces :: [Parsec String () String],
		parseFormat_comment :: [Parsec String () String],
		parseFormat_prodSep :: [Parsec String () String]
	}
	--deriving (Show)
