-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
module Utils.Logging where

import Control.Monad.Trans
import Control.Monad.Except
import System.IO


class Monad m => MonadLog m where
	doLog :: String -> m ()


instance MonadLog IO where
	doLog str =
		do
			hPutStrLn stderr $
				unlines $
				[ "----------------------------------------------------------------"
				, str
				]
			hFlush stderr
 
instance MonadLog m => MonadLog (ExceptT e m) where
	doLog = lift . doLog
