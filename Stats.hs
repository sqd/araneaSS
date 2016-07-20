module Stats (
    Stats,
    StatsC,
    newStats,
    clearStats,
    newStatsC,
    getStats
    ) where

import Data.Conduit

import qualified Data.IntMap as M

import Control.Concurrent.MVar

import Control.Monad.Trans

import Data.ByteString as BS

import Common


type Stats = MVar (M.IntMap Integer)
type StatsC = Conduit ByteString IO ByteString

newStats :: IO Stats
newStats = newMVar M.empty

clearStats :: Stats -> IO ()
clearStats x = modifyMVar_ x $ return . M.map (const 0)

newStatsC :: Port -> Stats -> StatsC
newStatsC port s = awaitForever $ \d -> do
    liftIO $ modifyMVar_ s
        $ return . M.adjust (+ (fromIntegral $ BS.length d)) port
    yield d

getStats :: Stats -> IO (M.IntMap Integer)
getStats = readMVar
