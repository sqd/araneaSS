import Manager
import CmdOption
import Connection


main = do
    opt <- getOption
    config <- getConfig opt
    checkConfig config
    runManager config
