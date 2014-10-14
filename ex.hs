module Main where

import Control.Applicative
import Control.Monad
import FRP.Elerea.Clocked


main :: IO ()
main = do
    testSignal drift


simple :: SignalGen (Signal Int)
simple = stateful 3 (+2)


divisibleBy :: (Integral a) => a -> a -> Bool
divisibleBy n x = x `mod` n == 0


counter :: SignalGen (Signal Integer)
counter = stateful 0 (+1)




edge :: Signal Bool -> SignalGen (Signal Bool)
edge = withClock (pure True) . transfer False (\b b' -> b && not b')


drift :: SignalGen (Signal (Integer, Integer))
drift = do
    time <- counter
    t1 <- edge ((\_ -> True) <$> time)
    t3 <- edge (divisibleBy 3 <$> time)
    c1 <- withClock t1 counter
    c3 <- withClock t3 counter
    return ((,) <$> time <*> c3)


testSignal :: (Show a) => SignalGen (Signal a) -> IO ()
testSignal gen = do
    smp <- start gen
    loop (smp >>= print)
    where loop m = m >> loop m


-- testSignal s0 = loop s0 w0
--     where
--     loop s' w' = do
--         (ds, s) <- stepSession s'
--         let Identity (mx, w) = stepWire w' ds (Right ())
--         liftIO $ do
--             putChar '\r'
--             putStr (either (\ex -> "I: " ++ show ex) show mx)
--             putStr "\027[K"
--             hFlush stdout
--         loop s w
