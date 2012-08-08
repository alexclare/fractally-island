
import Control.Monad.State
import Data.Array
import Data.Char
import Data.List
import Graphics.GD
import System.Environment
import System.IO
import System.Random

{-|
Perform one iteration of the diamond-square algorithm, adding in an amount of
random variation proportional to the flux argument.

Returns an extrapolated array twice as large in either dimension.
-}
diamondSquare :: (Ix a, Integral a, Fractional e, Random e, RandomGen g) =>
                 Array (a, a) e -> e -> g -> (Array (a, a) e, g)
diamondSquare grid flux g = (grid', g')
    where (rows, cols) = snd $ bounds grid
          grid' = array ((1, 1), (rows*2, cols*2))
                  [((i,j), next i j) | i <- [1..rows*2], j <- [1..cols*2]]
          variation = listArray ((1, 1), (rows, cols))
                      (map (\x -> (2 * x - 1) * flux) rs)
          (rs, g') = runState (replicateM (fromIntegral rows * fromIntegral cols)
                               (state random)) g
          -- perhaps replace this tedious formula with some list comprehension
          next row col | even row && even col = prev 0 0
                       | even row             = (3*prev 0 0 + 3*prev 0 1)/8 +
                                                (prev (-1) 0 + prev (-1) 1 +
                                                 prev 1 0 + prev 1 1)/16 +
                                                (rand (-1) 0 + rand 0 0)/4
                       | even col             = (3*prev 0 0 + 3*prev 1 0)/8 +
                                                (prev 0 (-1) + prev 1 (-1) +
                                                 prev 0 1 + prev 1 1)/16 +
                                                (rand 0 (-1) + rand 0 0)/4
                       | otherwise            = (prev 0 0 + prev 1 0 +
                                                 prev 0 1 + prev 1 1)/4 +
                                                rand 0 0
                       where prev ro co = grid ! offset ro co
                             rand ro co = variation ! offset ro co
                             offset ro co = ((row `div` 2 + ro) `mod` rows + 1,
                                             (col `div` 2 + co) `mod` cols + 1)

-- | Generate terrain using a given initial shape, amplitude, and iterations
terrain :: (Ix a, Integral a, Eq i, Num i, Floating e, Random e, RandomGen g) =>
           ((a, a), (a, a)) -> i -> e -> g -> (Array (a, a) e, g)
terrain size iters amp g = go iters amp init g
    where init = listArray size (repeat 0.0)
          go iter flux grid rng | iter == 0 = (grid, rng)
                                | otherwise = go (iter-1) (flux / 2.0) grid' rng'
                                where (grid', rng') = diamondSquare grid flux g

writeTXT :: (Ix a, Integral a, Show e) =>
            Array (a, a) e -> String -> IO ()
writeTXT grid name = do
    outh <- openFile name WriteMode
    forM_ (assocs grid) (dump outh)
    hClose outh
    where
        dump outh ((row, col), val)
            | col `mod` cols == 0 = hPutStrLn outh (show val)
            | otherwise           = hPutStr outh (show val ++ " ")
        cols = snd $ snd $ bounds grid

writePNG :: (Ix a, Integral a, RealFrac e) =>
            Array (a, a) e -> String -> IO ()
writePNG grid name = do
    img <- newImage (fromIntegral rows, fromIntegral cols)
    forM_ (assocs grid) (paint img)
    savePngFile name img
    where paint img ((x, y), val) = setPixel (fromIntegral x - 1, fromIntegral y - 1)
                                             ((color . norm) val) img
          min = minimum (elems grid)
          max = maximum (elems grid)
          norm x = (x - min) / (max - min)
          color x | x < 0.6   = flm 128 128 255 x
                  | otherwise = flm 128 255 128 x
          flm r g b x = rgb (floor $ r * x) (floor $ g * x) (floor $ b * x)
          (rows, cols) = snd $ bounds grid

main :: IO ()
main = do
    -- Move more static stuff to the command line
    let grid = listArray ((1,1),(3,4)) (repeat 1.0) :: (Array (Int, Int) Float)
        -- rng = mkStdGen 10
    rng <- newStdGen
    let result = fst (terrain ((1, 1), (4, 3)) 7 (128.0::Float) rng)
    getArgs >>= mapM_ (\arg -> if "png" `isSuffixOf` map toLower arg
                               then writePNG result arg
                               else writeTXT result arg)
