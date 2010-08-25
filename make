ghc --make Main.hs
mv Main HBan
cabal configure
cabal build -v
