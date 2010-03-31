for file in euler[0-9]*.hs; do echo $file; runhaskell $file; echo; done
