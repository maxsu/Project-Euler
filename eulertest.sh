for file in euler[0-9][0-9][0-9]; do echo $file; runhaskell "$file/$file.hs"; echo; done
