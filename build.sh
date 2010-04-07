for file in euler[0-9][0-9][0-9]; do echo $file; ghc -O3 --make "$file/$file.hs"; echo; done
