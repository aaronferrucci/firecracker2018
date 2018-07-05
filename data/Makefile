processed_10k.txt: raw_10k Makefile
	dos2unix -n raw_10k raw_10k.unix
	paste -d '\t' - - - < raw_10k.unix > processed_10k.txt

