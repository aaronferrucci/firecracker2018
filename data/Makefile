all: processed_10k.txt processed_5k.txt

processed_10k.txt: raw_10k Makefile
	dos2unix -n raw_10k raw_10k.unix
	paste -d '\t' - - - < raw_10k.unix > processed_10k.txt

processed_5k.txt: raw_5k Makefile
	dos2unix -n raw_5k raw_5k.unix
	paste -d '\t' - - - < raw_5k.unix > processed_5k.txt

