These R scripts assume that you have downloaded Transparify's raw rating data and want to understand how the sauce is made. That's transparent!

The data assumes that you have the data file in a directory called ```krip_data``` and the code in a file called ``krip_code``` . In order to set up these directories:
```bash
cd $MYDIR
mkdir krip_data krip_code
cd krip_code
```

The you can clone this repository into your ```krip_code`` file
```bash
git clone https://github.com/aserlich/transparify
```

Or you can simply download the repository.

Then load up R.

In R type
```R
source(format_transparify.r)
```
You will then get the Krippendorff Alpha statistics. The program will also output the data format for you to use in other functions in the ```irr`` library in R.

Have fun!
