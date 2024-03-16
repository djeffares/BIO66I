# Notes on data analysis
- started 2023-12-14


### 2023-12-14

The data is here on google drive: 
https://drive.google.com/drive/folders/12ede56NYuNhz1mOmS6zBmXKzxJ7qTLGg

Download it all to /Users/dj757/gd/modules/BIO00066I-BABS4/data

### 2023-12-17
working dir is now: /Users/dj757/gd/modules/BIO00066I
set up a git repo https://github.com/djeffares/BIO00066I

BIO00066I-workshop2.Rmd started
and BIO00066I-workshop2.html

Cleaning up all the data files:

### 2023-12-18

MSCs mesenchymal stem cells / stromal cells
    are multipotent stem cells (but not pluripotent)
    mesenchymal: defined by cell surface marker
        can diff into mesenchymal cells: bone, cartilage, fat (connective tissues)
    in lots of tissues: bone marrow, adipose, etc
    ours are from BM

MSCs: immuno-modulatory, make cytokines
    are heterogeneous
    have subpopulations - but not understood

source: BM femoral head tissues (hip bone)
    MSCs infected with telomerase --> immortalised
    cloned by limiting dilution to CF units
    creating clonal homogenous cell lines

A & B are different cell lines from **one donor**
    some are text book MSCs
    some we don't know what they do: they are nullipotent
        but more immunomod
    clone A: text book (pluripotent)
    clone B: more immunomod


**clone A**
**clone B**

Clone A: Y201 
    more migratory
    might move to a site in the body
    thinnner: len/width ratio

    B: Y202
    less potent
    more rounded
    

remove 
    area < 500 ug
    dry mass < 200 pg
    large ones?


#### 2024-01-12 data clean up and markdown work

Worked on comparing the two clones in the all-cell-data-FFT.tsv file.
Found some differences, in the script analysis-cloneA-v-cloneB-2024-01-12.R
Put this in to a markdown: comparing-clones.Rmd, which **may** be a workshop.

#### 2024-01-16 looking at analysis-cloneA-v-cloneB-2024-01-12.R: filtering
 To clean up the data, to remove 
    area < 500 ug
    dry mass < 200 pg
    large ones?
None of these make much sense, given the data.
So I filtered by quantiles: c(0.0001,0.9999) for area, dry.mass and volume
This data is saved in: all-cell-data-FFT.filtered.2024-01-16.tsv 
and all.filtering.plots.2024-01-16.pdf

#### 2024-01-16 looking at Manual Cell Tracking data (mtrackJ)

data in: /Users/dj757/gd/modules/BIO00066I/raw-data/
A1 points python output.csv renamed A1-points-python-output.csv
B2 points python output.csv B2-points-python-output.csv

improved the column names:
data/A1-points-python-output-improved-names-2024-01-16.csv

script: tracking-data-analysis.R


# 2024-01-17: summary so far


### play-with-data-2023-12-18.R
**shows** that these metrics make nice plots, as are they are very different bwteeen A and clone B
- width
- area
- length
- track.length
- length.to.width.ratio
- sphercity

See: /Users/dj757/gd/modules/BIO00066I/plots/sample.metrics.2023-12-18.jpg

script: play-with-data-2023-12-18.R
inputs: all-cell-data-FFT.tsv

TO DO:
	remove track length
	give them an excel file with different tabs?
	would the website file loading affect R project stuff (ask Emma)
	
###: Comparing and filtering data from clone A and clone B
**Shows:**
- cell volume, dry mass and cell area are very different between clone A and clone B filtering
- filtering by area < 500 ug, dry mass < 200 or for large ones doe not make much sense
- we can filter by quantiles, see: all.filtering.plots. This shows the distrbutions a little better

cloneA is thinner and longer
cloneB is wider and shorter

all params of these cells are statistically different:
Some have SUBTLE differences
"dry.mass 7.7e-166"
"volume 7.7e-166"
"length 0"
"orientation 1.9e-21"

Some are VERY DIFFERENT
"sphericity 0"
"mean.thickness 0"
"radius 0"
"area 0"
"width 0"
"displacement 0" 
"instantaneous.velocity 0"


inputs: all-cell-data-FFT.tsv
	which contains
	/Users/dj757/gd/modules/BIO00066I/raw-data/
	A1-FFT.csv
	A2-FFT.csv
	A3-FFT.csv
	B1-FFT.csv
	B2-FFT.csv
	B3-FFT.csv

script: analysis-cloneA-v-cloneB-2024-01-12.R

TO DO:
	do filtering to cut off the top ends a bit more
	add this to the workshop? do NOT trust the equipment!
		ie: the company recommended this - is this OK??
	
### Looking at Manual Cell Tracking data (mtrackJ): tracking-data-analysis
**Shows:** that speed and track length are very different between A1 and B2

script:
 	tracking-data-analysis.R

inputs:	
	A1-points-python-output.csv
	B2-points-python-output.csv

### Comparing the repeats:
**shows** that clone B, replicate 3 is a little different

script:
 	comparing-repeats.R

inputs:	
	A1-points-python-output.csv
	B2-points-python-output.csv

	TO DO:
		igore movement from all FFT data
		instead use the Manual Cell Tracking data

TO DO next:
	make some rose plots from: alb_A1_8bit-0.25 points.xlsx
		(this is what the python script used as input)

	see what I can do with these alb_A1_8bit-0.25 points.xlsx
		animated plots?
		do they move in random directions? or vertical/horizonal?
	
possible workshops:
		1. replicates and size metrics
		2. movement information 1
		3. extrapolation from lab stuff & movement information 2 (rose plots)
					
ALSO: molecular data
	secretome prpteomic analysis has been done
	and there is a manuscript on biorxiv: https://www.biorxiv.org/content/10.1101/2023.01.19.524473v1
	calibration curve data from Amanda

### 2024-01-17 Comments from Amnda about the cells & lab work

From an email

**The broad question** I'm posing to the students is whether we can use morphometric parametres (which I define as size, shape) and cells migration as predictors as stem cell function, as cell structure relates to function. I.e. are then one or two parametres (or a combination) that a typical stem cell that can become multiple cell types has which then differs in a stem cell that doesn't differentiate. This could then be useful therapeutically to characterise cells quickly without the need for longer functional assays or methods that don't currently distinguish different stem cells easily from one another.

	
**The practical sessions the students complete are:**
	1. Cell Culture and seeding the two clonal cell lines on to glass coverslips
	2. 'Fixing' the cells a few days later and staining their cytoskeleton and nuclei
	3. Imaging the cells using fluorescent microscopy
	4. Enzyme activity assay of alkaline phosphatase as a marker of differentiation (functional stem cell assay)
	This is where the students will create a calibration curve and extrapolate unknown data.

**The data analysis then complements what the students will see visually in the lab** as they can work with multiple parameters to first see how they vary and then secondly perhaps try and define for themselves a specific signature of parametres that could be the basis of identifying the same cells from a mixed population, for example making plots with parametres on different axis and overlaying the data to see if that distinguishes or not (length to width ratio vs sphericity could be a good one based on your plots). I don't know whether we could do something like PCA here to look at differences, I've never done it before so could be a ridiculous idea.


**The two clonal lines are genetically identical** as they come from the same donor, but they have different gene expression patterns, as they are functionally different and as shown from proteomics and secretome data. As far as they have characterised them functionally (stem cell differentiation) they haven't noticeably changed, similarly to HeLa cells and CHO cells that have been cultured for many years.


# 2024-01-18 consolodation into one script per workshop


### Possible workshops:
1. Common R workshop
2. Replicates and size metrics
3. Movement information 1
4. Movement information 2 (rose plots) and calibration curve


#### cell biology workshop 1. Replicates and size metrics

script:  BIO00066I-workshop2-replicates-and-size-metrics.R
workshop2-replicates-and-size-metrics.qmd

Use these scripts as starting points:

- play-with-data-2023-12-18.R
	#makes a nice summary table of the clones
	
- analysis-cloneA-v-cloneB-2024-01-12.R
	#does some filtering and shows which metrics that are different
	
- BIO00066I-workshop2.Rmd
	#good for a simple into to the data
	
- comparing-repeats.R
	#shows how repeats differ

combine all these scripts:


### getting Quarto running:
from: https://quarto.org/docs/get-started/

- installed Quarto
- follow instructios here for R Studio: https://quarto.org/docs/get-started/hello/rstudio.html 

EASY!!


#### 2024-01-19

**worked on script: BIO00066I-workshop2-replicates-and-size-metrics.R**

this is more or less complete and it:

- introduces the full feature table data
- explores cell shape/size metrics with plots and stats
- shows which  shape/size metrics are correlated (most of them!)
- cell shape metriics consistently distinguish between clones, in all reps
 
NB: 
now usingdata is filtered to remove the top end and low end of area, dry.mass and volume
as described in filtering-FFT-data-2024-01-19.R

data from [here](https://djeffares.github.io/teaching/BIO00066I/all-cell-data-FFT.filtered.2024-01-19.tsv)


#### 2024-01-20

worked on workshop 3: BIO00066I-workshop3-cell-movement-metrics.R

**exploring cell movement metrics:**
	first, in the FFTdata, showing that
	the movement metrics are **not** strongly correlated (unlike the cell shape data)
	but they do consistently distinguish between clones, in all reps

**PCA**

- I tried to separate the clones by principal componets clustering
- It was not very convincing
- See pca-trial-2024-01-20.R and arranged.pca.plot.2024.01.20.jpeg


#### 2024-01-23 workshop2 into quarto and trial of hosting:

based on this page:
https://quarto.org/docs/publishing/github-pages.html

I have the hosting working on the repo BIO66I, see: https://djeffares.github.io/BIO66I/

This is deployed from branch: gh-pages, from the folder docs/

To update
	make a new .qmd
	on terminal:
		quarto render (in the fir where the qmd's are, this puts the htmls in docs)
		git add docs/*, commit and push
	you will then need to wait a bit for the pages build and deployment in workflows on github

NOTE:

The navbar is controlled in _quarto.yml

BIO66I/data/ contains all the data in branch main

but the only data available for download is in branch  gh-pages, in dir docs/data
this is at: https://djeffares.github.io/BIO66I/data/all-cell-data-FFT.filtered.2024-01-19.tsv
	
NOTE: be careful that you are in branch gh-pages when adding, commiting and pushing

#### Moving the directory:

from /Users/dj757/gd/modules/BIO00066I
to /Users/dj757/gd/modules/BIO66I

because I got the BIO66I to be published, and its simpler to type!

#### 2024-02-15 starting on workshop 3
starting with: BIO00066I-workshop3-cell-movement-metrics.R


#### Improving the analysis with the new dates data for w2

See script: BIO00066I-new-w2-gated-data.R
obtained all data from AB
/Users/dj757/gd/modules/BIO66I/raw-data/2016-08-26_14-47-36_P*csv

### Work for workshop 4

from AB:
Yes you're right that the two files are alb_B2_8bit-0.25 tracks.xlsx and alb_A1_8bit-0.25 tracks.xlsx

The difference is the files you attached to the email. You've got the  alb_A1_8bit-0.25 points.xlsx attached to the email rather than alb_A1_8bit-0.25 tracks.xlsx. The points data is the raw data I obtained from cell tracking on imageJ. Whereas the tracks file is the output from python which analysed the point file to give metrics.


##### new names for manual point and track data:
alb_A1_8bit-0.25 points.xlsx
alb_B2_8bit-0.25 points.xlsx

alb_A1_8bit-0.25 tracks.xlsx
alb_B2_8bit-0.25 tracks.xlsx

From AB:

I've renamed the files to match each other, added labels to the columns and put the correct units in.

What I would like to do on R if we can for each clone is:
- Sort the data so we have summaries for each individual cell of the following metrics (mean speed, total distance travelled, euclidean distance (shortest distance from start to finish/as crow flies), meandering index (total distance travelled/euclidean distance)
- For A1 the measurements for the first cell I tracked (from lineage 1, cell 1) is the first 15 rows of the spreadsheet.
Cell I15 is the total distance travelled,
cell J15 is the euclidean distance
the mean speed is the mean of cells M6 - M15

### 2024-03-16
### standard curve


workind script: 

file: raw-data/Example Alkaline Phosphatase Activity assay .xlsx
added 2nd sheet that is more tidyverse





