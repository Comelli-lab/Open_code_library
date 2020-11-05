# :sparkles: miRNA Expression Analysis :sparkles:

### Authors
* Zoë Lofft (zlofft@gmail.com)
* Paraskevi Massara (massaraevi@yahoo.com)

### *Background of Development*
To better understand the example scripts, a brief description of the project that this workflow was based around will be explained. 

**Overall goal**: Assesssing miRNA signatures in an intestinal cell line (Caco-2BBe1) inresponse to cranberry proanthocyanidin and two of its gut microbe-derived metabolites at two different concentrations, with and without inflammation stimulated by IL-1beta. Thus, the main treatment groups being analyzed in the code are:

* DMEM (the control vehicle) with and without IL-1beta
* Cranberry proanthocyanidin (PAC) with and without IL-1beta
* Metabolite 1 (MET1) with and without IL-1beta
* Metabolite 2 (MET2) with and without IL-1beta

Although this project was based on miRNA data, the workflow should be suitable for **any type of nCounter data**
Most of the steps are explained in the scripts, but this will contain some additional information in greater detail.

### Required Packages and Associated Vignettes
**Main Packages**
* [NanoStringNorm](https://cran.r-project.org/web/packages/NanoStringNorm/vignettes/NanoStringNorm_Introduction.pdf)
* [NanoStringDiff](https://www.bioconductor.org/packages/devel/bioc/vignettes/NanoStringDiff/inst/doc/NanoStringDiff.pdf)
* [Pheatmap](https://cran.r-project.org/web/packages/pheatmap/pheatmap.pdf)
* [GSOAP](https://github.com/tomastokar/gsoap)

**Required to use Main Packages**
* BiocManager (for NSNorm)
* Vsn (for NSNorm)
* Biobase (for NSDiff)
* RColorBrewer (for pheatmap, _optional_)
* Plyr (for GSOAP)
* Reshape2 (for GSOAP)
* Devtools (for GSOAP)

## 1. Normalization Using NSNorm
**File input type**: txt

The file should contain: first 3 columns (see example below), in addition to ALL samples of interest and their corresponding counts

**Example**: first 3 columns you MUST Keep in the txt file
| Code Class    | Name          | Accessation |
| ------------- |:-------------:| -----------:|
| Positive      | POS(A)_103    | nMIR1223    |
| Negative      | NEG(H)_11     | nMIR0998    |
| Housekeeping  | GAPDH         | NM_007788   |

Call the function **"NanoStringNorm"** to apply the normalization parameters of your choosing.
The arguments within the function can be modified to best suit the data you are normalizing.
As specified in the script, the following code allows you to retain only the expressed endogenous probes in a matrix, which is denoted as **final matrix**:
```diff
+Produce a matrix of genes expressed in your sample 
complete_genes <-
  data_normalized_test$gene.summary.stats.norm[which(data_normalized_test$gene.summary.stats.norm$Missing <
                                                       90), ]

keep_genes <- grep("hsa", rownames(complete_genes), value = TRUE)

gene_matrix <-NanoStringNorm(
  x = ns_norm,
  CodeCount = "geo.mean",
  Background = "mean",
  SampleContent = "top.geo.mean",
  round.values = TRUE,
  take.log = TRUE,
  return.matrix.of.endogenous.probes = T
)

final_matrix <- gene_matrix[keep_genes,]

write.csv(final_matrix, "~/Desktop/final_matrix_KEEP.csv")
```
**Note**: "data_normalized_test" represents the object you assigned the normalization parameters to in the NSNorm function

### End of Step 1 :white_check_mark:
Now you have a matrix of the log2 transformed expression counts of the genes that are actually expressed in your samples.
This matrix can be used for downstream analyses including hierarchical clustering/the creation of heatmaps. 

## 2. Differential Expression Analysis Using NSDiff
:warning: This package required a lot of computational power when running many comparisons consecutively and because of this we ran our script on a supercomputer

:clock3: Running the example code you see in the code folder (10 comparisons) took ~14 hours on a supercomputer

**File input type**: csv

### Three Important Notes _re: data input_
**Firstly**, you need to input a csv file with the same data structure as you did with the raw input for NSNorm (ie. first three columns and all samples of interest and their counts). You **CANNOT** use the normalized data that you produced from NSNorm because NSDiff requires raw input to be made into a formal class NanoStringSet object which will then have normalization parameters applied before it is used for differential expression analysis. **Ligation** and **Spikein** code classes aren't used by this package, so you can omit them in the csv. However, it is ok if they are present, they will just be ignored. 

**Secondly**, you need to include all samples you intend on using in the analysis in a singular csv file. For example, if you intend on making x pairwise comparisons of y groups, all y groups should be normalized together. Avoid making singular csv files specifically for every unique comparison because the data will be normalized differently each time, rendering unnecessarily increased variability between comparisons.   

**Thirdly**, if co-working with others, you may encounter the following error when trying to create a NanoStringSet object from a csv file:
```diff
-Error in rowSums(counts) : 'x' must be numeric
```
The issue has to do with the csv formatting. Ensure both people are using the same type of csv file (ex. normal csv, or UTF-8 csv).

### Phenotype Information
Ascribe phenotypic information to your dataset by assigning an object to a dataframe that includes the descriptions of your samples. In my case, I had the DMEM group and 3 treatments (PAC, MET1, MET2), all with and without IL-1beta (x2 of each), with 3 biological replicates of each. 

**=24 samples to annotate but only 8 unique groups**

The package recognizes how many biological replicates you have through the designs argument when you perform the analysis. So give every biological replicate the same name or they will be considered as unique samples.

### Contrast Vector
The contrast(s) you specify will dictate which groups you are comparing and the type of comparisons you are making.
**There are two important things to note about the contrast vector:** 
* :one: It must contain the same number of digits or fractions as the number of unique groups that you have _(So in my case and in the example script, 8)_
* :two: The contents of the vector must summate to 1 

:rotating_light: **Very important:** the group with the negative number will be the comparison group; ie) in example 1 the first group would be the control group (with the -1) and the second group would be the treatment group. 

_NOTE: in my code the reverse was done, so I flipped all of the fold changes after getting the results_ 

**Example 1**: a two group comparison among 5 unique groups. (Compare group 2 to group 1, the control group, which is denoted by the -)
```diff
!contrast1 <- c(-1, 1, 0, 0, 0)
```
**Example 2**: involves multiple groups in the comparison. (Compare the mean of all other groups to the control group, which is denoted by the -)
```diff
!contrast2 <- c(-1, 1/4, 1/4, 1/4, 1/4)
```
**Example 3**: compare the mean of groups 4 and 5 to the control group.
```diff
!contrast3 <- c(-1, 0, 0, 1/2, 1/2)
```
### Run Your Comparisons using the glm.LRT function
After calling this function, you must include **3 main arguments**  to indicate the following components:
* Your normalized dataset
* A design matrix that incorportates the phenotypic information 
* A contrast vector

**Save the result as a unique object, turn it into a data frame, then export it as a csv file**

### End of Step 2 :white_check_mark:
Now you have your differential expression statistics; you will now know how many miRNAs are differentially expressed between your groups of interest by using a q-value cutoff and/or fold change cutoff. 
Knowing this, you can create heatmaps in step 3 to cluster and visualize the significantly miRNAs. 

## 3. Heatmaps using pheatmap
Making heatmaps in R with pheatmap is very easy, fast, and extremely customizable!

_Note: it was not possible to extract the normalized data set from NSDiff so we had to use normalized data from NSNorm to make the heatmaps, which is not the ideal case!_

**File input type**: csv file containing the miRNAs you would like to include (first column), which will become the row names, and their normalized values; each subsequent column will be a unique sample and the first row will indicate what that sample is. See the code example below for how to input your data correctly to be used in pheatmap. 


```diff
your_heatmap_object <- read.csv("heatmap_data_file.csv", row.names = 1)
your_heatmap_object <- data.matrix(your_heatmap_object)
```

:exclamation: The object that represents your data **MUST** be a matrix to be used in pheatmap.

In general, the [RDocumentation](https://www.rdocumentation.org/packages/pheatmap/versions/1.0.12/topics/pheatmap) site for pheatmap explains how it can be customized very easily. 

<ins>Some main tips include:</ins>
* Use RColourBrewer to alter the colour schemes if you would like
* Save as a vector image (ex. PDF or svg file) to preserve quality 
* Resize the cells and text for a more consistent layout that is easy to read, epecially when you have many heatmaps with different numbers of miRNA

**Example 1** Since in the first heatmap there were only 2 miRNAs, the automatic sizing made the cells quite long, which makes it look very different from the second heatmap with 5 miRNAs. Additionally, the text is very small and hard to read.

<img width="750" alt="Screen Shot 2020-09-06 at 10 51 48 AM" src="https://user-images.githubusercontent.com/64021196/92328470-0c607a80-f02f-11ea-8956-695cdb3538fd.png">

**Example 2** These are the exact same data sets but the cells were resized to look more uniform and the text size was increased. 

<img width="712" alt="Screen Shot 2020-09-06 at 10 52 05 AM" src="https://user-images.githubusercontent.com/64021196/92328525-72e59880-f02f-11ea-80b9-ffdfd9e65e0e.png">

_Note: I did not z-score the heatmaps, but you could easily do this using the "scale" argument in the pheatmap function._

:star: **TIP** if you are making many heatmaps using the same arguments/ variable names (colour scheme, cell size, etc) you could create a function to save time and not repeat code.

### End of Step 3 :white_check_mark:

## 4. Visualize pathDIP results using GSOAP

**File input type**: txt file from pathDIP 

<ins>When you run a pathDIP search you will get the following information in a tabular format:</ins>
* Pathway source
* Pathway name
* p-value
* q-value (BH method)
* q-value (Bonferroini method) 

:exclamation: **BUT** to run GSOAP you also need a column showing all of the genes from your querry list that are annotated in the significantly enriched pathway! This is **NOT** included in the output you get from pathDIP. 

However, the creator of GSOAP, Dr. Tomas Tokar, provided us with the function "read.pathdip.result" that can be used with the raw pathDIP output file to obtain this and easily prepare the raw data for use in the package. 

By reading the [GSOAP publication](https://academic.oup.com/bioinformatics/article/36/9/2923/5715574) or [github page](https://github.com/tomastokar/gsoap), you can see the many ways in which the plots can be customized to your liking. 

In the example code of my plots (two shown below) I visualized the **top 100** significantly enriched pathways (using BH-adjusted q-value), labelled the **top 5** enriched pathways on the plot, used **colour** to denote significance, and **circle size** to denote number of querry genes in the enriched pathway: 

<img width="724" alt="Screen Shot 2020-09-06 at 11 36 30 AM" src="https://user-images.githubusercontent.com/64021196/92329401-43d22580-f035-11ea-8c77-e87869abe3e5.png">

_Re: file export- I saved the files as pdfs to preserve quality. However, on my mac they looked fine but on a PC they look different! So, I would suggest still saving as a pdf and then converting it to a high-quality jpeg after._

### End of Step 4 :white_check_mark:

## Extra: Aesthetic, customizable plots using ggplot2
**The example code is very easy to follow!** :+1:

**File input type**: csv file with your data 

_Double bar graphs are shown in this code, but with simple adjustments you could also do single bar graphs, line graphs, histograms, boxplots, etc. depending on the nature of your data and how you would like to visualize it._

**Context of the data**: I had smaller volumes of data in these cases so I simply created data frames, but if you had larger amounts of data you could just use csv or txt files. In these graphs I was analyzing IL-6 and IL-8 protein in cell supernatant in response to the treatments alone (PAC/MET1/MET2) or with IL-1beta. The factors I was looking at were combinations of treatment, dose, and inflammation (to give you an idea of what the variable names are and why).

<ins>Packages</ins>
* [ggplot2](https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf)
* [ggsignif](https://cran.r-project.org/web/packages/ggsignif/vignettes/intro.html)  
* [ggarrange](https://rdrr.io/cran/egg/man/ggarrange.html) 

:heavy_check_mark: Customize your **colours** using [hexadecimal codes](http://www.sthda.com/english/wiki/colors-in-r)
:heavy_check_mark: In order to get **error** bars, use the object created from applying the data summary function to your original data set
:heavy_check_mark: **Annotate** your plots (can add dashed lines, stars, letters, text, etc.) after creating the plots, or if your stats are more simple and you want to annotate everything that is significant, consider using ggsignif instead
:heavy_check_mark: **Customize** aspects such as bar width, titles, subtitles, axis titles, font size, legend appearance and more to your liking very easily in the code

In general, the [RDocumentation](https://www.rdocumentation.org/packages/ggplot2/versions/3.3.2) page for ggplot2 is a good resource to see how the arguments in the ggplot function can be changed to customize your graphs. 

_If you would like, you can panel your graphs using ggarrange._

**Example**

![plotsA_B](https://user-images.githubusercontent.com/64021196/92329887-847f6e00-f038-11ea-908a-2b31fc04dd88.png)

:bulb:Need more inspiration? Check out the [R Graph Gallery](https://www.r-graph-gallery.com/) to see the endless possibilities of creative ways to visualize your data!

