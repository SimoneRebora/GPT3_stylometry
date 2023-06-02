# Applying Stylometry to GPT-3 texts

Scripts for the paper "GPT-3 vs. Delta. Applying stylometry to large language models", presented at the *AIUCD2023* Conference ([slides](https://docs.google.com/presentation/d/10Fq6a4HP8D6W3ouHesAccOKaBPKhTudxET0tavBODtQ/edit?usp=sharing)).

## Scripts

This repository contains two scripts, written in the Python and R programming languages:  
- **gpt3_create_novels.py** generates texts with GPT3 by using the [OpenAI API](https://openai.com/blog/openai-api) (features for text generation are read from the *gpt3_features.csv* file). You can call the script via `python gpt3_create_novels.py`. Before running it, remember to modify it at line 7, by adding your OpenAI API key.
- **Stylometric_analyses.R** performs stylometric analyses on the generated texts by using the [stylo](https://github.com/computationalstylistics/stylo) package. As the script is conceived for active exploration, I suggest running it via graphical interfaces like [RStudio](https://posit.co/download/rstudio-desktop/) or [PositCloud](https://posit.co/products/cloud/cloud/).

## Corpora

### English literature authors

The *level1* folder contains the English novel collection for the ELTeC, the European Literary Text Collection, produced by the COST Action Distant Reading for European Literary History (CA16204, https://distant-reading.net) and downloaded from: https://github.com/COST-ELTeC/ELTeC-eng.  
All texts included in this collection are in the public domain. The textual markup is provided with a Creative Commons Attribution International 4.0 licence (CC BY, https://creativecommons.org/licenses/by/4.0/).  
General information about ELTeC releases is available at https://github.com/COST-ELTeC/ELTeC.  

### GPT-3 texts

The texts created with GPT-3 will be saved in the "GPT3" folder, divided by different generation strategies (called "tries").  

## Requirements

Scripts have been tested with Python 3.9.2 and R 4.2.1.  
To install required packages, run `pip install -r requirements_python.txt` and `Rscript requirements_R.R`
