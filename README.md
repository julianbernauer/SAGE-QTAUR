# SAGE-QTAUR

This repository accompanies the book "Quantitative Text Analysis Using R" by Julian Bernauer and Anna Wohlmann published by SAGE in March 2025.

It contains: 

- Replication code for the chapters 
- Data sets used in the book 
- Answers to the questions in the book
- Solution code for the exercises in the book 
- Additional resources including slides 

The book is mainly build on R code, but also contains some Python. 

Please feel free to contact us for feedback or questions or if you find any errors in the code. We are happy to consider any suggestions for improvement; simply submit a pull request. 

##  Accessing R files 
You can learn more about Github in Chapter 1 of the book. Here, we want to quickly explain how to get the code from GitHub into an R project on your computer:
1) download git onto your computer: https://git-scm.com/
2) in R, navigate to git under Tools > Global Options and check the version control box
3) add the directory where you just installed Git in the Git executable field
4) on the GitHub landing page of this repository, click on the green "code" button
5) copy the URL under HTTPS. Alternatively, you can create an SSH key for RStudio and use that URL
6) go to File > New Project > Version Control > Git, and in the Repository URL field, input the link you copied
7) through "browse" determine where our code should be stored on your local machine
8) click ‘Create Project’

## Read RMarkdown files
Some of the exercise files are written as RMarkdown files. When you look at those files in code,
you will see all the R code inside so-called chunks, while text is written around them.
To see the intended form of such files, you need the knitr package. When knitr
is installed, you have a knit button below the menu bar, fittingly with a ball of wool
and knitting needles. Through that button, you can create a Word, PDF or HTML
document out of our files.

## Need help?
If you have questions or the solutions of our exercises aren't working for you, you can ask the authors and other readers questions in the issue tab.
Make sure to first use the tools we gave you in the book to figure out the issue, e.g., check stack overflow. Then, ask the question as precisely as possible.
