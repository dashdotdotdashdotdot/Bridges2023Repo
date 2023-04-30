# Bridges2023Repo

This repository contains the code that produces the key figures in my forthcoming paper "Drawing with Statistics", which I will be presenting at this confernce:

https://www.bridgesmathart.org/b2023/

program 1 is intended to be run line by line without using many embeded functions so what the program is doing will be clear to the user.

program 2-4 provide a workflow to enable the user to create their own library of line drawings/silhouttes.

program 2 creates a list of lists where each sublist is a specific drawing analyzed through the framework.

program 3 converts the list of lists to a single csv file for all drawings.  Allows for manual over-rides for when program 2 fails to find a pleasing ordering of the points.

program 4 produces a pdf that contains a picture of each of the drawings as produced through this framework.

The idea is for the user to clone/fork the repo, install rstudio on their machine.  Run program 1 line by line to see how it works (and overwrite drawings1.jpg through drawings4.jpg in the process).  Run programs 2-4 to overwrite allDrawings.pdf, allDrawings.RData and allDrawings26April2023.csv.
Next:
1) create your own drawing and bring it into the folder as a jpg
2) add it to the vectors examples in program2 
3) rerun programs 2-4 and the new output should incorporate the new drawing, 
5) experiment with drawings contain in the csv file.  

Please let me know how it goes.
